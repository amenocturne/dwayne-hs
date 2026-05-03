package com.skril.dwayne.data.events

import app.cash.sqldelight.db.SqlDriver
import app.cash.sqldelight.driver.jdbc.sqlite.JdbcSqliteDriver
import com.skril.dwayne.data.model.OrgTime
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TextNode
import com.skril.dwayne.db.DwayneDatabase
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * Direct tests against [EventStore] — the SQLDelight adapter. These are
 * separate from [LocalTaskRepositoryTest] because the repo wraps the store;
 * here we exercise the store's own contract: idempotency, batched inserts,
 * sync-state KV, and JSON column round-trips.
 */
class EventStoreTest {

    private lateinit var driver: SqlDriver
    private lateinit var db: DwayneDatabase
    private lateinit var store: EventStore

    @Before
    fun setUp() {
        driver = JdbcSqliteDriver(JdbcSqliteDriver.IN_MEMORY)
        DwayneDatabase.Schema.create(driver)
        db = DwayneDatabase(driver)
        store = EventStore(db)
    }

    @After
    fun tearDown() {
        driver.close()
    }

    private fun genesis(
        file: String = "/Phone.org",
        idx: Int = 0,
        occurredAt: String = "2026-04-30T12:00:00.000Z",
        keyword: String = "INBOX",
    ): Event = EventProjection.genesisEvent(
        filePath = file,
        taskIndex = idx,
        occurredAt = occurredAt,
        task = Task(
            level = 1,
            todoKeyword = keyword,
            title = listOf(TextNode.Plain("t")),
            tags = emptyList(),
        ),
    )

    @Test
    fun `insert returns true on first write and false on PK collision`() {
        val e = genesis()
        assertTrue("first insert should succeed", store.insert(e))
        assertFalse("second insert with identical PK should be ignored", store.insert(e))
        assertEquals(1L, store.count())
    }

    @Test
    fun `insertAll batches writes in a single transaction and returns count of new rows`() {
        val a = genesis(idx = 0, occurredAt = "2026-04-30T12:00:00.000Z")
        val b = genesis(idx = 1, occurredAt = "2026-04-30T12:00:01.000Z")
        val c = genesis(idx = 2, occurredAt = "2026-04-30T12:00:02.000Z")
        val written = store.insertAll(listOf(a, b, c))
        assertEquals(3, written)
        assertEquals(3L, store.count())
    }

    @Test
    fun `insertAll de-duplicates against existing rows via INSERT OR IGNORE`() {
        val a = genesis(idx = 0, occurredAt = "2026-04-30T12:00:00.000Z")
        store.insert(a)
        val b = genesis(idx = 1, occurredAt = "2026-04-30T12:00:01.000Z")
        // Re-include `a`. The store should only count `b` as newly written.
        val written = store.insertAll(listOf(a, b))
        assertEquals(1, written)
        assertEquals(2L, store.count())
    }

    @Test
    fun `insertAll empty list short-circuits without a transaction`() {
        val written = store.insertAll(emptyList())
        assertEquals(0, written)
        assertEquals(0L, store.count())
    }

    @Test
    fun `selectAfter returns only rows strictly newer than the bookmark`() {
        store.insert(genesis(idx = 0, occurredAt = "2026-04-30T12:00:00.000Z"))
        store.insert(genesis(idx = 1, occurredAt = "2026-04-30T12:00:01.000Z"))
        store.insert(genesis(idx = 2, occurredAt = "2026-04-30T12:00:02.000Z"))

        val after = store.selectAfter("2026-04-30T12:00:01.000Z")
        // Strict `>`: the bookmark row itself is excluded.
        assertEquals(1, after.size)
        assertEquals(2, after.single().taskIndex)
    }

    @Test
    fun `maxTaskIndexForFile returns the highest index for that file only`() {
        store.insert(genesis(file = "/A.org", idx = 0, occurredAt = "2026-04-30T12:00:00.000Z"))
        store.insert(genesis(file = "/A.org", idx = 7, occurredAt = "2026-04-30T12:00:01.000Z"))
        store.insert(genesis(file = "/B.org", idx = 1, occurredAt = "2026-04-30T12:00:02.000Z"))
        assertEquals(7, store.maxTaskIndexForFile("/A.org"))
        assertEquals(1, store.maxTaskIndexForFile("/B.org"))
        assertNull(store.maxTaskIndexForFile("/C.org"))
    }

    @Test
    fun `JSON columns round-trip - title tags scheduled properties description`() {
        val rich = listOf(TextNode.Plain("hello "), TextNode.Link("https://x", "x"))
        val e = Event(
            filePath = "/Phone.org",
            taskIndex = 0,
            occurredAt = "2026-04-30T12:00:00.000Z",
            level = 2,
            todoKeyword = "TODO",
            priority = 3,
            title = rich,
            tags = listOf("home", "errand"),
            scheduled = OrgTime(date = "2026-05-01", time = "10:00"),
            properties = listOf(listOf("a", "1"), listOf("b", "2")),
            description = listOf(TextNode.Plain("body")),
        )
        store.insert(e)
        val back = store.selectAll().single()
        assertEquals(e.title, back.title)
        assertEquals(e.tags, back.tags)
        assertEquals(e.scheduled, back.scheduled)
        assertEquals(e.properties, back.properties)
        assertEquals(e.description, back.description)
        assertEquals(2, back.level)
        assertEquals(3, back.priority)
    }

    @Test
    fun `selectAll returns events ordered by occurredAt ASC`() {
        store.insert(genesis(idx = 0, occurredAt = "2026-04-30T12:00:02.000Z"))
        store.insert(genesis(idx = 1, occurredAt = "2026-04-30T12:00:00.000Z"))
        store.insert(genesis(idx = 2, occurredAt = "2026-04-30T12:00:01.000Z"))

        val all = store.selectAll()
        assertEquals(3, all.size)
        assertEquals(listOf(
            "2026-04-30T12:00:00.000Z",
            "2026-04-30T12:00:01.000Z",
            "2026-04-30T12:00:02.000Z",
        ), all.map { it.occurredAt })
    }

    // -- sync_state KV -------------------------------------------------------

    @Test
    fun `sync_state put then get round-trips a value`() {
        store.putSyncState(EventStore.KEY_LAST_PULLED_AT, "2026-04-30T12:00:00Z")
        assertEquals("2026-04-30T12:00:00Z", store.getSyncState(EventStore.KEY_LAST_PULLED_AT))
    }

    @Test
    fun `sync_state put overwrites the prior value (REPLACE semantics)`() {
        store.putSyncState(EventStore.KEY_LAST_PULLED_AT, "v1")
        store.putSyncState(EventStore.KEY_LAST_PULLED_AT, "v2")
        assertEquals("v2", store.getSyncState(EventStore.KEY_LAST_PULLED_AT))
    }

    @Test
    fun `sync_state get returns null for an unknown key`() {
        assertNull(store.getSyncState("nonexistent-bookmark"))
    }
}
