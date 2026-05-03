package com.skril.dwayne.data.sync

import app.cash.sqldelight.db.SqlDriver
import app.cash.sqldelight.driver.jdbc.sqlite.JdbcSqliteDriver
import com.skril.dwayne.data.events.Event
import com.skril.dwayne.data.events.EventProjection
import com.skril.dwayne.data.events.EventStore
import com.skril.dwayne.data.events.EventsResponse
import com.skril.dwayne.data.events.PostEventsResponse
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TextNode
import com.skril.dwayne.db.DwayneDatabase
import kotlinx.coroutines.test.runTest
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import java.util.TimeZone

class SyncEngineTest {

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

    /** Records every call so tests can assert on inputs. */
    private class FakeTransport(
        var pullResponse: () -> EventsResponse = {
            EventsResponse(events = emptyList(), serverNow = "2026-04-30T12:00:00Z")
        },
        var pushResponse: (List<Event>) -> PostEventsResponse = { evts ->
            PostEventsResponse(accepted = evts.size, serverNow = "2026-04-30T12:00:00Z")
        },
        var pullThrows: Throwable? = null,
        var pushThrows: Throwable? = null,
    ) : SyncTransport {
        val pullCalls = mutableListOf<String?>()
        val pushCalls = mutableListOf<List<Event>>()

        override suspend fun pull(since: String?): EventsResponse {
            pullCalls += since
            pullThrows?.let { throw it }
            return pullResponse()
        }

        override suspend fun push(events: List<Event>): PostEventsResponse {
            pushCalls += events
            pushThrows?.let { throw it }
            return pushResponse(events)
        }

        override fun close() {}
    }

    private fun genesis(
        file: String = "/Phone.org",
        idx: Int = 0,
        occurredAt: String = "2026-04-30T12:00:00.000Z",
    ): Event = EventProjection.genesisEvent(
        filePath = file,
        taskIndex = idx,
        occurredAt = occurredAt,
        task = Task(
            level = 1,
            todoKeyword = "INBOX",
            title = listOf(TextNode.Plain("t")),
            tags = emptyList(),
        ),
    )

    private fun engine(transport: SyncTransport, owned: List<String> = listOf("/Phone.org"), windowHours: Int = 24) =
        SyncEngine(store = store, api = transport, ownedFiles = owned, pullWindowHours = windowHours)

    // -- Pull ----------------------------------------------------------------

    @Test
    fun `pull writes received events to the local store`() = runTest {
        val incoming = listOf(
            genesis(idx = 0, occurredAt = "2026-04-30T11:00:00.000Z"),
            genesis(idx = 1, occurredAt = "2026-04-30T11:01:00.000Z"),
        )
        val transport = FakeTransport(
            pullResponse = { EventsResponse(events = incoming, serverNow = "2026-04-30T12:00:00Z") }
        )
        val result = engine(transport).runOnce(initialFullPull = true)

        assertEquals(2, result.pulled)
        assertEquals(2L, store.count())
    }

    @Test
    fun `pull advances last_pulled_at on success`() = runTest {
        val transport = FakeTransport(
            pullResponse = { EventsResponse(events = emptyList(), serverNow = "2026-04-30T13:30:00Z") }
        )
        engine(transport).runOnce()
        assertEquals("2026-04-30T13:30:00Z", store.getSyncState(EventStore.KEY_LAST_PULLED_AT))
    }

    @Test
    fun `subsequent pull requests since-window from last_pulled_at minus window`() = runTest {
        // Seed: prior pull set last_pulled_at to 12:00:00.
        store.putSyncState(EventStore.KEY_LAST_PULLED_AT, "2026-04-30T12:00:00Z")

        val transport = FakeTransport()
        engine(transport, windowHours = 24).runOnce()

        // Window shifts back 24h. Should request since = day before.
        val sinceParam = transport.pullCalls.single()
        assertNotNull(sinceParam)
        // Format is the device's millis-precision format.
        assertEquals("2026-04-29T12:00:00.000Z", sinceParam)
    }

    @Test
    fun `pull window-shift handles millis-precision bookmark (regression)`() = runTest {
        // Repro for the bug where shiftIso silently failed to parse millis
        // timestamps and returned the input unshifted, defeating the window.
        store.putSyncState(EventStore.KEY_LAST_PULLED_AT, "2026-04-30T12:00:00.000Z")

        val transport = FakeTransport()
        engine(transport, windowHours = 1).runOnce()

        val since = transport.pullCalls.single()!!
        // Must have shifted back exactly 1 hour, not returned the bookmark.
        assertEquals("2026-04-30T11:00:00.000Z", since)
    }

    @Test
    fun `initialFullPull sends since=null`() = runTest {
        val transport = FakeTransport()
        engine(transport).runOnce(initialFullPull = true)
        assertNull(transport.pullCalls.single())
    }

    @Test
    fun `pull failure does not advance bookmarks and propagates`() = runTest {
        val boom = RuntimeException("network down")
        val transport = FakeTransport(pullThrows = boom)
        var caught: Throwable? = null
        try {
            engine(transport).runOnce()
        } catch (t: Throwable) {
            caught = t
        }
        assertEquals(boom, caught)
        assertNull("last_pulled_at must not advance", store.getSyncState(EventStore.KEY_LAST_PULLED_AT))
        assertNull("last_pushed_at must not advance", store.getSyncState(EventStore.KEY_LAST_PUSHED_AT))
        // Push must not be called when pull threw.
        assertTrue(transport.pushCalls.isEmpty())
    }

    @Test
    fun `pull is idempotent — duplicate events are dropped on insert`() = runTest {
        val e = genesis(idx = 0, occurredAt = "2026-04-30T11:00:00.000Z")
        store.insert(e)
        val transport = FakeTransport(
            pullResponse = { EventsResponse(events = listOf(e), serverNow = "2026-04-30T12:00:00Z") }
        )
        val result = engine(transport).runOnce(initialFullPull = true)
        assertEquals("0 newly inserted (PK already present)", 0, result.pulled)
        assertEquals(1L, store.count())
    }

    // -- Push ----------------------------------------------------------------

    @Test
    fun `push reads owned events newer than last_pushed_at and POSTs them`() = runTest {
        val owned = genesis(file = "/Phone.org", idx = 0, occurredAt = "2026-04-30T12:00:00.000Z")
        val foreign = genesis(file = "/Other.org", idx = 0, occurredAt = "2026-04-30T12:00:01.000Z")
        store.insert(owned)
        store.insert(foreign)

        val transport = FakeTransport()
        val result = engine(transport, owned = listOf("/Phone.org")).runOnce()

        assertEquals(1, result.pushed)
        val pushed = transport.pushCalls.single()
        assertEquals(1, pushed.size)
        assertEquals("/Phone.org", pushed.single().filePath)
    }

    @Test
    fun `push advances last_pushed_at to max occurredAt of pushed batch`() = runTest {
        store.insert(genesis(idx = 0, occurredAt = "2026-04-30T12:00:00.000Z"))
        store.insert(genesis(idx = 1, occurredAt = "2026-04-30T12:00:05.000Z"))
        store.insert(genesis(idx = 2, occurredAt = "2026-04-30T12:00:01.000Z"))

        val transport = FakeTransport()
        engine(transport).runOnce()

        assertEquals("2026-04-30T12:00:05.000Z", store.getSyncState(EventStore.KEY_LAST_PUSHED_AT))
    }

    @Test
    fun `push skips events from non-owned files entirely`() = runTest {
        // Both events come from a peer's file. Nothing to push.
        store.insert(genesis(file = "/Peer.org", idx = 0, occurredAt = "2026-04-30T12:00:00.000Z"))
        store.insert(genesis(file = "/Peer.org", idx = 1, occurredAt = "2026-04-30T12:00:01.000Z"))

        val transport = FakeTransport()
        val result = engine(transport, owned = listOf("/Phone.org")).runOnce()
        assertEquals(0, result.pushed)
        assertTrue(transport.pushCalls.isEmpty())
        assertNull(store.getSyncState(EventStore.KEY_LAST_PUSHED_AT))
    }

    @Test
    fun `push with an empty owned-events set is a no-op`() = runTest {
        val transport = FakeTransport()
        val result = engine(transport).runOnce()
        assertEquals(0, result.pushed)
        assertTrue(transport.pushCalls.isEmpty())
    }

    @Test
    fun `subsequent push only includes events newer than last_pushed_at`() = runTest {
        // First batch.
        store.insert(genesis(idx = 0, occurredAt = "2026-04-30T12:00:00.000Z"))
        val transport = FakeTransport()
        engine(transport).runOnce()
        // Second batch: one new owned event after the bookmark.
        store.insert(genesis(idx = 1, occurredAt = "2026-04-30T12:00:01.000Z"))
        engine(transport).runOnce()

        // Two push calls; second contained only the new event.
        assertEquals(2, transport.pushCalls.size)
        assertEquals(1, transport.pushCalls[0].size)
        assertEquals(1, transport.pushCalls[1].size)
        assertEquals(1, transport.pushCalls[1].single().taskIndex)
    }

    @Test
    fun `push failure does not advance last_pushed_at and propagates`() = runTest {
        store.insert(genesis(idx = 0, occurredAt = "2026-04-30T12:00:00.000Z"))
        val transport = FakeTransport(pushThrows = RuntimeException("server 500"))

        var caught: Throwable? = null
        try {
            engine(transport).runOnce()
        } catch (t: Throwable) {
            caught = t
        }
        assertNotNull(caught)
        assertNull(store.getSyncState(EventStore.KEY_LAST_PUSHED_AT))
        // Pull bookmark may have advanced (pull succeeded first), but push didn't.
        assertNotNull(store.getSyncState(EventStore.KEY_LAST_PULLED_AT))
    }

    // -- Combined ------------------------------------------------------------

    @Test
    fun `runOnce executes pull then push and returns counts`() = runTest {
        val incoming = listOf(genesis(file = "/Peer.org", idx = 0, occurredAt = "2026-04-30T11:00:00.000Z"))
        val transport = FakeTransport(
            pullResponse = { EventsResponse(events = incoming, serverNow = "2026-04-30T12:00:00Z") }
        )
        store.insert(genesis(file = "/Phone.org", idx = 0, occurredAt = "2026-04-30T11:30:00.000Z"))

        val result = engine(transport, owned = listOf("/Phone.org")).runOnce(initialFullPull = true)
        assertEquals(1, result.pulled)
        assertEquals(1, result.pushed)
    }
}
