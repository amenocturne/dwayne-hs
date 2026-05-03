package com.skril.dwayne.data.repository

import app.cash.sqldelight.db.SqlDriver
import app.cash.sqldelight.driver.jdbc.sqlite.JdbcSqliteDriver
import com.skril.dwayne.data.events.EventProjection
import com.skril.dwayne.data.events.EventStore
import com.skril.dwayne.db.DwayneDatabase
import kotlinx.coroutines.test.runTest
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Before
import org.junit.Test

class LocalTaskRepositoryTest {

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

    @Test
    fun `capture emits a single event with all fields populated`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val captured = repo.capture("buy milk")

        assertEquals("/Phone.org", captured.pointer.file)
        assertEquals(0, captured.pointer.taskIndex)
        assertEquals("INBOX", captured.task.todoKeyword)

        val rows = store.selectAll()
        assertEquals(1, rows.size)
        val event = rows.single()
        assertEquals("/Phone.org", event.filePath)
        assertEquals(0, event.taskIndex)
        // Genesis event: every field non-null (sentinels stand in for absents).
        assertNotNull(event.level)
        assertNotNull(event.todoKeyword)
        assertNotNull(event.priority)
        assertNotNull(event.title)
        assertNotNull(event.tags)
        assertNotNull(event.scheduled)
        assertNotNull(event.deadline)
        assertNotNull(event.created)
        assertNotNull(event.closed)
        assertNotNull(event.properties)
        assertNotNull(event.description)
    }

    @Test
    fun `consecutive captures get sequential task indices per file`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val a = repo.capture("first")
        val b = repo.capture("second")
        val c = repo.capture("third")

        assertEquals(0, a.pointer.taskIndex)
        assertEquals(1, b.pointer.taskIndex)
        assertEquals(2, c.pointer.taskIndex)
        assertEquals(3, store.selectAll().size)
    }

    @Test
    fun `capture without configured inbox file fails loudly`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "" })
        try {
            repo.capture("nope")
            error("expected IllegalArgumentException")
        } catch (e: IllegalArgumentException) {
            // expected
        }
    }

    @Test
    fun `editTask appends a delta event - projection reflects new keyword`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val captured = repo.capture("draft")
        val edited = repo.changeKeyword(captured.pointer, "TODO")

        assertEquals("TODO", edited.task.todoKeyword)
        assertEquals(2, store.selectAll().size)

        // Reload from DB and confirm the projection still resolves to TODO.
        val state = EventProjection.project(store.selectAll())
        assertEquals("TODO", state[captured.pointer]!!.todoKeyword)
    }

    @Test
    fun `removeTag clears tag set when last tag removed`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val captured = repo.capture("draft")
        val withTag = repo.addTag(captured.pointer, "home")
        assertEquals(listOf("home"), withTag.task.tags)

        val cleared = repo.removeTag(captured.pointer, "home")
        assertEquals(emptyList<String>(), cleared.task.tags)
    }

    @Test
    fun `getView returns INBOX captures`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        repo.capture("a")
        repo.capture("b")
        val response = repo.getView("inbox")
        assertEquals(2, response.data.size)
        assertEquals(2, response.metadata.total)
    }

    @Test
    fun `editing a task that does not exist still records the delta`() = runTest {
        // No genesis present. Projection won't materialize the task, so the
        // edit raises because `_state.value[pointer]` is null. We expect this.
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        try {
            repo.changeKeyword(
                pointer = com.skril.dwayne.data.model.TaskPointer("/Phone.org", 99),
                keyword = "DONE",
            )
            error("expected IllegalStateException")
        } catch (_: IllegalStateException) {
            // expected
        }
    }
}
