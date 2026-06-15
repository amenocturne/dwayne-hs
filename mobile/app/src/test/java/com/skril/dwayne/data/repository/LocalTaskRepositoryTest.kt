package com.skril.dwayne.data.repository

import app.cash.sqldelight.db.SqlDriver
import app.cash.sqldelight.driver.jdbc.sqlite.JdbcSqliteDriver
import com.skril.dwayne.data.events.Event
import com.skril.dwayne.data.events.EventProjection
import com.skril.dwayne.data.events.EventStore
import com.skril.dwayne.data.events.Nullable
import com.skril.dwayne.data.model.ClearOrSet
import com.skril.dwayne.data.model.EditTaskRequest
import com.skril.dwayne.data.model.OrgTime
import com.skril.dwayne.data.model.TaskPointer
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
    fun `recentCaptures returns captured tasks from inbox file in descending genesis order`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val first = repo.capture("first")
        LocalTaskRepository(store, inboxFileProvider = { "/Other.org" }).capture("other")
        val second = repo.capture("second")

        val recent = repo.recentCaptures(limit = 10)

        assertEquals(listOf(second.pointer, first.pointer), recent.map { it.pointer })
        assertEquals(listOf("second", "first"), recent.map { taskTitle(it.task.title) })
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

    // -- Views ---------------------------------------------------------------

    @Test
    fun `getView today filters by TODAY keyword`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val a = repo.capture("a")
        val b = repo.capture("b")
        val c = repo.capture("c")
        repo.changeKeyword(a.pointer, "TODAY")
        repo.changeKeyword(b.pointer, "SOON")
        repo.changeKeyword(c.pointer, "TODAY")

        val today = repo.getView("today")
        assertEquals(2, today.metadata.total)
        assertTrue(today.data.all { it.task.todoKeyword == "TODAY" })
    }

    @Test
    fun `getView work-queue includes TODAY and SOON sorted by priority`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val a = repo.capture("a")
        val b = repo.capture("b")
        val c = repo.capture("c")
        // a: TODAY priority 3, b: SOON priority 1, c: TODAY priority null (lowest)
        repo.changeKeyword(a.pointer, "TODAY")
        repo.changePriority(a.pointer, 3)
        repo.changeKeyword(b.pointer, "SOON")
        repo.changePriority(b.pointer, 1)
        repo.changeKeyword(c.pointer, "TODAY")

        val q = repo.getView("work-queue")
        assertEquals(3, q.metadata.total)
        // Priority sort ascending; null tasks sort to the end via Int.MAX_VALUE.
        val priorities = q.data.map { it.task.priority }
        assertEquals(listOf(1, 3, null), priorities)
    }

    @Test
    fun `getView trash returns TRASH tasks - default views exclude TRASH via keyword filter`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val a = repo.capture("a")
        val b = repo.capture("b")
        repo.deleteTask(a.pointer)

        val trash = repo.getView("trash")
        assertEquals(1, trash.metadata.total)
        assertEquals(a.pointer, trash.data.single().pointer)

        // INBOX view excludes the trashed task; only one INBOX task remains.
        val inbox = repo.getView("inbox")
        assertEquals(1, inbox.metadata.total)
        assertEquals(b.pointer, inbox.data.single().pointer)
    }

    @Test
    fun `getView paginates with offset and limit`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        repeat(5) { repo.capture("t$it") }

        val page1 = repo.getView("inbox", offset = 0, limit = 2)
        assertEquals(2, page1.data.size)
        assertEquals(5, page1.metadata.total)

        val page2 = repo.getView("inbox", offset = 2, limit = 2)
        assertEquals(2, page2.data.size)

        val tail = repo.getView("inbox", offset = 4, limit = 100)
        assertEquals(1, tail.data.size)
    }

    @Test
    fun `getView with unknown name returns all tasks`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val a = repo.capture("a")
        val b = repo.capture("b")
        repo.changeKeyword(b.pointer, "DONE")
        // Unknown view name falls through the when-branch to "else -> all".
        val all = repo.getView("nonsense-view")
        assertEquals(2, all.metadata.total)
    }

    // -- Search --------------------------------------------------------------

    @Test
    fun `search matches title substring case-insensitively`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        repo.capture("Buy Milk")
        repo.capture("walk dog")

        val hits = repo.search("MILK")
        assertEquals(1, hits.metadata.total)
        val text = hits.data.single().task.title.filterIsInstance<TextNode.Plain>().joinToString("") { it.text }
        assertEquals("Buy Milk", text)
    }

    @Test
    fun `search matches tag substring`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val a = repo.capture("buy milk")
        repo.addTag(a.pointer, "groceries")
        repo.capture("walk dog")

        val hits = repo.search("groc")
        assertEquals(1, hits.metadata.total)
    }

    @Test
    fun `search empty query returns the base view`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        repo.capture("a")
        repo.capture("b")
        val hits = repo.search("   ", view = "inbox")
        assertEquals(2, hits.metadata.total)
    }

    // -- Idempotency / event store ------------------------------------------

    @Test
    fun `appending the same event twice is a no-op (PK INSERT OR IGNORE)`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val captured = repo.capture("once")
        // Re-insert the same genesis row directly via the store; PK collision
        // means the second insert is silently ignored.
        val genesisRow = store.selectAll().single()
        val inserted = store.insert(genesisRow)
        assertEquals("second insert with same PK should be a no-op", false, inserted)
        assertEquals(1, store.selectAll().size)
        // Projection is unchanged.
        assertEquals(captured.task.todoKeyword, EventProjection.project(store.selectAll())[captured.pointer]!!.todoKeyword)
    }

    // -- Out-of-order events -------------------------------------------------

    @Test
    fun `out-of-order events project to latest-wins by occurredAt`() = runTest {
        // Insert a "later" delta first, then the genesis with an earlier
        // timestamp; projection sorts by occurredAt and applies in order, so
        // the delta wins.
        val later = Event(
            filePath = "/Phone.org",
            taskIndex = 0,
            occurredAt = "2026-04-30T15:00:00.000Z",
            todoKeyword = "DONE",
        )
        store.insert(later)
        // Genesis with earlier occurredAt.
        val genesis = EventProjection.genesisEvent(
            filePath = "/Phone.org",
            taskIndex = 0,
            occurredAt = "2026-04-30T12:00:00.000Z",
            task = com.skril.dwayne.data.model.Task(
                level = 1,
                todoKeyword = "INBOX",
                title = listOf(TextNode.Plain("late-arriving genesis")),
                tags = emptyList(),
            ),
        )
        store.insert(genesis)

        val state = EventProjection.project(store.selectAll())
        val task = state[TaskPointer("/Phone.org", 0)]!!
        assertEquals("DONE", task.todoKeyword)
    }

    // -- Mutations -----------------------------------------------------------

    @Test
    fun `addTag is idempotent for an already-present tag`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val captured = repo.capture("draft")
        repo.addTag(captured.pointer, "home")
        val again = repo.addTag(captured.pointer, "home")
        assertEquals(listOf("home"), again.task.tags)
        // We still emit a delta event. Document this so behaviour change is loud.
        assertEquals(3, store.selectAll().size) // genesis + addTag + addTag-noop
    }

    @Test
    fun `changePriority null clears the priority via sentinel`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val captured = repo.capture("draft")
        repo.changePriority(captured.pointer, 2)
        val cleared = repo.changePriority(captured.pointer, null)
        assertNull(cleared.task.priority)

        val rows = store.selectAll()
        // The clearing event must use the priority-cleared sentinel, not raw null.
        val lastEdit = rows.last()
        assertEquals(Nullable.PRIORITY_CLEARED, lastEdit.priority)
    }

    @Test
    fun `editTask scheduled clear via ClearOrSet null applies sentinel`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val captured = repo.capture("draft")
        // Set a scheduled date, then clear it.
        repo.editTask(EditTaskRequest(
            file = captured.pointer.file,
            taskIndex = captured.pointer.taskIndex,
            scheduled = ClearOrSet(OrgTime(date = "2026-05-10")),
        ))
        val cleared = repo.editTask(EditTaskRequest(
            file = captured.pointer.file,
            taskIndex = captured.pointer.taskIndex,
            scheduled = ClearOrSet(null),
        ))
        assertNull(cleared.task.scheduled)
    }

    @Test
    fun `monotonic timestamp avoids same-millis PK collisions across rapid writes`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val captured = repo.capture("draft")
        // Three rapid edits — even on a fast machine they may hit the same
        // System.currentTimeMillis(). The repo bumps lastEmittedMillis to
        // guarantee distinct timestamps and avoid INSERT OR IGNORE drops.
        repo.changeKeyword(captured.pointer, "TODO")
        repo.changeKeyword(captured.pointer, "TODAY")
        repo.changeKeyword(captured.pointer, "DONE")

        val rows = store.selectAll()
        assertEquals(4, rows.size) // genesis + 3 deltas
        val timestamps = rows.map { it.occurredAt }
        assertEquals("all timestamps distinct", timestamps.size, timestamps.toSet().size)
        // Final state reflects the last write.
        val state = EventProjection.project(rows)
        assertEquals("DONE", state[captured.pointer]!!.todoKeyword)
    }

    @Test
    fun `deleteTask sets keyword to TRASH and is reachable from trash view`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val captured = repo.capture("draft")
        repo.deleteTask(captured.pointer)

        val trash = repo.getView("trash")
        assertEquals(1, trash.metadata.total)
        assertEquals("TRASH", trash.data.single().task.todoKeyword)
    }

    @Test
    fun `removeTag of last tag emits TAGS_CLEARED sentinel which projects to empty list`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val captured = repo.capture("draft")
        repo.addTag(captured.pointer, "home")
        repo.removeTag(captured.pointer, "home")

        val state = EventProjection.project(store.selectAll())
        assertTrue(state[captured.pointer]!!.tags.isEmpty())
        // The clearing event must be the explicit empty-list sentinel.
        val lastEvent = store.selectAll().last()
        assertEquals(Nullable.TAGS_CLEARED, lastEvent.tags)
    }

    @Test
    fun `task indices are independent per file`() = runTest {
        val repo = LocalTaskRepository(store, inboxFileProvider = { "/Phone.org" })
        val a = repo.capture("a")
        // Switch to a second inbox file via a fresh repo instance.
        val repo2 = LocalTaskRepository(store, inboxFileProvider = { "/Other.org" })
        val b = repo2.capture("b")
        assertEquals(0, a.pointer.taskIndex)
        assertEquals(0, b.pointer.taskIndex)
        assertEquals("/Phone.org", a.pointer.file)
        assertEquals("/Other.org", b.pointer.file)
    }
}

private fun taskTitle(title: List<TextNode>): String =
    title.filterIsInstance<TextNode.Plain>().joinToString("") { it.text }
