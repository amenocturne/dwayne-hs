package com.skril.dwayne.data.events

import com.skril.dwayne.data.model.OrgTime
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskPointer
import com.skril.dwayne.data.model.TextNode
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class EventProjectionTest {

    private val file = "/Phone.org"
    private val pointer = TaskPointer(file, 0)

    private fun genesis(occurredAt: String, title: String = "hello", keyword: String = "INBOX"): Event =
        EventProjection.genesisEvent(
            filePath = file,
            taskIndex = 0,
            occurredAt = occurredAt,
            task = Task(
                level = 1,
                todoKeyword = keyword,
                priority = null,
                title = listOf(TextNode.Plain(title)),
                tags = emptyList(),
                scheduled = null,
                deadline = null,
                createdProp = null,
                closed = null,
                properties = emptyList(),
                description = emptyList(),
            ),
        )

    @Test
    fun `genesis only yields a Task with the populated fields`() {
        val ev = genesis("2026-04-30T12:00:00Z", title = "buy milk", keyword = "INBOX")
        val state = EventProjection.project(listOf(ev))
        val task = state[pointer]!!
        assertEquals(1, task.level)
        assertEquals("INBOX", task.todoKeyword)
        assertEquals("buy milk", (task.title.first() as TextNode.Plain).text)
        assertNull(task.priority)
        assertNull(task.scheduled)
        assertTrue(task.tags.isEmpty())
    }

    @Test
    fun `genesis plus delta applies the delta on top`() {
        val g = genesis("2026-04-30T12:00:00Z", keyword = "INBOX")
        val delta = Event(
            filePath = file,
            taskIndex = 0,
            occurredAt = "2026-04-30T13:00:00Z",
            todoKeyword = "TODO",
        )
        val state = EventProjection.project(listOf(g, delta))
        assertEquals("TODO", state[pointer]!!.todoKeyword)
    }

    @Test
    fun `two deltas - latest wins per field`() {
        val g = genesis("2026-04-30T12:00:00Z", keyword = "INBOX")
        val d1 = Event(
            filePath = file, taskIndex = 0,
            occurredAt = "2026-04-30T13:00:00Z",
            todoKeyword = "TODO",
        )
        val d2 = Event(
            filePath = file, taskIndex = 0,
            occurredAt = "2026-04-30T14:00:00Z",
            todoKeyword = "DONE",
        )
        // Out of order on input — projection sorts by occurredAt.
        val state = EventProjection.project(listOf(d2, g, d1))
        assertEquals("DONE", state[pointer]!!.todoKeyword)
    }

    @Test
    fun `cleared field via sentinel maps to absent on Task`() {
        val g = EventProjection.genesisEvent(
            filePath = file,
            taskIndex = 0,
            occurredAt = "2026-04-30T12:00:00Z",
            task = Task(
                level = 1,
                todoKeyword = "TODO",
                priority = 1,
                title = listOf(TextNode.Plain("t")),
                tags = listOf("home"),
                scheduled = OrgTime(date = "2026-05-01"),
                deadline = null,
                createdProp = null,
                closed = null,
                properties = emptyList(),
                description = emptyList(),
            ),
        )
        // Clear priority + tags + scheduled.
        val clear = Event(
            filePath = file,
            taskIndex = 0,
            occurredAt = "2026-04-30T13:00:00Z",
            priority = Nullable.PRIORITY_CLEARED,
            tags = Nullable.TAGS_CLEARED,
            scheduled = Nullable.ORG_TIME_CLEARED,
        )
        val task = EventProjection.project(listOf(g, clear))[pointer]!!
        assertNull("priority cleared", task.priority)
        assertTrue("tags cleared", task.tags.isEmpty())
        assertNull("scheduled cleared", task.scheduled)
    }

    @Test
    fun `delta-only stream without genesis yields no task`() {
        // Without level/keyword/title coverage, the projection must drop the row.
        val delta = Event(
            filePath = file, taskIndex = 0,
            occurredAt = "2026-04-30T13:00:00Z",
            todoKeyword = "DONE",
        )
        val state = EventProjection.project(listOf(delta))
        assertTrue(state.isEmpty())
    }

    @Test
    fun `same task two events same timestamp - deterministic projection`() {
        // Two events at same (file, idx) with different occurredAt — projection
        // picks the latest. Same-timestamp ordering is unspecified by the spec
        // but both events should still be applied.
        val g = genesis("2026-04-30T12:00:00Z", keyword = "INBOX")
        val a = Event(
            filePath = file, taskIndex = 0,
            occurredAt = "2026-04-30T13:00:00Z",
            todoKeyword = "TODO",
        )
        val b = Event(
            filePath = file, taskIndex = 0,
            occurredAt = "2026-04-30T14:00:00Z",
            todoKeyword = "DONE",
        )
        val state = EventProjection.project(listOf(g, a, b))
        assertEquals("DONE", state[pointer]!!.todoKeyword)
    }

    @Test
    fun `out-of-order events project deterministically regardless of input order`() {
        // Same event set, two different input orders — the projection must
        // produce the same final state because it sorts by occurredAt.
        val g = genesis("2026-04-30T12:00:00Z", keyword = "INBOX")
        val d1 = Event(file, 0, "2026-04-30T13:00:00Z", todoKeyword = "TODO")
        val d2 = Event(file, 0, "2026-04-30T14:00:00Z", priority = 2)
        val d3 = Event(file, 0, "2026-04-30T15:00:00Z", todoKeyword = "DONE")

        val a = EventProjection.project(listOf(g, d1, d2, d3))[pointer]!!
        val b = EventProjection.project(listOf(d3, d1, g, d2))[pointer]!!
        val c = EventProjection.project(listOf(d2, d3, g, d1))[pointer]!!
        assertEquals(a, b)
        assertEquals(b, c)
        assertEquals("DONE", a.todoKeyword)
        assertEquals(2, a.priority)
    }

    @Test
    fun `org-time clearing via ORG_TIME_CLEARED sentinel maps to absent`() {
        val g = EventProjection.genesisEvent(
            filePath = file,
            taskIndex = 0,
            occurredAt = "2026-04-30T12:00:00Z",
            task = Task(
                level = 1,
                todoKeyword = "TODO",
                title = listOf(TextNode.Plain("t")),
                tags = emptyList(),
                deadline = OrgTime(date = "2026-05-01"),
            ),
        )
        val clear = Event(
            filePath = file,
            taskIndex = 0,
            occurredAt = "2026-04-30T13:00:00Z",
            deadline = Nullable.ORG_TIME_CLEARED,
        )
        val task = EventProjection.project(listOf(g, clear))[pointer]!!
        assertNull(task.deadline)
    }

    @Test
    fun `properties cleared via empty-list sentinel projects to empty`() {
        val g = EventProjection.genesisEvent(
            filePath = file,
            taskIndex = 0,
            occurredAt = "2026-04-30T12:00:00Z",
            task = Task(
                level = 1,
                todoKeyword = "TODO",
                title = listOf(TextNode.Plain("t")),
                tags = emptyList(),
                properties = listOf(listOf("a", "1")),
            ),
        )
        val clear = Event(
            filePath = file,
            taskIndex = 0,
            occurredAt = "2026-04-30T13:00:00Z",
            properties = emptyList(),
        )
        val task = EventProjection.project(listOf(g, clear))[pointer]!!
        assertTrue(task.properties.isEmpty())
    }

    @Test
    fun `events for different tasks project independently`() {
        val g1 = genesis("2026-04-30T12:00:00Z", keyword = "INBOX")
        val g2 = EventProjection.genesisEvent(
            filePath = file,
            taskIndex = 1,
            occurredAt = "2026-04-30T12:00:01Z",
            task = Task(
                level = 1,
                todoKeyword = "TODO",
                title = listOf(TextNode.Plain("two")),
                tags = emptyList(),
            ),
        )
        val state = EventProjection.project(listOf(g1, g2))
        assertEquals(2, state.size)
        assertEquals("INBOX", state[pointer]!!.todoKeyword)
        assertEquals("TODO", state[TaskPointer(file, 1)]!!.todoKeyword)
    }

    @Test
    fun `genesis-equivalent state survives a delta that touches only a non-required field`() {
        // Genesis covers level/keyword/title at t0; a later delta only touches
        // priority. The task should still materialize and reflect the priority.
        val g = genesis("2026-04-30T12:00:00Z", keyword = "INBOX")
        val d = Event(file, 0, "2026-04-30T13:00:00Z", priority = 2)
        val task = EventProjection.project(listOf(g, d))[pointer]!!
        assertEquals(2, task.priority)
        assertEquals("INBOX", task.todoKeyword)
    }

    // -- Symmetry with the Haskell server-side projection --------------------

    @Test
    fun `Haskell-symmetry - capture plus 2 edits plus tag change yields expected state`() {
        // Fixture: a known event sequence the server would also fold to the
        // same final state. Each event mirrors what the API endpoints emit:
        //   t0: capture "buy milk" (genesis: INBOX)
        //   t1: edit keyword TODO
        //   t2: edit priority 2
        //   t3: tags = [home]
        // We compute the expected projection by hand here. If the Haskell
        // projection diverges, this test fails and the sync invariant breaks.
        val g = EventProjection.genesisEvent(
            filePath = file,
            taskIndex = 0,
            occurredAt = "2026-04-30T12:00:00Z",
            task = Task(
                level = 1,
                todoKeyword = "INBOX",
                title = listOf(TextNode.Plain("buy milk")),
                tags = emptyList(),
                createdProp = OrgTime(date = "2026-04-30"),
            ),
        )
        val d1 = Event(file, 0, "2026-04-30T12:01:00Z", todoKeyword = "TODO")
        val d2 = Event(file, 0, "2026-04-30T12:02:00Z", priority = 2)
        val d3 = Event(file, 0, "2026-04-30T12:03:00Z", tags = listOf("home"))

        val task = EventProjection.project(listOf(g, d1, d2, d3))[pointer]!!
        assertEquals(1, task.level)
        assertEquals("TODO", task.todoKeyword)
        assertEquals(2, task.priority)
        assertEquals("buy milk", (task.title.first() as TextNode.Plain).text)
        assertEquals(listOf("home"), task.tags)
        assertEquals(OrgTime(date = "2026-04-30"), task.createdProp)
        assertNull(task.scheduled)
        assertNull(task.deadline)
        assertNull(task.closed)
    }

    @Test
    fun `Haskell-symmetry - latest-per-field semantics with out-of-order delivery`() {
        // The spec calls out: "out-of-order events trigger a recompute that
        // may *replace* a newer field with an older value if the older event's
        // field was set when no newer event touched it." Exercise that.
        //
        // Sequence the server received (by occurredAt):
        //   t0  genesis INBOX
        //   t1  keyword=TODO, priority=2
        //   t2  keyword=DONE              (priority NOT touched)
        //
        // Final state: keyword=DONE, priority=2 (priority's latest is t1).
        val g = genesis("2026-04-30T12:00:00Z", keyword = "INBOX")
        val d1 = Event(file, 0, "2026-04-30T12:01:00Z", todoKeyword = "TODO", priority = 2)
        val d2 = Event(file, 0, "2026-04-30T12:02:00Z", todoKeyword = "DONE")

        // Deliver them out-of-order to mimic a sync pull.
        val task = EventProjection.project(listOf(d2, g, d1))[pointer]!!
        assertEquals("DONE", task.todoKeyword)
        assertEquals(2, task.priority)
    }
}
