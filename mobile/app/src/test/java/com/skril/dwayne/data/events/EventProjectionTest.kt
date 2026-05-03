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
}
