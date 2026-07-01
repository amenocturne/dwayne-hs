package com.skril.dwayne.notifications

import com.skril.dwayne.data.model.OrgTime
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskPointer
import com.skril.dwayne.data.model.TextNode
import java.time.LocalDateTime
import java.time.ZoneId
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class ScheduledReminderPlanTest {
    private val zone = ZoneId.of("UTC")

    @Test
    fun `upcoming includes only future scheduled values with time`() {
        val tasks = mapOf(
            TaskPointer("/Inbox.org", 1) to task("future", OrgTime("2026-07-02", "09:30")),
            TaskPointer("/Inbox.org", 2) to task("date only", OrgTime("2026-07-02")),
            TaskPointer("/Inbox.org", 3) to task("past", OrgTime("2026-06-30", "09:30")),
        )
        val now = LocalDateTime.parse("2026-07-01T12:00:00").atZone(zone).toInstant().toEpochMilli()

        val reminders = ScheduledReminderPlan.upcoming(tasks, nowMillis = now, zoneId = zone)

        assertEquals(1, reminders.size)
        assertEquals("/Inbox.org", reminders.single().file)
        assertEquals(1, reminders.single().taskIndex)
        assertEquals("future", reminders.single().title)
    }

    @Test
    fun `invalid scheduled time is ignored`() {
        val tasks = mapOf(
            TaskPointer("/Inbox.org", 1) to task("bad", OrgTime("2026-07-02", "nope")),
        )

        assertTrue(ScheduledReminderPlan.upcoming(tasks, zoneId = zone).isEmpty())
    }

    private fun task(title: String, scheduled: OrgTime): Task =
        Task(
            level = 1,
            todoKeyword = "TODO",
            priority = null,
            title = listOf(TextNode.Plain(title)),
            tags = emptyList(),
            scheduled = scheduled,
            deadline = null,
            createdProp = null,
            closed = null,
            properties = emptyList(),
            description = emptyList(),
        )
}
