package com.skril.dwayne.notifications

import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskPointer
import com.skril.dwayne.data.model.TextNode
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException

data class ScheduledReminder(
    val file: String,
    val taskIndex: Int,
    val title: String,
    val triggerAtMillis: Long,
) {
    val notificationId: Int =
        (31 * file.hashCode() + taskIndex).let { if (it == Int.MIN_VALUE) 0 else kotlin.math.abs(it) }
}

object ScheduledReminderPlan {
    private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

    fun upcoming(
        tasks: Map<TaskPointer, Task>,
        dateOnlyReminderTime: String,
        nowMillis: Long = System.currentTimeMillis(),
        zoneId: ZoneId = ZoneId.systemDefault(),
    ): List<ScheduledReminder> =
        tasks.mapNotNull { (pointer, task) ->
            val scheduled = task.scheduled ?: return@mapNotNull null
            val time = scheduled.time ?: dateOnlyReminderTime
            val triggerAtMillis = parseTriggerMillis(scheduled.date, time, zoneId) ?: return@mapNotNull null
            if (triggerAtMillis <= nowMillis) return@mapNotNull null
            ScheduledReminder(
                file = pointer.file,
                taskIndex = pointer.taskIndex,
                title = task.title.toReminderTitle(),
                triggerAtMillis = triggerAtMillis,
            )
        }.sortedBy { it.triggerAtMillis }

    fun parseTriggerMillis(date: String, time: String, zoneId: ZoneId = ZoneId.systemDefault()): Long? =
        try {
            LocalDateTime.parse("$date $time", dateTimeFormatter)
                .atZone(zoneId)
                .toInstant()
                .toEpochMilli()
        } catch (_: DateTimeParseException) {
            null
        }

    private fun List<TextNode>.toReminderTitle(): String =
        joinToString("") { node ->
            when (node) {
                is TextNode.Plain -> node.text
                is TextNode.Link -> node.title ?: node.url
            }
        }.ifBlank { "Scheduled task" }
}
