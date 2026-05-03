package com.skril.dwayne.data.events

import com.skril.dwayne.data.model.OrgTime
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskPointer
import com.skril.dwayne.data.model.TextNode

/**
 * Pure projection: a list of events folded down to the current Task state.
 *
 * Mirrors `Events.Projection.fileStateFromEvents` on the Haskell side.
 *
 * Algorithm:
 *   1. Group events by (file_path, task_index).
 *   2. For each group, sort by occurredAt ASC.
 *   3. Fold each event onto the running Task — for every populated field on
 *      the event, overwrite the Task's field. If the populated value is the
 *      type's cleared sentinel, the Task's field becomes "absent"
 *      (null / empty list / empty string).
 *   4. Drop tasks that never received a genesis event (level + keyword +
 *      title must all have been touched at some point).
 */
object EventProjection {

    fun project(events: List<Event>): Map<TaskPointer, Task> {
        val grouped = events.groupBy { TaskPointer(it.filePath, it.taskIndex) }
        val out = LinkedHashMap<TaskPointer, Task>(grouped.size)
        for ((pointer, evs) in grouped) {
            val task = foldTaskEvents(evs) ?: continue
            out[pointer] = task
        }
        return out
    }

    /**
     * Materialize a single task from its events. Returns null if no event in
     * the stream covered the required fields (level, todoKeyword, title) —
     * i.e. there's no genesis on record.
     */
    fun foldTaskEvents(events: List<Event>): Task? {
        if (events.isEmpty()) return null
        val sorted = events.sortedBy { it.occurredAt }
        var task = EMPTY_TASK
        var sawLevel = false
        var sawKeyword = false
        var sawTitle = false
        for (e in sorted) {
            task = applyEventToTask(task, e)
            if (e.level != null) sawLevel = true
            if (e.todoKeyword != null) sawKeyword = true
            if (e.title != null) sawTitle = true
        }
        return if (sawLevel && sawKeyword && sawTitle) task else null
    }

    fun applyEventToTask(task: Task, e: Event): Task = task.copy(
        level = e.level ?: task.level,
        todoKeyword = e.todoKeyword?.let {
            if (Nullable.isKeywordNull(it)) "" else it
        } ?: task.todoKeyword,
        priority = if (e.priority == null) task.priority else {
            if (Nullable.isPriorityNull(e.priority)) null else e.priority
        },
        title = if (e.title == null) task.title else {
            if (Nullable.isTitleNull(e.title)) emptyList() else e.title
        },
        tags = if (e.tags == null) task.tags else {
            if (Nullable.isTagsNull(e.tags)) emptyList() else e.tags
        },
        scheduled = applyOrgTime(e.scheduled, task.scheduled),
        deadline = applyOrgTime(e.deadline, task.deadline),
        createdProp = applyOrgTime(e.created, task.createdProp),
        closed = applyOrgTime(e.closed, task.closed),
        properties = if (e.properties == null) task.properties else {
            if (Nullable.isPropertiesNull(e.properties)) emptyList() else e.properties
        },
        description = if (e.description == null) task.description else {
            if (Nullable.isDescriptionNull(e.description)) emptyList() else e.description
        },
    )

    private fun applyOrgTime(eventValue: OrgTime?, prev: OrgTime?): OrgTime? {
        if (eventValue == null) return prev
        return if (Nullable.isOrgTimeNull(eventValue)) null else eventValue
    }

    private val EMPTY_TASK: Task = Task(
        level = 1,
        todoKeyword = "",
        priority = null,
        title = emptyList(),
        tags = emptyList(),
        scheduled = null,
        deadline = null,
        createdProp = null,
        closed = null,
        properties = emptyList(),
        description = emptyList(),
    )

    /** Build a genesis event from a complete Task. Every field becomes Just (using sentinels for absents). */
    fun genesisEvent(
        filePath: String,
        taskIndex: Int,
        occurredAt: String,
        task: Task,
    ): Event = Event(
        filePath = filePath,
        taskIndex = taskIndex,
        occurredAt = occurredAt,
        level = task.level,
        todoKeyword = task.todoKeyword,
        priority = task.priority ?: Nullable.PRIORITY_CLEARED,
        title = if (task.title.isEmpty()) Nullable.TITLE_CLEARED else task.title,
        tags = task.tags,
        scheduled = task.scheduled ?: Nullable.ORG_TIME_CLEARED,
        deadline = task.deadline ?: Nullable.ORG_TIME_CLEARED,
        created = task.createdProp ?: Nullable.ORG_TIME_CLEARED,
        closed = task.closed ?: Nullable.ORG_TIME_CLEARED,
        properties = task.properties,
        description = if (task.description.isEmpty()) Nullable.DESCRIPTION_CLEARED else task.description,
    )

    /** Convert a list of TextNode to plain text for description-from-string genesis. */
    fun plainTitle(text: String): List<TextNode> =
        if (text.isEmpty()) emptyList() else listOf(TextNode.Plain(text))
}
