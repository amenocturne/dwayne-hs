package com.skril.dwayne.data.query

import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskWithPointer
import com.skril.dwayne.data.model.TextNode

data class TaskViewOption(
    val viewName: String,
    val label: String,
)

data class TaskSearchViewOption(
    val viewName: String?,
    val label: String,
)

val MobileFeedViews: List<TaskViewOption> = listOf(
    TaskViewOption("work-queue", "work-queue"),
    TaskViewOption("inbox", "inbox"),
    TaskViewOption("defer", "defer"),
    TaskViewOption("today", "today"),
    TaskViewOption("soon", "soon"),
    TaskViewOption("todo", "todo"),
    TaskViewOption("waiting", "waiting"),
    TaskViewOption("someday", "someday"),
    TaskViewOption("list", "list"),
    TaskViewOption("done", "done"),
)

val MobileSearchViewFilters: List<TaskSearchViewOption> =
    listOf(TaskSearchViewOption(null, "All")) +
        MobileFeedViews.map { TaskSearchViewOption(it.viewName, it.label) }

fun applyTaskView(viewName: String, tasks: List<TaskWithPointer>): List<TaskWithPointer> =
    when (viewName) {
        "inbox" -> tasks.filterByKeyword("INBOX")
        "defer" -> tasks.filterByKeyword("DEFER")
        "today" -> tasks.filterByKeyword("TODAY").sortedByPriority()
        "soon" -> tasks.filterByKeyword("SOON").sortedByPriority()
        "todo" -> tasks.filterByKeyword("TODO").sortedByPriority()
        "waiting" -> tasks.filterByKeyword("WAITING")
        "someday" -> tasks.filterByKeyword("SOMEDAY")
        "list" -> tasks.filterByKeyword("LIST")
        "work-queue" -> tasks
            .filter { it.task.todoKeyword in listOf("TODAY", "SOON") }
            .sortedByPriority()
        "done" -> tasks.filterByKeyword("DONE")
        "trash" -> tasks.filterByKeyword("TRASH")
        else -> tasks
    }

fun filterTaskSearch(tasks: List<TaskWithPointer>, query: String): List<TaskWithPointer> {
    val normalized = query.trim()
    if (normalized.isEmpty()) return tasks
    return tasks.filter { matchesTaskSearch(it.task, normalized) }
}

fun matchesTaskSearch(task: Task, query: String): Boolean {
    val normalized = query.trim().lowercase()
    if (normalized.isEmpty()) return true

    val titleText = task.title.toSearchText()
    val descText = task.description.toSearchText()
    return titleText.contains(normalized, ignoreCase = true)
        || descText.contains(normalized, ignoreCase = true)
        || task.tags.any { it.contains(normalized, ignoreCase = true) }
        || task.todoKeyword.contains(normalized, ignoreCase = true)
}

fun matchesProcessingFilter(task: Task, filter: String): Boolean {
    val terms = filter.trim().split(Regex("\\s+")).filter { it.isNotEmpty() }
    if (terms.isEmpty()) return true
    return terms.all { term ->
        when {
            term.startsWith("keyword:", ignoreCase = true) -> {
                val value = term.substringAfter(":")
                task.todoKeyword.equals(value, ignoreCase = true)
            }
            term.startsWith("tag:", ignoreCase = true) -> {
                val value = term.substringAfter(":")
                task.tags.any { it.equals(value, ignoreCase = true) }
            }
            else -> {
                val text = task.title.toSearchText() + " " + task.description.toSearchText()
                text.contains(term, ignoreCase = true)
            }
        }
    }
}

private fun List<TaskWithPointer>.filterByKeyword(keyword: String): List<TaskWithPointer> =
    filter { it.task.todoKeyword == keyword }

private fun List<TaskWithPointer>.sortedByPriority(): List<TaskWithPointer> =
    sortedBy { it.task.priority ?: Int.MAX_VALUE }

private fun List<TextNode>.toSearchText(): String =
    joinToString("") { node ->
        when (node) {
            is TextNode.Plain -> node.text
            is TextNode.Link -> node.title ?: node.url
        }
    }
