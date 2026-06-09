package com.skril.dwayne.data.query

import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskPointer
import com.skril.dwayne.data.model.TaskWithPointer
import com.skril.dwayne.data.model.TextNode
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class TaskQueriesTest {

    @Test
    fun `feed views define the mobile tab order`() {
        assertEquals(
            listOf(
                "work-queue",
                "inbox",
                "defer",
                "today",
                "soon",
                "todo",
                "waiting",
                "someday",
                "list",
                "done",
            ),
            MobileFeedViews.map { it.viewName },
        )
    }

    @Test
    fun `search filters include all feed views plus all`() {
        assertEquals(null, MobileSearchViewFilters.first().viewName)
        assertEquals(
            MobileFeedViews.map { it.viewName },
            MobileSearchViewFilters.drop(1).map { it.viewName },
        )
    }

    @Test
    fun `work queue includes today and soon sorted by priority`() {
        val tasks = listOf(
            taskWithPointer(0, task(keyword = "INBOX")),
            taskWithPointer(1, task(keyword = "SOON", priority = 3)),
            taskWithPointer(2, task(keyword = "TODAY", priority = 1)),
            taskWithPointer(3, task(keyword = "SOON", priority = null)),
        )

        val result = applyTaskView("work-queue", tasks)

        assertEquals(listOf(2, 1, 3), result.map { it.pointer.taskIndex })
    }

    @Test
    fun `search matches text tags and keyword case-insensitively`() {
        val tasks = listOf(
            taskWithPointer(0, task(keyword = "INBOX", title = "Buy milk")),
            taskWithPointer(1, task(keyword = "LIST", tags = listOf("Music"))),
            taskWithPointer(2, task(keyword = "WAITING", description = "Reply from Alex")),
        )

        assertEquals(listOf(0), filterTaskSearch(tasks, "MILK").map { it.pointer.taskIndex })
        assertEquals(listOf(1), filterTaskSearch(tasks, "music").map { it.pointer.taskIndex })
        assertEquals(listOf(2), filterTaskSearch(tasks, "waiting").map { it.pointer.taskIndex })
    }

    @Test
    fun `recent phone captures preserve pointer order and drop missing tasks`() {
        val first = TaskPointer("/inbox.org", 1)
        val missing = TaskPointer("/inbox.org", 2)
        val second = TaskPointer("/inbox.org", 3)

        val result = resolveRecentPhoneCaptures(
            pointers = listOf(first, missing, second),
            tasksByPointer = mapOf(
                second to task(keyword = "INBOX", title = "Second"),
                first to task(keyword = "INBOX", title = "First"),
            ),
        )

        assertEquals(listOf(first, second), result.map { it.pointer })
    }

    @Test
    fun `processing filter supports keyword tag and text terms`() {
        val task = task(
            keyword = "INBOX",
            title = "Watch documentary",
            tags = listOf("music"),
            description = "Download stems",
        )

        assertTrue(matchesProcessingFilter(task, "keyword:inbox tag:music stems"))
        assertFalse(matchesProcessingFilter(task, "keyword:done tag:music stems"))
    }
}

private fun taskWithPointer(index: Int, task: Task): TaskWithPointer =
    TaskWithPointer(task = task, pointer = TaskPointer("/tasks.org", index))

private fun task(
    keyword: String,
    title: String = "Task",
    tags: List<String> = emptyList(),
    description: String = "",
    priority: Int? = null,
): Task =
    Task(
        level = 1,
        todoKeyword = keyword,
        priority = priority,
        title = listOf(TextNode.Plain(title)),
        tags = tags,
        description = if (description.isBlank()) emptyList() else listOf(TextNode.Plain(description)),
    )
