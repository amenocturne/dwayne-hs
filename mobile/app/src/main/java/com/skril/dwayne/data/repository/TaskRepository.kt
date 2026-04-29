package com.skril.dwayne.data.repository

import com.skril.dwayne.data.mock.MockData
import com.skril.dwayne.data.model.*

interface TaskRepository {
    suspend fun getView(viewName: String, offset: Int = 0, limit: Int = 100): PaginatedResponse
    suspend fun search(query: String, view: String? = null, offset: Int = 0, limit: Int = 100): PaginatedResponse
    suspend fun capture(title: String): TaskWithPointer
    suspend fun editTask(request: EditTaskRequest): TaskWithPointer
    suspend fun changeKeyword(pointer: TaskPointer, keyword: String): TaskWithPointer
    suspend fun changePriority(pointer: TaskPointer, priority: Int?): TaskWithPointer
    suspend fun addTag(pointer: TaskPointer, tag: String): TaskWithPointer
    suspend fun removeTag(pointer: TaskPointer, tag: String): TaskWithPointer
    suspend fun deleteTask(pointer: TaskPointer): TaskWithPointer
}

class MockTaskRepository : TaskRepository {

    private val tasks = MockData.allTasks.toMutableList()

    override suspend fun getView(viewName: String, offset: Int, limit: Int): PaginatedResponse {
        val filtered = when (viewName) {
            "inbox" -> tasks.filter { it.task.todoKeyword == "INBOX" }
            "today" -> tasks.filter { it.task.todoKeyword == "TODAY" }.sortedBy { it.task.priority ?: Int.MAX_VALUE }
            "soon" -> tasks.filter { it.task.todoKeyword == "SOON" }.sortedBy { it.task.priority ?: Int.MAX_VALUE }
            "todo" -> tasks.filter { it.task.todoKeyword == "TODO" }.sortedBy { it.task.priority ?: Int.MAX_VALUE }
            "waiting" -> tasks.filter { it.task.todoKeyword == "WAITING" }
            "someday" -> tasks.filter { it.task.todoKeyword == "SOMEDAY" }
            "list" -> tasks.filter { it.task.todoKeyword == "LIST" }
            "work-queue" -> tasks.filter { it.task.todoKeyword in listOf("TODAY", "SOON") }.sortedBy { it.task.priority ?: Int.MAX_VALUE }
            "done" -> tasks.filter { it.task.todoKeyword == "DONE" }
            "trash" -> tasks.filter { it.task.todoKeyword == "TRASH" }
            else -> tasks
        }
        return MockData.paginatedResponse(filtered, offset, limit)
    }

    override suspend fun search(query: String, view: String?, offset: Int, limit: Int): PaginatedResponse {
        val base = if (view != null) {
            getView(view).data
        } else {
            tasks
        }
        val q = query.lowercase()
        val results = base.filter { twp ->
            val titleText = twp.task.title.filterIsInstance<TextNode.Plain>().joinToString("") { it.text }
            val descText = twp.task.description.filterIsInstance<TextNode.Plain>().joinToString("") { it.text }
            titleText.lowercase().contains(q) || descText.lowercase().contains(q) || twp.task.tags.any { it.lowercase().contains(q) }
        }
        return MockData.paginatedResponse(results, offset, limit)
    }

    override suspend fun capture(title: String): TaskWithPointer {
        val newTask = TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "INBOX",
                title = listOf(TextNode.Plain(title)),
                createdProp = OrgTime(date = "2026-03-07"),
            ),
            pointer = TaskPointer(file = "/inbox.org", taskIndex = tasks.size),
        )
        tasks.add(newTask)
        return newTask
    }

    override suspend fun editTask(request: EditTaskRequest): TaskWithPointer {
        val pointer = TaskPointer(request.file, request.taskIndex)
        val idx = tasks.indexOfFirst { it.pointer == pointer }
        if (idx == -1) error("Task not found")
        var task = tasks[idx].task
        request.keyword?.let { task = task.copy(todoKeyword = it) }
        request.priority?.let { task = task.copy(priority = it.value) }
        request.title?.let { task = task.copy(title = listOf(TextNode.Plain(it))) }
        request.tags?.let { task = task.copy(tags = it) }
        request.scheduled?.let { task = task.copy(scheduled = it.value) }
        request.deadline?.let { task = task.copy(deadline = it.value) }
        val updated = tasks[idx].copy(task = task)
        tasks[idx] = updated
        return updated
    }

    override suspend fun changeKeyword(pointer: TaskPointer, keyword: String): TaskWithPointer =
        editTask(EditTaskRequest(file = pointer.file, taskIndex = pointer.taskIndex, keyword = keyword))

    override suspend fun changePriority(pointer: TaskPointer, priority: Int?): TaskWithPointer =
        editTask(EditTaskRequest(file = pointer.file, taskIndex = pointer.taskIndex, priority = ClearOrSet(priority)))

    override suspend fun addTag(pointer: TaskPointer, tag: String): TaskWithPointer {
        val idx = tasks.indexOfFirst { it.pointer == pointer }
        if (idx == -1) error("Task not found")
        val updated = tasks[idx].let { it.copy(task = it.task.copy(tags = it.task.tags + tag)) }
        tasks[idx] = updated
        return updated
    }

    override suspend fun removeTag(pointer: TaskPointer, tag: String): TaskWithPointer {
        val idx = tasks.indexOfFirst { it.pointer == pointer }
        if (idx == -1) error("Task not found")
        val updated = tasks[idx].let { it.copy(task = it.task.copy(tags = it.task.tags - tag)) }
        tasks[idx] = updated
        return updated
    }

    override suspend fun deleteTask(pointer: TaskPointer): TaskWithPointer =
        editTask(EditTaskRequest(file = pointer.file, taskIndex = pointer.taskIndex, keyword = "TRASH"))
}
