package com.skril.dwayne.ui.screens.swipe

import androidx.compose.ui.graphics.Color
import com.skril.dwayne.data.model.EditTaskRequest
import com.skril.dwayne.data.model.PaginatedResponse
import com.skril.dwayne.data.model.PaginationMetadata
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskPointer
import com.skril.dwayne.data.model.TaskWithPointer
import com.skril.dwayne.data.model.TextNode
import com.skril.dwayne.data.repository.TaskRepository
import kotlinx.coroutines.test.runTest
import org.junit.Assert.assertEquals
import org.junit.Assert.assertSame
import org.junit.Assert.assertTrue
import org.junit.Test

class ProcessingActionsTest {

    @Test
    fun `resolves back direction before child lookup`() {
        val root = emptyBranch("root", Color.Unspecified)
            .withChild(Dir.Left, Terminal("DONE", Color.Green, Modification.SetKeyword("DONE")))

        val resolution = resolveProcessingSwipe(root, backDir = Dir.Left, dir = Dir.Left)

        assertEquals(ProcessingSwipeResolution.NavigateBack, resolution)
    }

    @Test
    fun `resolves branch child as branch navigation`() {
        val branch = emptyBranch("deeper", Color.Blue)
        val root = emptyBranch("root", Color.Unspecified).withChild(Dir.Right, branch)

        val resolution = resolveProcessingSwipe(root, backDir = null, dir = Dir.Right)

        assertTrue(resolution is ProcessingSwipeResolution.EnterBranch)
        assertSame(branch, (resolution as ProcessingSwipeResolution.EnterBranch).branch)
    }

    @Test
    fun `set keyword modification delegates to changeKeyword`() = runTest {
        val repository = RecordingTaskRepository()
        val pointer = TaskPointer("/inbox.org", 7)
        val task = testTask(keyword = "INBOX")

        applyProcessingModification(repository, pointer, task, Modification.SetKeyword("DONE"))

        assertEquals(pointer, repository.changedKeywordPointer)
        assertEquals("DONE", repository.changedKeyword)
    }

    @Test
    fun `set keyword and tags modification preserves existing tags and de-duplicates new ones`() = runTest {
        val repository = RecordingTaskRepository()
        val pointer = TaskPointer("/inbox.org", 7)
        val task = testTask(keyword = "INBOX", tags = listOf("music"))

        applyProcessingModification(
            repository,
            pointer,
            task,
            Modification.SetKeywordAndTags("LIST", listOf("music", "download")),
        )

        assertEquals(
            EditTaskRequest(
                file = "/inbox.org",
                taskIndex = 7,
                keyword = "LIST",
                tags = listOf("music", "download"),
            ),
            repository.editRequest,
        )
    }
}

private class RecordingTaskRepository : TaskRepository {
    var changedKeywordPointer: TaskPointer? = null
    var changedKeyword: String? = null
    var editRequest: EditTaskRequest? = null

    override suspend fun getView(viewName: String, offset: Int, limit: Int): PaginatedResponse =
        PaginatedResponse(emptyList(), PaginationMetadata(0))

    override suspend fun search(query: String, view: String?, offset: Int, limit: Int): PaginatedResponse =
        PaginatedResponse(emptyList(), PaginationMetadata(0))

    override suspend fun recentCaptures(limit: Int): List<TaskWithPointer> =
        emptyList()

    override suspend fun capture(title: String): TaskWithPointer =
        error("not used")

    override suspend fun editTask(request: EditTaskRequest): TaskWithPointer {
        editRequest = request
        return TaskWithPointer(
            testTask(keyword = request.keyword ?: "INBOX"),
            TaskPointer(request.file, request.taskIndex),
        )
    }

    override suspend fun changeKeyword(pointer: TaskPointer, keyword: String): TaskWithPointer {
        changedKeywordPointer = pointer
        changedKeyword = keyword
        return TaskWithPointer(testTask(keyword = keyword), pointer)
    }

    override suspend fun changePriority(pointer: TaskPointer, priority: Int?): TaskWithPointer =
        error("not used")

    override suspend fun addTag(pointer: TaskPointer, tag: String): TaskWithPointer =
        error("not used")

    override suspend fun removeTag(pointer: TaskPointer, tag: String): TaskWithPointer =
        error("not used")

    override suspend fun deleteTask(pointer: TaskPointer): TaskWithPointer =
        error("not used")
}

private fun testTask(keyword: String, tags: List<String> = emptyList()): Task =
    Task(
        level = 1,
        todoKeyword = keyword,
        title = listOf(TextNode.Plain("Task")),
        tags = tags,
    )
