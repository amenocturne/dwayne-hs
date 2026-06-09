package com.skril.dwayne.ui.screens.swipe

import com.skril.dwayne.data.model.EditTaskRequest
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskPointer
import com.skril.dwayne.data.model.TaskWithPointer
import com.skril.dwayne.data.repository.TaskRepository

sealed class ProcessingSwipeResolution {
    data object Ignore : ProcessingSwipeResolution()
    data object NavigateBack : ProcessingSwipeResolution()
    data class EnterBranch(val branch: Branch) : ProcessingSwipeResolution()
    data class ApplyTerminal(val terminal: Terminal) : ProcessingSwipeResolution()
}

fun resolveProcessingSwipe(
    currentNode: Branch,
    backDir: Dir?,
    dir: Dir,
): ProcessingSwipeResolution {
    if (dir == backDir) return ProcessingSwipeResolution.NavigateBack

    return when (val child = currentNode.childAt(dir)) {
        null -> ProcessingSwipeResolution.Ignore
        is Branch -> ProcessingSwipeResolution.EnterBranch(child)
        is Terminal -> ProcessingSwipeResolution.ApplyTerminal(child)
    }
}

suspend fun applyProcessingModification(
    repository: TaskRepository,
    pointer: TaskPointer,
    task: Task,
    modification: Modification,
): TaskWithPointer = when (modification) {
    is Modification.SetKeyword ->
        repository.changeKeyword(pointer, modification.keyword)

    is Modification.SetKeywordAndTags ->
        repository.editTask(
            EditTaskRequest(
                file = pointer.file,
                taskIndex = pointer.taskIndex,
                keyword = modification.keyword,
                tags = task.tags.withAddedTags(modification.tags),
            )
        )
}

fun List<String>.withAddedTags(tagsToAdd: List<String>): List<String> =
    (this + tagsToAdd).distinct()
