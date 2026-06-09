package com.skril.dwayne.ui.components

import androidx.compose.foundation.clickable
import androidx.compose.ui.Modifier
import com.skril.dwayne.data.model.TaskPointer

sealed class TaskCardInteraction {
    data object PreviewOnly : TaskCardInteraction()
    data class OpenDetail(
        val pointer: TaskPointer,
        val onOpen: (TaskPointer) -> Unit,
    ) : TaskCardInteraction()
}

fun TaskCardInteraction.opensDetail(): Boolean =
    this is TaskCardInteraction.OpenDetail

fun Modifier.taskCardInteraction(interaction: TaskCardInteraction): Modifier =
    when (interaction) {
        TaskCardInteraction.PreviewOnly -> this
        is TaskCardInteraction.OpenDetail -> clickable {
            interaction.onOpen(interaction.pointer)
        }
    }
