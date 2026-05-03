package com.skril.dwayne.ui.screens.detail

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material3.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskPointer
import com.skril.dwayne.data.repository.LocalTaskRepository
import com.skril.dwayne.ui.components.toPlainString
import com.skril.dwayne.ui.theme.keywordColor
import com.skril.dwayne.ui.theme.priorityColor

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun TaskDetailScreen(
    repository: LocalTaskRepository,
    file: String,
    taskIndex: Int,
    onBack: () -> Unit,
) {
    val state by repository.state.collectAsState()
    val pointer = TaskPointer(file = file, taskIndex = taskIndex)
    val task = state[pointer]

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Task") },
                navigationIcon = {
                    IconButton(onClick = onBack) {
                        Icon(Icons.AutoMirrored.Default.ArrowBack, contentDescription = "Back")
                    }
                },
            )
        },
    ) { padding ->
        if (task == null) {
            Box(
                modifier = Modifier.fillMaxSize().padding(padding).padding(32.dp),
                contentAlignment = Alignment.Center,
            ) {
                Text("Task not found", color = MaterialTheme.colorScheme.onSurfaceVariant)
            }
        } else {
            TaskDetailBody(
                task = task,
                pointer = pointer,
                modifier = Modifier.padding(padding),
            )
        }
    }
}

@Composable
private fun TaskDetailBody(task: Task, pointer: TaskPointer, modifier: Modifier = Modifier) {
    Column(
        modifier = modifier
            .fillMaxSize()
            .verticalScroll(rememberScrollState())
            .padding(16.dp),
        verticalArrangement = Arrangement.spacedBy(16.dp),
    ) {
        Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            Text(
                text = task.todoKeyword,
                style = MaterialTheme.typography.labelLarge,
                color = keywordColor(task.todoKeyword),
            )
            task.priority?.let { p ->
                Text(
                    text = when (p) {
                        1 -> "#A"
                        2 -> "#B"
                        3 -> "#C"
                        else -> "#$p"
                    },
                    style = MaterialTheme.typography.labelLarge,
                    color = priorityColor(p),
                )
            }
        }

        Text(
            text = task.title.toPlainString(),
            style = MaterialTheme.typography.headlineSmall,
            color = MaterialTheme.colorScheme.onSurface,
        )

        if (task.tags.isNotEmpty()) {
            FlowRowSpaced {
                task.tags.forEach { tag ->
                    Box(
                        modifier = Modifier
                            .clip(RoundedCornerShape(8.dp))
                            .background(MaterialTheme.colorScheme.surfaceVariant)
                            .padding(horizontal = 10.dp, vertical = 4.dp),
                    ) {
                        Text(
                            text = tag,
                            style = MaterialTheme.typography.labelMedium,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                }
            }
        }

        DateField("Scheduled", task.scheduled?.let { fmtTime(it.date, it.time) })
        DateField("Deadline", task.deadline?.let { fmtTime(it.date, it.time) })
        DateField("Created", task.createdProp?.let { fmtTime(it.date, it.time) })
        DateField("Closed", task.closed?.let { fmtTime(it.date, it.time) })

        val descText = task.description.toPlainString()
        if (descText.isNotBlank()) {
            HorizontalDivider()
            Text(
                text = "Description",
                style = MaterialTheme.typography.titleSmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
            Text(
                text = descText,
                style = MaterialTheme.typography.bodyMedium,
                color = MaterialTheme.colorScheme.onSurface,
            )
        }

        if (task.properties.isNotEmpty()) {
            HorizontalDivider()
            Text(
                text = "Properties",
                style = MaterialTheme.typography.titleSmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
            task.properties.forEach { kv ->
                if (kv.size >= 2) {
                    Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                        Text(
                            text = "${kv[0]}:",
                            style = MaterialTheme.typography.labelMedium,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                        Text(
                            text = kv[1],
                            style = MaterialTheme.typography.bodyMedium,
                        )
                    }
                }
            }
        }

        HorizontalDivider()
        Text(
            text = "${pointer.file}  •  index ${pointer.taskIndex}",
            style = MaterialTheme.typography.labelSmall,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            fontFamily = FontFamily.Monospace,
        )
    }
}

@Composable
private fun DateField(label: String, value: String?) {
    if (value == null) return
    Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
        Text(
            text = "$label:",
            style = MaterialTheme.typography.labelMedium,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
        Text(
            text = value,
            style = MaterialTheme.typography.bodyMedium,
        )
    }
}

private fun fmtTime(date: String, time: String?): String =
    if (time != null) "$date $time" else date

@Composable
private fun FlowRowSpaced(content: @Composable () -> Unit) {
    Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) { content() }
}
