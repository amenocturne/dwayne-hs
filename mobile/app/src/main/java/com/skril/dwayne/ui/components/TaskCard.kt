package com.skril.dwayne.ui.components

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.OrgTime
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TextNode
import com.skril.dwayne.ui.theme.keywordColor
import com.skril.dwayne.ui.theme.priorityColor

// Atomic parts: each renders one slice of a Task. Compose freely.

@Composable
fun TaskCardHeader(
    task: Task,
    style: TextStyle = MaterialTheme.typography.labelSmall,
) {
    Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        Text(
            text = task.todoKeyword,
            style = style,
            color = keywordColor(task.todoKeyword),
        )
        task.priority?.let { p ->
            Text(
                text = priorityLabel(p),
                style = style,
                color = priorityColor(p),
            )
        }
    }
}

@Composable
fun TaskCardTitle(
    task: Task,
    style: TextStyle = MaterialTheme.typography.bodyLarge,
    maxLines: Int = Int.MAX_VALUE,
) {
    Text(
        text = task.title.toPlainString(),
        style = style,
        color = MaterialTheme.colorScheme.onSurface,
        maxLines = maxLines,
        overflow = TextOverflow.Ellipsis,
    )
}

@Composable
fun TaskCardTags(
    task: Task,
    style: TextStyle = MaterialTheme.typography.labelSmall,
) {
    if (task.tags.isEmpty()) return
    Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
        task.tags.forEach { tag ->
            Text(
                text = ":$tag:",
                style = style,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
    }
}

@Composable
fun TaskCardDescription(
    task: Task,
    style: TextStyle = MaterialTheme.typography.bodyMedium,
    maxLines: Int = Int.MAX_VALUE,
) {
    val descText = task.description.toPlainString()
    if (descText.isBlank()) return
    Text(
        text = descText,
        style = style,
        color = MaterialTheme.colorScheme.onSurfaceVariant,
        maxLines = maxLines,
        overflow = TextOverflow.Ellipsis,
    )
}

@Composable
fun TaskCardScheduledLine(
    task: Task,
    style: TextStyle = MaterialTheme.typography.labelSmall,
) {
    val s = task.scheduled ?: return
    Text(
        text = "SCHEDULED: ${formatOrgTime(s)}",
        style = style,
        color = MaterialTheme.colorScheme.onSurfaceVariant,
    )
}

@Composable
fun TaskCardDeadlineLine(
    task: Task,
    style: TextStyle = MaterialTheme.typography.labelSmall,
) {
    val d = task.deadline ?: return
    Text(
        text = "DEADLINE: ${formatOrgTime(d)}",
        style = style,
        color = MaterialTheme.colorScheme.onSurfaceVariant,
    )
}

// Compact list-style card. Composes the atomic parts.

@Composable
fun TaskCard(
    task: Task,
    modifier: Modifier = Modifier,
    interaction: TaskCardInteraction = TaskCardInteraction.PreviewOnly,
) {
    Column(
        modifier = modifier
            .fillMaxWidth()
            .clip(RoundedCornerShape(12.dp))
            .background(MaterialTheme.colorScheme.surface)
            .taskCardInteraction(interaction)
            .padding(16.dp),
    ) {
        TaskCardHeader(task)
        Spacer(modifier = Modifier.height(4.dp))
        TaskCardTitle(task, maxLines = 2)
        if (task.tags.isNotEmpty()) {
            Spacer(modifier = Modifier.height(8.dp))
            TaskCardTags(task)
        }
        if (task.description.toPlainString().isNotBlank()) {
            Spacer(modifier = Modifier.height(4.dp))
            TaskCardDescription(task, maxLines = 1)
        }
        if (task.scheduled != null) {
            Spacer(modifier = Modifier.height(4.dp))
            TaskCardScheduledLine(task)
        }
        if (task.deadline != null) {
            Spacer(modifier = Modifier.height(2.dp))
            TaskCardDeadlineLine(task)
        }
    }
}

private fun priorityLabel(p: Int): String = when (p) {
    1 -> "#A"; 2 -> "#B"; 3 -> "#C"; else -> "#$p"
}

private fun formatOrgTime(t: OrgTime): String =
    if (t.time != null) "${t.date} ${t.time}" else t.date

fun List<TextNode>.toPlainString(): String = joinToString("") { node ->
    when (node) {
        is TextNode.Plain -> node.text
        is TextNode.Link -> node.title ?: node.url
    }
}

fun firstLinkUrl(task: Task): String? {
    fun firstIn(nodes: List<TextNode>): String? =
        nodes.firstNotNullOfOrNull { it as? TextNode.Link }?.url
    return firstIn(task.title) ?: firstIn(task.description)
}
