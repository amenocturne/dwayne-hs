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
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TextNode
import com.skril.dwayne.ui.theme.keywordColor
import com.skril.dwayne.ui.theme.priorityColor

@Composable
fun TaskCard(
    task: Task,
    modifier: Modifier = Modifier,
) {
    Column(
        modifier = modifier
            .fillMaxWidth()
            .clip(RoundedCornerShape(12.dp))
            .background(MaterialTheme.colorScheme.surface)
            .padding(16.dp),
    ) {
        Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(
                text = task.todoKeyword,
                style = MaterialTheme.typography.labelSmall,
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
                    style = MaterialTheme.typography.labelSmall,
                    color = priorityColor(p),
                )
            }
        }

        Spacer(modifier = Modifier.height(4.dp))

        Text(
            text = task.title.toPlainString(),
            style = MaterialTheme.typography.bodyLarge,
            color = MaterialTheme.colorScheme.onSurface,
            maxLines = 2,
            overflow = TextOverflow.Ellipsis,
        )

        if (task.tags.isNotEmpty()) {
            Spacer(modifier = Modifier.height(8.dp))
            Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
                task.tags.forEach { tag ->
                    Text(
                        text = ":$tag:",
                        style = MaterialTheme.typography.labelSmall,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
            }
        }

        val descText = task.description.toPlainString()
        if (descText.isNotBlank()) {
            Spacer(modifier = Modifier.height(4.dp))
            Text(
                text = descText,
                style = MaterialTheme.typography.bodyMedium,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
                maxLines = 1,
                overflow = TextOverflow.Ellipsis,
            )
        }

        task.scheduled?.let {
            Spacer(modifier = Modifier.height(4.dp))
            Text(
                text = "SCHEDULED: ${it.date}${it.time?.let { t -> " $t" } ?: ""}",
                style = MaterialTheme.typography.labelSmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }

        task.deadline?.let {
            Spacer(modifier = Modifier.height(2.dp))
            Text(
                text = "DEADLINE: ${it.date}${it.time?.let { t -> " $t" } ?: ""}",
                style = MaterialTheme.typography.labelSmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
    }
}

fun List<TextNode>.toPlainString(): String = joinToString("") { node ->
    when (node) {
        is TextNode.Plain -> node.text
        is TextNode.Link -> node.title ?: node.url
    }
}
