package com.skril.dwayne.ui.screens.swipe

import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.foundation.background
import androidx.compose.foundation.gestures.detectDragGestures
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.TaskWithPointer
import com.skril.dwayne.data.model.TextNode
import com.skril.dwayne.data.repository.TaskRepository
import com.skril.dwayne.ui.components.TaskCard
import com.skril.dwayne.ui.components.toPlainString
import com.skril.dwayne.ui.theme.*
import kotlinx.coroutines.launch
import kotlin.math.absoluteValue
import kotlin.math.roundToInt

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SwipeProcessingScreen(
    repository: TaskRepository,
    refreshKey: Any = Unit,
    onError: (String) -> Unit,
) {
    var inboxTasks by remember { mutableStateOf<List<TaskWithPointer>>(emptyList()) }
    var currentIndex by remember { mutableIntStateOf(0) }
    var offsetX by remember { mutableFloatStateOf(0f) }
    var offsetY by remember { mutableFloatStateOf(0f) }
    val scope = rememberCoroutineScope()

    LaunchedEffect(repository, refreshKey) {
        try {
            val response = repository.getView("inbox")
            inboxTasks = response.data
            currentIndex = 0
        } catch (t: Throwable) {
            onError("Load inbox failed: ${t.message ?: t::class.java.simpleName}")
        }
    }

    val currentTask = inboxTasks.getOrNull(currentIndex)

    fun processSwipe(keyword: String) {
        val task = currentTask ?: return
        scope.launch {
            try {
                repository.changeKeyword(task.pointer, keyword)
                currentIndex++
                offsetX = 0f
                offsetY = 0f
            } catch (t: Throwable) {
                onError("Change keyword failed: ${t.message ?: t::class.java.simpleName}")
            }
        }
    }

    Column(modifier = Modifier.fillMaxSize()) {
        TopAppBar(
            title = { Text("Process Inbox") },
            actions = {
                if (inboxTasks.isNotEmpty()) {
                    Text(
                        text = "${currentIndex + 1}/${inboxTasks.size}",
                        style = MaterialTheme.typography.bodyMedium,
                        modifier = Modifier.padding(end = 16.dp),
                    )
                }
            },
        )

        if (currentTask == null) {
            Box(
                modifier = Modifier.fillMaxSize(),
                contentAlignment = Alignment.Center,
            ) {
                Text(
                    text = if (inboxTasks.isEmpty()) "Inbox is empty" else "All done!",
                    style = MaterialTheme.typography.titleLarge,
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                    textAlign = TextAlign.Center,
                )
            }
        } else {
            Box(
                modifier = Modifier
                    .fillMaxSize()
                    .padding(16.dp),
                contentAlignment = Alignment.Center,
            ) {
                // Swipe hints
                val swipeThreshold = 150f
                val showLeftHint = offsetX < -swipeThreshold / 2
                val showRightHint = offsetX > swipeThreshold / 2
                val showUpHint = offsetY < -swipeThreshold / 2

                if (showLeftHint) {
                    Text(
                        text = "SOMEDAY",
                        style = MaterialTheme.typography.titleLarge,
                        color = KeywordWaiting,
                        modifier = Modifier
                            .align(Alignment.CenterStart)
                            .padding(start = 8.dp),
                    )
                }
                if (showRightHint) {
                    Text(
                        text = "TODO",
                        style = MaterialTheme.typography.titleLarge,
                        color = KeywordTodo,
                        modifier = Modifier
                            .align(Alignment.CenterEnd)
                            .padding(end = 8.dp),
                    )
                }
                if (showUpHint) {
                    Text(
                        text = "TODAY",
                        style = MaterialTheme.typography.titleLarge,
                        color = KeywordToday,
                        modifier = Modifier
                            .align(Alignment.TopCenter)
                            .padding(top = 32.dp),
                    )
                }

                // Draggable card
                val rotation by animateFloatAsState(
                    targetValue = (offsetX / 20f).coerceIn(-15f, 15f),
                    label = "rotation",
                )

                Box(
                    modifier = Modifier
                        .offset { IntOffset(offsetX.roundToInt(), offsetY.roundToInt()) }
                        .graphicsLayer { rotationZ = rotation }
                        .pointerInput(currentIndex) {
                            detectDragGestures(
                                onDragEnd = {
                                    when {
                                        offsetX < -swipeThreshold -> processSwipe("SOMEDAY")
                                        offsetX > swipeThreshold -> processSwipe("TODO")
                                        offsetY < -swipeThreshold -> processSwipe("TODAY")
                                        else -> {
                                            offsetX = 0f
                                            offsetY = 0f
                                        }
                                    }
                                },
                                onDrag = { change, dragAmount ->
                                    change.consume()
                                    offsetX += dragAmount.x
                                    offsetY += dragAmount.y
                                },
                            )
                        }
                        .fillMaxWidth(0.9f)
                        .fillMaxHeight(0.6f),
                ) {
                    Column(
                        modifier = Modifier
                            .fillMaxSize()
                            .clip(RoundedCornerShape(16.dp))
                            .background(MaterialTheme.colorScheme.surface)
                            .padding(24.dp),
                        verticalArrangement = Arrangement.Center,
                    ) {
                        Text(
                            text = currentTask.task.title.toPlainString(),
                            style = MaterialTheme.typography.titleLarge,
                            color = MaterialTheme.colorScheme.onSurface,
                        )

                        if (currentTask.task.tags.isNotEmpty()) {
                            Spacer(modifier = Modifier.height(12.dp))
                            Text(
                                text = currentTask.task.tags.joinToString(" ") { ":$it:" },
                                style = MaterialTheme.typography.bodyMedium,
                                color = MaterialTheme.colorScheme.onSurfaceVariant,
                            )
                        }

                        val desc = currentTask.task.description.toPlainString()
                        if (desc.isNotBlank()) {
                            Spacer(modifier = Modifier.height(16.dp))
                            Text(
                                text = desc,
                                style = MaterialTheme.typography.bodyLarge,
                                color = MaterialTheme.colorScheme.onSurfaceVariant,
                            )
                        }
                    }
                }

                // Direction labels at bottom
                Row(
                    modifier = Modifier
                        .align(Alignment.BottomCenter)
                        .fillMaxWidth()
                        .padding(bottom = 16.dp),
                    horizontalArrangement = Arrangement.SpaceEvenly,
                ) {
                    Text("← SOMEDAY", style = MaterialTheme.typography.labelSmall, color = KeywordWaiting)
                    Text("↑ TODAY", style = MaterialTheme.typography.labelSmall, color = KeywordToday)
                    Text("TODO →", style = MaterialTheme.typography.labelSmall, color = KeywordTodo)
                }
            }
        }
    }
}
