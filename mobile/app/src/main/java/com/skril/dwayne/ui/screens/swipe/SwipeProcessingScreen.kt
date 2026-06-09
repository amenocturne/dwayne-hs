package com.skril.dwayne.ui.screens.swipe

import android.content.Intent
import android.net.Uri
import androidx.compose.animation.core.animateDpAsState
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.gestures.detectDragGestures
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.OpenInNew
import androidx.compose.material.icons.filled.Tune
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.rotate
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.graphics.lerp
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskPointer
import com.skril.dwayne.data.repository.LocalTaskRepository
import com.skril.dwayne.ui.components.TaskCardDescription
import com.skril.dwayne.ui.components.TaskCardTags
import com.skril.dwayne.ui.components.TaskCardTitle
import com.skril.dwayne.ui.components.firstLinkUrl
import com.skril.dwayne.ui.components.toPlainString
import kotlinx.coroutines.launch
import kotlin.math.abs
import kotlin.math.atan2
import kotlin.math.max
import kotlin.math.roundToInt

private const val SwipeThreshold = 150f

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SwipeProcessingScreen(
    repository: LocalTaskRepository,
    tasksByPointer: Map<TaskPointer, Task>,
    onError: (String) -> Unit,
    onTaskClick: (TaskPointer) -> Unit = {},
    onEditTree: () -> Unit = {},
    tree: Branch = DefaultProcessingTree,
) {
    var filterText by rememberSaveable { mutableStateOf("keyword:INBOX") }
    var consumed by remember { mutableStateOf<Set<TaskPointer>>(emptySet()) }
    var pathStack by remember { mutableStateOf<List<Pair<Branch, Dir>>>(emptyList()) }

    val currentNode: Branch = pathStack.lastOrNull()?.let { (parent, dir) ->
        parent.childAt(dir) as Branch
    } ?: tree
    val backDir: Dir? = pathStack.lastOrNull()?.second?.opposite()

    val filteredTasks: List<Pair<TaskPointer, Task>> =
        remember(tasksByPointer, filterText, consumed) {
            tasksByPointer.asSequence()
                .filter { (ptr, task) -> ptr !in consumed && matchesFilter(task, filterText) }
                .map { (ptr, task) -> ptr to task }
                .sortedBy { (ptr, _) -> "${ptr.file}:${ptr.taskIndex}" }
                .toList()
        }

    var currentIndex by remember(filterText) { mutableIntStateOf(0) }
    val current: Pair<TaskPointer, Task>? = filteredTasks.getOrNull(currentIndex)

    var offsetX by remember { mutableFloatStateOf(0f) }
    var offsetY by remember { mutableFloatStateOf(0f) }
    val scope = rememberCoroutineScope()
    val context = LocalContext.current

    fun handleSwipe(dir: Dir) {
        offsetX = 0f
        offsetY = 0f
        when (val resolution = resolveProcessingSwipe(currentNode, backDir, dir)) {
            ProcessingSwipeResolution.Ignore -> return
            ProcessingSwipeResolution.NavigateBack -> {
                pathStack = pathStack.dropLast(1)
            }
            is ProcessingSwipeResolution.EnterBranch -> {
                pathStack = pathStack + (currentNode to dir)
            }
            is ProcessingSwipeResolution.ApplyTerminal -> {
                val (ptr, task) = current ?: return
                consumed = consumed + ptr
                pathStack = emptyList()
                scope.launch {
                    try {
                        applyProcessingModification(repository, ptr, task, resolution.terminal.modification)
                    } catch (t: Throwable) {
                        consumed = consumed - ptr
                        onError("Apply failed: ${t.message ?: t::class.java.simpleName}")
                    }
                }
            }
        }
    }

    Column(modifier = Modifier.fillMaxSize()) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 16.dp, vertical = 8.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            OutlinedTextField(
                value = filterText,
                onValueChange = { filterText = it },
                modifier = Modifier.weight(1f),
                label = { Text("Filter") },
                placeholder = { Text("e.g. keyword:INBOX, tag:music, music.youtube") },
                singleLine = true,
            )
            Spacer(modifier = Modifier.width(8.dp))
            IconButton(onClick = onEditTree) {
                Icon(Icons.Filled.Tune, contentDescription = "Edit processing tree")
            }
        }

        if (current == null) {
            Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
                Text(
                    text = if (filteredTasks.isEmpty()) "No tasks match" else "All processed",
                    style = MaterialTheme.typography.titleLarge,
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                    textAlign = TextAlign.Center,
                )
            }
            return@Column
        }

        val statusText = buildString {
            append("${currentIndex + 1}/${filteredTasks.size}")
            if (pathStack.isNotEmpty()) {
                append("  •  ")
                append(currentNode.label)
            }
        }
        Text(
            text = statusText,
            style = MaterialTheme.typography.bodyMedium,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            modifier = Modifier
                .align(Alignment.CenterHorizontally)
                .padding(bottom = 4.dp),
        )

        var dragAreaSize by remember { mutableStateOf(IntSize.Zero) }
        val cornerAngleDeg: Double = remember(dragAreaSize) {
            if (dragAreaSize.width <= 0 || dragAreaSize.height <= 0) 45.0
            else Math.toDegrees(
                atan2(dragAreaSize.height.toDouble(), dragAreaSize.width.toDouble())
            )
        }

        Box(
            modifier = Modifier
                .fillMaxWidth()
                .weight(1f)
                .onSizeChanged { dragAreaSize = it },
        ) {
            for (dir in Dir.entries) {
                if (dir == backDir) {
                    EdgeMarker(
                        dir = dir,
                        text = "BACK",
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                        active = isDirActive(offsetX, offsetY, dir, cornerAngleDeg),
                    )
                } else {
                    val child = currentNode.childAt(dir)
                    if (child != null) {
                        EdgeMarker(
                            dir = dir,
                            text = if (child is Branch) "${child.label} ›" else child.label,
                            color = child.color,
                            active = isDirActive(offsetX, offsetY, dir, cornerAngleDeg),
                        )
                    }
                }
            }

            val dominantDir = dominantDirection(offsetX, offsetY, cornerAngleDeg)
            val sectorColor: Color = when {
                dominantDir == null -> MaterialTheme.colorScheme.surface
                dominantDir == backDir -> MaterialTheme.colorScheme.surfaceVariant
                else -> currentNode.childAt(dominantDir)?.color
                    ?: MaterialTheme.colorScheme.surface
            }
            val sectorProgress =
                (max(abs(offsetX), abs(offsetY)) / SwipeThreshold).coerceIn(0f, 1f)
            val cardSurface = lerp(
                MaterialTheme.colorScheme.surface,
                sectorColor,
                sectorProgress * 0.5f,
            )

            // Compute rotation directly: tracks the finger 1:1 during drag, and
            // snaps to 0 instantly when offsetX is reset — no settling animation
            // standing between the user and the next card.
            val rotation = (offsetX / 20f).coerceIn(-15f, 15f)

            Box(
                modifier = Modifier
                    .align(Alignment.Center)
                    .offset { IntOffset(offsetX.roundToInt(), offsetY.roundToInt()) }
                    .graphicsLayer { rotationZ = rotation }
                    .pointerInput(current.first, currentNode, backDir) {
                        detectDragGestures(
                            onDragEnd = {
                                val swipeDir = directionOf(
                                    offsetX, offsetY,
                                    requireThreshold = true,
                                    cornerAngleDeg = cornerAngleDeg,
                                )
                                if (swipeDir != null) {
                                    handleSwipe(swipeDir)
                                } else {
                                    offsetX = 0f
                                    offsetY = 0f
                                }
                            },
                            onDrag = { change, dragAmount ->
                                change.consume()
                                offsetX += dragAmount.x
                                offsetY += dragAmount.y
                            },
                        )
                    }
                    .fillMaxWidth(0.75f)
                    .fillMaxHeight(0.55f)
                    .clip(RoundedCornerShape(16.dp))
                    .background(cardSurface),
            ) {
                Column(
                    modifier = Modifier
                        .fillMaxSize()
                        .clickable { onTaskClick(current.first) }
                        .padding(24.dp),
                    verticalArrangement = Arrangement.Center,
                ) {
                    TaskCardTitle(task = current.second, style = MaterialTheme.typography.titleLarge)
                    if (current.second.tags.isNotEmpty()) {
                        Spacer(modifier = Modifier.height(12.dp))
                        TaskCardTags(task = current.second, style = MaterialTheme.typography.bodyMedium)
                    }
                    Spacer(modifier = Modifier.height(16.dp))
                    TaskCardDescription(task = current.second, style = MaterialTheme.typography.bodyLarge)
                }
                val link = firstLinkUrl(current.second)
                if (link != null) {
                    IconButton(
                        onClick = {
                            try {
                                val intent = Intent(Intent.ACTION_VIEW, Uri.parse(link))
                                context.startActivity(intent)
                            } catch (t: Throwable) {
                                onError("Open link failed: ${t.message ?: t::class.java.simpleName}")
                            }
                        },
                        modifier = Modifier.align(Alignment.TopEnd).padding(4.dp),
                    ) {
                        Icon(
                            imageVector = Icons.AutoMirrored.Filled.OpenInNew,
                            contentDescription = "Open first link",
                            tint = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                }
            }
        }
    }
}

// Diagonal cones (30°) sit on the actual corner-to-center line of the drag rectangle:
// the corner direction is at angle ±cornerAngleDeg (and ±(180-cornerAngleDeg)) for a
// rectangle of width W and height H, with cornerAngleDeg = atan2(H, W). Cardinal cones
// fill the remaining angular space — on a tall phone the Up/Down cones end up narrow
// because the diagonals point steeply toward the corners.
private const val DiagonalHalfConeDeg = 15.0

private fun directionOf(
    x: Float,
    y: Float,
    requireThreshold: Boolean,
    cornerAngleDeg: Double,
): Dir? {
    if (requireThreshold) {
        if (x * x + y * y < SwipeThreshold * SwipeThreshold) return null
    } else {
        if (abs(x) < 1f && abs(y) < 1f) return null
    }
    val angle = Math.toDegrees(atan2(y.toDouble(), x.toDouble()))
    val a = cornerAngleDeg
    val h = DiagonalHalfConeDeg
    return when {
        angle in -(a - h)..(a - h) -> Dir.Right
        angle in (a - h)..(a + h) -> Dir.DownRight
        angle in (a + h)..(180.0 - a - h) -> Dir.Down
        angle in (180.0 - a - h)..(180.0 - a + h) -> Dir.DownLeft
        angle >= (180.0 - a + h) || angle <= -(180.0 - a + h) -> Dir.Left
        angle in -(180.0 - a + h)..-(180.0 - a - h) -> Dir.UpLeft
        angle in -(180.0 - a - h)..-(a + h) -> Dir.Up
        angle in -(a + h)..-(a - h) -> Dir.UpRight
        else -> null
    }
}

private fun isDirActive(
    offsetX: Float,
    offsetY: Float,
    dir: Dir,
    cornerAngleDeg: Double,
): Boolean =
    directionOf(offsetX, offsetY, requireThreshold = true, cornerAngleDeg = cornerAngleDeg) == dir

private fun dominantDirection(
    offsetX: Float,
    offsetY: Float,
    cornerAngleDeg: Double,
): Dir? =
    directionOf(offsetX, offsetY, requireThreshold = false, cornerAngleDeg = cornerAngleDeg)

@Composable
private fun BoxScope.EdgeMarker(dir: Dir, text: String, color: Color, active: Boolean) {
    when (dir) {
        Dir.Up -> TopEdgeMarker(text, color, active)
        Dir.Down -> BottomEdgeMarker(text, color, active)
        Dir.Left -> LeftEdgeMarker(text, color, active)
        Dir.Right -> RightEdgeMarker(text, color, active)
        Dir.UpLeft -> CornerMarker(text, color, active, Alignment.TopStart)
        Dir.UpRight -> CornerMarker(text, color, active, Alignment.TopEnd)
        Dir.DownLeft -> CornerMarker(text, color, active, Alignment.BottomStart)
        Dir.DownRight -> CornerMarker(text, color, active, Alignment.BottomEnd)
    }
}

@Composable
private fun BoxScope.CornerMarker(
    text: String,
    color: Color,
    active: Boolean,
    alignment: Alignment,
) {
    val alpha by animateFloatAsState(if (active) 1f else 0.55f, label = "cornerAlpha")
    Box(
        modifier = Modifier
            .align(alignment)
            .padding(8.dp)
            .clip(RoundedCornerShape(8.dp))
            .background(color.copy(alpha = alpha * 0.25f))
            .padding(horizontal = 8.dp, vertical = 4.dp),
    ) {
        Text(
            text = text,
            style = MaterialTheme.typography.labelMedium,
            color = color.copy(alpha = alpha),
        )
    }
}

@Composable
private fun BoxScope.TopEdgeMarker(text: String, color: Color, active: Boolean) {
    val alpha by animateFloatAsState(if (active) 1f else 0.55f, label = "topAlpha")
    val bandHeight by animateDpAsState(if (active) 6.dp else 3.dp, label = "topBand")

    Box(
        modifier = Modifier
            .align(Alignment.TopCenter)
            .fillMaxWidth()
            .height(bandHeight)
            .background(color.copy(alpha = alpha)),
    )
    Text(
        text = text,
        style = MaterialTheme.typography.titleMedium,
        color = color.copy(alpha = alpha),
        modifier = Modifier
            .align(Alignment.TopCenter)
            .padding(top = 14.dp),
    )
}

@Composable
private fun BoxScope.BottomEdgeMarker(text: String, color: Color, active: Boolean) {
    val alpha by animateFloatAsState(if (active) 1f else 0.55f, label = "bottomAlpha")
    val bandHeight by animateDpAsState(if (active) 6.dp else 3.dp, label = "bottomBand")

    Box(
        modifier = Modifier
            .align(Alignment.BottomCenter)
            .fillMaxWidth()
            .height(bandHeight)
            .background(color.copy(alpha = alpha)),
    )
    Text(
        text = text,
        style = MaterialTheme.typography.titleMedium,
        color = color.copy(alpha = alpha),
        modifier = Modifier
            .align(Alignment.BottomCenter)
            .padding(bottom = 14.dp),
    )
}

@Composable
private fun BoxScope.LeftEdgeMarker(text: String, color: Color, active: Boolean) {
    val alpha by animateFloatAsState(if (active) 1f else 0.55f, label = "leftAlpha")
    val bandWidth by animateDpAsState(if (active) 6.dp else 3.dp, label = "leftBand")

    Box(
        modifier = Modifier
            .align(Alignment.CenterStart)
            .fillMaxHeight()
            .width(bandWidth)
            .background(color.copy(alpha = alpha)),
    )
    Box(
        modifier = Modifier
            .align(Alignment.CenterStart)
            .padding(start = 8.dp)
            .width(24.dp)
            .fillMaxHeight(),
        contentAlignment = Alignment.Center,
    ) {
        Text(
            text = text,
            style = MaterialTheme.typography.titleMedium,
            color = color.copy(alpha = alpha),
            modifier = Modifier
                .wrapContentSize(unbounded = true)
                .rotate(-90f),
        )
    }
}

@Composable
private fun BoxScope.RightEdgeMarker(text: String, color: Color, active: Boolean) {
    val alpha by animateFloatAsState(if (active) 1f else 0.55f, label = "rightAlpha")
    val bandWidth by animateDpAsState(if (active) 6.dp else 3.dp, label = "rightBand")

    Box(
        modifier = Modifier
            .align(Alignment.CenterEnd)
            .fillMaxHeight()
            .width(bandWidth)
            .background(color.copy(alpha = alpha)),
    )
    Box(
        modifier = Modifier
            .align(Alignment.CenterEnd)
            .padding(end = 8.dp)
            .width(24.dp)
            .fillMaxHeight(),
        contentAlignment = Alignment.Center,
    ) {
        Text(
            text = text,
            style = MaterialTheme.typography.titleMedium,
            color = color.copy(alpha = alpha),
            modifier = Modifier
                .wrapContentSize(unbounded = true)
                .rotate(90f),
        )
    }
}

private fun matchesFilter(task: Task, filter: String): Boolean {
    val terms = filter.trim().split(Regex("\\s+")).filter { it.isNotEmpty() }
    if (terms.isEmpty()) return true
    return terms.all { term ->
        when {
            term.startsWith("keyword:", ignoreCase = true) -> {
                val v = term.substringAfter(":")
                task.todoKeyword.equals(v, ignoreCase = true)
            }
            term.startsWith("tag:", ignoreCase = true) -> {
                val v = term.substringAfter(":")
                task.tags.any { it.equals(v, ignoreCase = true) }
            }
            else -> {
                val text = task.title.toPlainString() + " " + task.description.toPlainString()
                text.contains(term, ignoreCase = true)
            }
        }
    }
}
