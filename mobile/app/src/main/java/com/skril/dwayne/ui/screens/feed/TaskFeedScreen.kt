package com.skril.dwayne.ui.screens.feed

import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.foundation.background
import androidx.compose.foundation.gestures.detectDragGesturesAfterLongPress
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.hapticfeedback.HapticFeedbackType
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalHapticFeedback
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.TaskWithPointer
import com.skril.dwayne.data.repository.TaskRepository
import com.skril.dwayne.ui.components.TaskCard
import com.skril.dwayne.ui.theme.keywordColor
import kotlinx.coroutines.launch

private val viewTabs = listOf("work-queue", "inbox", "today", "soon", "todo", "waiting", "someday", "list", "done")

private const val TodoViewName = "todo"

private data class FeedSwipeAction(
    val label: String,
    val keyword: String,
    val color: Color,
)

private data class FeedSwipeActions(
    val left: FeedSwipeAction,
    val right: FeedSwipeAction,
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun TaskFeedScreen(
    repository: TaskRepository,
    refreshKey: Any = Unit,
    onError: (String) -> Unit,
    onTaskClick: (com.skril.dwayne.data.model.TaskPointer) -> Unit = {},
) {
    var selectedTab by rememberSaveable { mutableIntStateOf(0) }
    var tasks by remember { mutableStateOf<List<TaskWithPointer>>(emptyList()) }
    var totalCount by remember { mutableIntStateOf(0) }
    val listState = rememberLazyListState()
    val scope = rememberCoroutineScope()
    val snackbarHostState = remember { SnackbarHostState() }

    fun loadTasks(viewName: String) {
        scope.launch {
            try {
                val response = repository.getView(viewName)
                tasks = response.data
                totalCount = response.metadata.total
            } catch (t: Throwable) {
                tasks = emptyList()
                totalCount = 0
                onError("Load $viewName failed: ${t.message ?: t::class.java.simpleName}")
            }
        }
    }

    fun changeTaskKeyword(twp: TaskWithPointer, keyword: String) {
        val viewName = viewTabs[selectedTab]
        val previousKeyword = twp.task.todoKeyword
        scope.launch {
            try {
                repository.changeKeyword(twp.pointer, keyword)
                loadTasks(viewName)
                val result = snackbarHostState.showSnackbar(
                    message = "Moved to $keyword",
                    actionLabel = "Undo",
                    withDismissAction = true,
                )
                if (result == SnackbarResult.ActionPerformed) {
                    repository.changeKeyword(twp.pointer, previousKeyword)
                    loadTasks(viewName)
                }
            } catch (t: Throwable) {
                onError("Move to $keyword failed: ${t.message ?: t::class.java.simpleName}")
                loadTasks(viewName)
            }
        }
    }

    LaunchedEffect(selectedTab, repository, refreshKey) {
        loadTasks(viewTabs[selectedTab])
    }

    Scaffold(
        snackbarHost = { SnackbarHost(snackbarHostState) },
    ) { scaffoldPadding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(scaffoldPadding),
        ) {
            TopAppBar(
                title = { Text("Dwayne") },
            )

            ScrollableTabRow(
                selectedTabIndex = selectedTab,
                edgePadding = 16.dp,
            ) {
                viewTabs.forEachIndexed { index, tab ->
                    Tab(
                        selected = selectedTab == index,
                        onClick = { selectedTab = index },
                        text = { Text(tab.uppercase()) },
                    )
                }
            }

            if (tasks.isEmpty()) {
                Box(
                    modifier = Modifier
                        .fillMaxSize()
                        .padding(32.dp),
                ) {
                    Text(
                        text = "No tasks",
                        style = MaterialTheme.typography.bodyLarge,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
            } else {
                val swipeActions = swipeActionsForView(viewTabs[selectedTab])
                LazyColumn(
                    state = listState,
                    modifier = Modifier.fillMaxSize(),
                    contentPadding = PaddingValues(16.dp),
                    verticalArrangement = Arrangement.spacedBy(8.dp),
                ) {
                    items(tasks, key = { "${it.pointer.file}:${it.pointer.taskIndex}" }) { twp ->
                        if (swipeActions == null) {
                            TaskCard(task = twp.task, onClick = { onTaskClick(twp.pointer) })
                        } else {
                            SwipeableTaskCard(
                                taskWithPointer = twp,
                                actions = swipeActions,
                                onClick = { onTaskClick(twp.pointer) },
                                onAction = { action -> changeTaskKeyword(twp, action.keyword) },
                            )
                        }
                    }
                }
            }
        }
    }
}

@Composable
private fun SwipeableTaskCard(
    taskWithPointer: TaskWithPointer,
    actions: FeedSwipeActions,
    onClick: () -> Unit,
    onAction: (FeedSwipeAction) -> Unit,
) {
    val density = LocalDensity.current
    val haptics = LocalHapticFeedback.current
    val thresholdPx = with(density) { 96.dp.toPx() }
    var dragOffset by remember(taskWithPointer.pointer) { mutableFloatStateOf(0f) }
    var hasCrossedThreshold by remember(taskWithPointer.pointer) { mutableStateOf(false) }
    val displayOffset by animateFloatAsState(dragOffset, label = "feedSwipeOffset")
    val activeAction = when {
        dragOffset <= -thresholdPx -> actions.left
        dragOffset >= thresholdPx -> actions.right
        else -> null
    }

    LaunchedEffect(activeAction) {
        val crossed = activeAction != null
        if (crossed && !hasCrossedThreshold) {
            haptics.performHapticFeedback(HapticFeedbackType.LongPress)
        }
        hasCrossedThreshold = crossed
    }

    Box(
        modifier = Modifier
            .fillMaxWidth()
            .clip(MaterialTheme.shapes.medium),
    ) {
        SwipeActionBackground(
            modifier = Modifier.matchParentSize(),
            actions = actions,
            activeAction = activeAction,
        )
        TaskCard(
            task = taskWithPointer.task,
            onClick = onClick,
            modifier = Modifier
                .graphicsLayer {
                    translationX = displayOffset
                }
                .pointerInput(taskWithPointer.pointer, actions) {
                    detectDragGesturesAfterLongPress(
                        onDragStart = {
                            haptics.performHapticFeedback(HapticFeedbackType.LongPress)
                        },
                        onDragEnd = {
                            val action = when {
                                dragOffset <= -thresholdPx -> actions.left
                                dragOffset >= thresholdPx -> actions.right
                                else -> null
                            }
                            dragOffset = 0f
                            hasCrossedThreshold = false
                            if (action != null) {
                                onAction(action)
                            }
                        },
                        onDragCancel = {
                            dragOffset = 0f
                            hasCrossedThreshold = false
                        },
                        onDrag = { change, dragAmount ->
                            change.consume()
                            dragOffset = (dragOffset + dragAmount.x)
                                .coerceIn(-thresholdPx * 1.35f, thresholdPx * 1.35f)
                        },
                    )
                },
        )
    }
}

@Composable
private fun SwipeActionBackground(
    modifier: Modifier = Modifier,
    actions: FeedSwipeActions,
    activeAction: FeedSwipeAction?,
) {
    Row(modifier = modifier) {
        SwipeActionSlot(
            action = actions.left,
            active = activeAction == actions.left,
            alignment = Alignment.CenterStart,
            modifier = Modifier.weight(1f),
        )
        Box(
            modifier = Modifier
                .width(64.dp)
                .fillMaxHeight()
                .background(MaterialTheme.colorScheme.surfaceVariant.copy(alpha = 0.35f)),
            contentAlignment = Alignment.Center,
        ) {
            Text(
                text = "KEEP",
                style = MaterialTheme.typography.labelSmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
                fontWeight = FontWeight.SemiBold,
            )
        }
        SwipeActionSlot(
            action = actions.right,
            active = activeAction == actions.right,
            alignment = Alignment.CenterEnd,
            modifier = Modifier.weight(1f),
        )
    }
}

@Composable
private fun SwipeActionSlot(
    action: FeedSwipeAction,
    active: Boolean,
    alignment: Alignment,
    modifier: Modifier = Modifier,
) {
    Box(
        modifier = modifier
            .fillMaxHeight()
            .background(action.color.copy(alpha = if (active) 0.48f else 0.22f))
            .padding(horizontal = 18.dp),
        contentAlignment = alignment,
    ) {
        Text(
            text = action.label,
            style = MaterialTheme.typography.labelLarge,
            color = action.color,
            fontWeight = FontWeight.Bold,
        )
    }
}

private fun swipeActionsForView(viewName: String): FeedSwipeActions? =
    when (viewName) {
        TodoViewName -> FeedSwipeActions(
            left = FeedSwipeAction(
                label = "SOMEDAY",
                keyword = "SOMEDAY",
                color = keywordColor("SOMEDAY"),
            ),
            right = FeedSwipeAction(
                label = "DONE",
                keyword = "DONE",
                color = keywordColor("DONE"),
            ),
        )
        else -> null
    }
