package com.skril.dwayne.ui.screens.feed

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.TaskWithPointer
import com.skril.dwayne.data.repository.TaskRepository
import com.skril.dwayne.ui.components.TaskCard
import kotlinx.coroutines.launch

private val viewTabs = listOf("work-queue", "inbox", "today", "soon", "todo", "waiting", "someday", "list", "done")

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun TaskFeedScreen(
    repository: TaskRepository,
    refreshKey: Any = Unit,
    onError: (String) -> Unit,
) {
    var selectedTab by remember { mutableIntStateOf(0) }
    var tasks by remember { mutableStateOf<List<TaskWithPointer>>(emptyList()) }
    var totalCount by remember { mutableIntStateOf(0) }
    val scope = rememberCoroutineScope()

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

    LaunchedEffect(selectedTab, repository, refreshKey) {
        loadTasks(viewTabs[selectedTab])
    }

    Column(modifier = Modifier.fillMaxSize()) {
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
            LazyColumn(
                modifier = Modifier.fillMaxSize(),
                contentPadding = PaddingValues(16.dp),
                verticalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                items(tasks, key = { "${it.pointer.file}:${it.pointer.taskIndex}" }) { twp ->
                    TaskCard(task = twp.task)
                }
            }
        }
    }
}
