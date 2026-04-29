package com.skril.dwayne.ui.screens.search

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Bookmark
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.TaskWithPointer
import com.skril.dwayne.data.repository.SavedQuery
import com.skril.dwayne.data.repository.SavedQueryStore
import com.skril.dwayne.data.repository.TaskRepository
import com.skril.dwayne.ui.components.TaskCard
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.launch

private val viewFilterOptions = listOf(
    null to "All",
    "work-queue" to "work-queue",
    "inbox" to "inbox",
    "today" to "today",
    "soon" to "soon",
    "todo" to "todo",
    "waiting" to "waiting",
    "someday" to "someday",
    "list" to "list",
    "done" to "done",
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SearchScreen(
    repository: TaskRepository,
    store: SavedQueryStore,
) {
    var queryText by remember { mutableStateOf("") }
    var viewFilter by remember { mutableStateOf<String?>(null) }
    var viewMenuOpen by remember { mutableStateOf(false) }
    var results by remember { mutableStateOf<List<TaskWithPointer>>(emptyList()) }
    var hasSearched by remember { mutableStateOf(false) }
    var saveDialogOpen by remember { mutableStateOf(false) }
    var saveName by remember { mutableStateOf("") }
    var savedQueries by remember { mutableStateOf<List<SavedQuery>>(emptyList()) }
    val scope = rememberCoroutineScope()

    LaunchedEffect(Unit) {
        store.queries.collect { savedQueries = it }
    }

    fun runSearch() {
        if (queryText.isBlank()) return
        scope.launch {
            val response = repository.search(queryText.trim(), viewFilter)
            results = response.data
            hasSearched = true
        }
    }

    Column(modifier = Modifier.fillMaxSize()) {
        TopAppBar(title = { Text("Search") })

        Column(modifier = Modifier.padding(horizontal = 16.dp, vertical = 8.dp)) {
            OutlinedTextField(
                value = queryText,
                onValueChange = { queryText = it },
                modifier = Modifier.fillMaxWidth(),
                placeholder = { Text("Query (text, :tag:, etc.)") },
                singleLine = true,
            )

            Spacer(modifier = Modifier.height(8.dp))

            ExposedDropdownMenuBox(
                expanded = viewMenuOpen,
                onExpandedChange = { viewMenuOpen = it },
            ) {
                OutlinedTextField(
                    value = viewFilter ?: "All",
                    onValueChange = {},
                    readOnly = true,
                    label = { Text("In view") },
                    trailingIcon = { ExposedDropdownMenuDefaults.TrailingIcon(expanded = viewMenuOpen) },
                    modifier = Modifier
                        .fillMaxWidth()
                        .menuAnchor(MenuAnchorType.PrimaryNotEditable, true),
                )
                ExposedDropdownMenu(
                    expanded = viewMenuOpen,
                    onDismissRequest = { viewMenuOpen = false },
                ) {
                    viewFilterOptions.forEach { (value, label) ->
                        DropdownMenuItem(
                            text = { Text(label) },
                            onClick = {
                                viewFilter = value
                                viewMenuOpen = false
                            },
                        )
                    }
                }
            }

            Spacer(modifier = Modifier.height(8.dp))

            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                Button(
                    onClick = { runSearch() },
                    enabled = queryText.isNotBlank(),
                    modifier = Modifier.weight(1f),
                ) {
                    Icon(Icons.Default.Search, contentDescription = null)
                    Spacer(modifier = Modifier.width(8.dp))
                    Text("Search")
                }
                IconButton(
                    onClick = {
                        saveName = ""
                        saveDialogOpen = true
                    },
                    enabled = queryText.isNotBlank(),
                ) {
                    Icon(Icons.Default.Bookmark, contentDescription = "Save query")
                }
            }
        }

        if (savedQueries.isNotEmpty()) {
            Text(
                text = "Saved queries",
                style = MaterialTheme.typography.titleSmall,
                fontWeight = FontWeight.SemiBold,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
                modifier = Modifier.padding(horizontal = 16.dp, vertical = 4.dp),
            )
            Column(modifier = Modifier.padding(horizontal = 16.dp)) {
                savedQueries.forEach { saved ->
                    SavedQueryRow(
                        saved = saved,
                        onApply = {
                            queryText = saved.query
                            viewFilter = saved.viewFilter
                            scope.launch {
                                val response = repository.search(saved.query, saved.viewFilter)
                                results = response.data
                                hasSearched = true
                            }
                        },
                        onDelete = {
                            scope.launch { store.remove(saved.name) }
                        },
                    )
                }
            }
            HorizontalDivider(modifier = Modifier.padding(vertical = 8.dp))
        }

        if (hasSearched) {
            if (results.isEmpty()) {
                Box(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(32.dp),
                    contentAlignment = Alignment.Center,
                ) {
                    Text(
                        text = "No results",
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
                    items(results, key = { "${it.pointer.file}:${it.pointer.taskIndex}" }) { twp ->
                        TaskCard(task = twp.task)
                    }
                }
            }
        }
    }

    if (saveDialogOpen) {
        AlertDialog(
            onDismissRequest = { saveDialogOpen = false },
            title = { Text("Save query") },
            text = {
                Column {
                    Text(
                        text = "Query: ${queryText.trim()}",
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                    if (viewFilter != null) {
                        Text(
                            text = "View: $viewFilter",
                            style = MaterialTheme.typography.bodySmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                    Spacer(modifier = Modifier.height(12.dp))
                    OutlinedTextField(
                        value = saveName,
                        onValueChange = { saveName = it },
                        label = { Text("Name") },
                        singleLine = true,
                    )
                }
            },
            confirmButton = {
                TextButton(
                    enabled = saveName.isNotBlank(),
                    onClick = {
                        scope.launch {
                            store.add(SavedQuery(saveName.trim(), queryText.trim(), viewFilter))
                            saveDialogOpen = false
                        }
                    },
                ) { Text("Save") }
            },
            dismissButton = {
                TextButton(onClick = { saveDialogOpen = false }) { Text("Cancel") }
            },
        )
    }
}

@Composable
private fun SavedQueryRow(
    saved: SavedQuery,
    onApply: () -> Unit,
    onDelete: () -> Unit,
) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 2.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Column(
            modifier = Modifier
                .weight(1f)
                .padding(end = 8.dp),
        ) {
            TextButton(
                onClick = onApply,
                contentPadding = PaddingValues(horizontal = 4.dp, vertical = 4.dp),
            ) {
                Column(modifier = Modifier.fillMaxWidth()) {
                    Text(
                        text = saved.name,
                        style = MaterialTheme.typography.bodyMedium,
                        fontWeight = FontWeight.Medium,
                    )
                    Text(
                        text = buildString {
                            append(saved.query)
                            saved.viewFilter?.let { append("  · in $it") }
                        },
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
            }
        }
        IconButton(onClick = onDelete) {
            Icon(Icons.Default.Close, contentDescription = "Delete")
        }
    }
}
