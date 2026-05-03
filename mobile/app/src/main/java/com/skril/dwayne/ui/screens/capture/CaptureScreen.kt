package com.skril.dwayne.ui.screens.capture

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.Send
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.TaskWithPointer
import com.skril.dwayne.data.repository.TaskRepository
import com.skril.dwayne.ui.components.TaskCard
import kotlinx.coroutines.launch

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun CaptureScreen(
    repository: TaskRepository,
    onError: (String) -> Unit,
) {
    var inputText by remember { mutableStateOf("") }
    var recentCaptures by remember { mutableStateOf<List<TaskWithPointer>>(emptyList()) }
    val scope = rememberCoroutineScope()

    Column(modifier = Modifier.fillMaxSize()) {
        TopAppBar(
            title = { Text("Capture") },
        )

        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(16.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            OutlinedTextField(
                value = inputText,
                onValueChange = { inputText = it },
                modifier = Modifier.weight(1f),
                placeholder = { Text("What's on your mind?") },
                singleLine = true,
            )
            Spacer(modifier = Modifier.width(8.dp))
            IconButton(
                onClick = {
                    if (inputText.isNotBlank()) {
                        scope.launch {
                            try {
                                val captured = repository.capture(inputText.trim())
                                recentCaptures = listOf(captured) + recentCaptures
                                inputText = ""
                            } catch (t: Throwable) {
                                onError("Capture failed: ${t.message ?: t::class.java.simpleName}")
                            }
                        }
                    }
                },
                enabled = inputText.isNotBlank(),
            ) {
                Icon(Icons.AutoMirrored.Default.Send, contentDescription = "Capture")
            }
        }


        if (recentCaptures.isNotEmpty()) {
            Text(
                text = "Recently captured",
                style = MaterialTheme.typography.titleMedium,
                modifier = Modifier.padding(horizontal = 16.dp, vertical = 8.dp),
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
            LazyColumn(
                contentPadding = PaddingValues(16.dp),
                verticalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                items(recentCaptures) { twp ->
                    TaskCard(task = twp.task)
                }
            }
        }
    }
}
