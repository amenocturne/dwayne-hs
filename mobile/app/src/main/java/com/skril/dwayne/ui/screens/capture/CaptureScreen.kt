package com.skril.dwayne.ui.screens.capture

import android.util.Log
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.Send
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.TaskPointer
import com.skril.dwayne.data.model.TaskWithPointer
import com.skril.dwayne.data.repository.TaskRepository
import com.skril.dwayne.ui.components.TaskCard
import com.skril.dwayne.ui.components.TaskCardInteraction
import kotlinx.coroutines.launch

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun CaptureScreen(
    repository: TaskRepository,
    onError: (String) -> Unit,
    initialText: String = "",
    initialTextRevision: Int = 0,
    onInitialTextConsumed: () -> Unit = {},
    recentRefreshKey: Any? = Unit,
    onTaskClick: (TaskPointer) -> Unit = {},
) {
    var inputText by remember { mutableStateOf("") }
    var recentCaptures by remember { mutableStateOf<List<TaskWithPointer>>(emptyList()) }
    var captureWriteRevision by remember { mutableIntStateOf(0) }
    var isCaptureInFlight by remember { mutableStateOf(false) }
    val focusRequester = remember { FocusRequester() }
    val keyboardController = LocalSoftwareKeyboardController.current

    LaunchedEffect(repository, recentRefreshKey, captureWriteRevision) {
        try {
            recentCaptures = repository.recentCaptures(limit = 3)
        } catch (t: Throwable) {
            Log.w(TAG, "Recent captures query failed", t)
        }
    }

    LaunchedEffect(Unit) {
        withFrameNanos { }
        focusRequester.requestFocus()
        keyboardController?.show()
    }

    LaunchedEffect(initialTextRevision) {
        if (initialText.isNotEmpty()) {
            Log.d(
                TAG,
                "Applying initial capture text revision=$initialTextRevision textLength=${initialText.length}",
            )
            inputText = initialText
            onInitialTextConsumed()
            withFrameNanos { }
            focusRequester.requestFocus()
            keyboardController?.show()
        }
    }
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
                modifier = Modifier
                    .weight(1f)
                    .focusRequester(focusRequester),
                placeholder = { Text("What's on your mind?") },
                singleLine = false,
                maxLines = 6,
            )
            Spacer(modifier = Modifier.width(8.dp))
            IconButton(
                onClick = {
                    if (inputText.isNotBlank() && !isCaptureInFlight) {
                        val submittedText = inputText
                        val captureTitle = submittedText.trim()
                        inputText = ""
                        isCaptureInFlight = true
                        Log.d(TAG, "Capture requested textLength=${captureTitle.length}")
                        scope.launch {
                            try {
                                val captured = repository.capture(captureTitle)
                                Log.d(
                                    TAG,
                                    "Capture success destinationFile=${captured.pointer.file} " +
                                        "pointer=${captured.pointer.file}:${captured.pointer.taskIndex}",
                                )
                                captureWriteRevision += 1
                            } catch (t: Throwable) {
                                Log.w(TAG, "Capture failed textLength=${captureTitle.length}", t)
                                if (inputText.isEmpty()) {
                                    inputText = submittedText
                                }
                                onError("Capture failed: ${t.message ?: t::class.java.simpleName}")
                            } finally {
                                isCaptureInFlight = false
                            }
                        }
                    }
                },
                enabled = inputText.isNotBlank() && !isCaptureInFlight,
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
                items(
                    items = recentCaptures,
                    key = { "${it.pointer.file}:${it.pointer.taskIndex}" },
                ) { taskWithPointer: TaskWithPointer ->
                    TaskCard(
                        task = taskWithPointer.task,
                        interaction = TaskCardInteraction.OpenDetail(taskWithPointer.pointer, onTaskClick),
                    )
                }
            }
        }
    }
}

private const val TAG = "DwayneCapture"
