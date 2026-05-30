package com.skril.dwayne.ui.screens.detail

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material.icons.filled.ArrowDropDown
import androidx.compose.material.icons.filled.Clear
import androidx.compose.material.icons.filled.DateRange
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.model.*
import com.skril.dwayne.data.repository.LocalTaskRepository
import com.skril.dwayne.ui.components.toPlainString
import com.skril.dwayne.ui.theme.keywordColor
import com.skril.dwayne.ui.theme.priorityColor
import kotlinx.coroutines.launch
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import java.util.TimeZone

private val keywordChoices = listOf(
    "INBOX", "DEFER", "TODAY", "SOON", "TODO", "PROJECT", "RELEVANT",
    "SOMEDAY", "WAITING", "NOTES", "LIST", "DONE", "TRASH",
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun TaskDetailScreen(
    repository: LocalTaskRepository,
    file: String,
    taskIndex: Int,
    onBack: () -> Unit,
    onError: (String) -> Unit,
) {
    val state by repository.state.collectAsState()
    val pointer = TaskPointer(file = file, taskIndex = taskIndex)
    val task = state[pointer]

    if (task == null) {
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
            Box(
                modifier = Modifier.fillMaxSize().padding(padding).padding(32.dp),
                contentAlignment = Alignment.Center,
            ) {
                Text("Task not found", color = MaterialTheme.colorScheme.onSurfaceVariant)
            }
        }
        return
    }

    var keyword by remember(task.todoKeyword) { mutableStateOf(task.todoKeyword) }
    var titleText by remember(task.title) { mutableStateOf(task.title.toPlainString()) }
    var descText by remember(task.description) { mutableStateOf(task.description.toPlainString()) }
    var scheduled by remember(task.scheduled) { mutableStateOf(task.scheduled) }
    var deadline by remember(task.deadline) { mutableStateOf(task.deadline) }

    val dirty = keyword != task.todoKeyword ||
        titleText != task.title.toPlainString() ||
        descText != task.description.toPlainString() ||
        scheduled != task.scheduled ||
        deadline != task.deadline

    val scope = rememberCoroutineScope()

    fun save() {
        scope.launch {
            try {
                val req = EditTaskRequest(
                    file = pointer.file,
                    taskIndex = pointer.taskIndex,
                    keyword = if (keyword != task.todoKeyword) keyword else null,
                    title = if (titleText != task.title.toPlainString()) titleText else null,
                    description = if (descText != task.description.toPlainString()) descText else null,
                    scheduled = if (scheduled != task.scheduled) ClearOrSet(scheduled) else null,
                    deadline = if (deadline != task.deadline) ClearOrSet(deadline) else null,
                )
                repository.editTask(req)
                onBack()
            } catch (t: Throwable) {
                onError("Edit failed: ${t.message ?: t::class.java.simpleName}")
            }
        }
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Task") },
                navigationIcon = {
                    IconButton(onClick = onBack) {
                        Icon(Icons.AutoMirrored.Default.ArrowBack, contentDescription = "Back")
                    }
                },
                actions = {
                    TextButton(onClick = ::save, enabled = dirty) {
                        Text("Save")
                    }
                },
            )
        },
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
                .verticalScroll(rememberScrollState())
                .padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(16.dp),
        ) {
            KeywordPicker(value = keyword, onChange = { keyword = it })

            OutlinedTextField(
                value = titleText,
                onValueChange = { titleText = it },
                label = { Text("Title") },
                modifier = Modifier.fillMaxWidth(),
                singleLine = false,
            )

            DateField(
                label = "Scheduled",
                value = scheduled,
                onChange = { scheduled = it },
            )
            DateField(
                label = "Deadline",
                value = deadline,
                onChange = { deadline = it },
            )

            if (task.tags.isNotEmpty()) {
                Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
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

            OutlinedTextField(
                value = descText,
                onValueChange = { descText = it },
                label = { Text("Description") },
                modifier = Modifier.fillMaxWidth().heightIn(min = 120.dp),
                minLines = 4,
            )

            task.priority?.let { p ->
                Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                    Text(
                        text = "Priority:",
                        style = MaterialTheme.typography.labelMedium,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                    Text(
                        text = when (p) {
                            1 -> "#A"; 2 -> "#B"; 3 -> "#C"; else -> "#$p"
                        },
                        style = MaterialTheme.typography.labelLarge,
                        color = priorityColor(p),
                    )
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
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun KeywordPicker(value: String, onChange: (String) -> Unit) {
    var expanded by remember { mutableStateOf(false) }
    ExposedDropdownMenuBox(expanded = expanded, onExpandedChange = { expanded = it }) {
        OutlinedTextField(
            value = value,
            onValueChange = {},
            readOnly = true,
            label = { Text("Keyword") },
            trailingIcon = { Icon(Icons.Default.ArrowDropDown, contentDescription = null) },
            colors = ExposedDropdownMenuDefaults.outlinedTextFieldColors(),
            modifier = Modifier
                .menuAnchor(MenuAnchorType.PrimaryEditable, true)
                .fillMaxWidth(),
            textStyle = MaterialTheme.typography.bodyLarge.copy(color = keywordColor(value)),
        )
        ExposedDropdownMenu(expanded = expanded, onDismissRequest = { expanded = false }) {
            keywordChoices.forEach { kw ->
                DropdownMenuItem(
                    text = { Text(kw, color = keywordColor(kw)) },
                    onClick = {
                        onChange(kw)
                        expanded = false
                    },
                )
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun DateField(
    label: String,
    value: OrgTime?,
    onChange: (OrgTime?) -> Unit,
) {
    var showPicker by remember { mutableStateOf(false) }
    val display = value?.let { fmtTime(it.date, it.time) } ?: ""

    OutlinedTextField(
        value = display,
        onValueChange = {},
        readOnly = true,
        label = { Text(label) },
        modifier = Modifier.fillMaxWidth(),
        trailingIcon = {
            Row {
                if (value != null) {
                    IconButton(onClick = { onChange(null) }) {
                        Icon(Icons.Default.Clear, contentDescription = "Clear $label")
                    }
                }
                IconButton(onClick = { showPicker = true }) {
                    Icon(Icons.Default.DateRange, contentDescription = "Pick $label")
                }
            }
        },
    )

    if (showPicker) {
        val initialMillis = value?.date?.let { parseDateMillis(it) }
            ?: System.currentTimeMillis()
        val state = rememberDatePickerState(initialSelectedDateMillis = initialMillis)
        DatePickerDialog(
            onDismissRequest = { showPicker = false },
            confirmButton = {
                TextButton(onClick = {
                    state.selectedDateMillis?.let { ms ->
                        onChange(OrgTime(date = formatDateUtc(ms), time = value?.time))
                    }
                    showPicker = false
                }) { Text("OK") }
            },
            dismissButton = {
                TextButton(onClick = { showPicker = false }) { Text("Cancel") }
            },
        ) {
            DatePicker(state = state)
        }
    }
}

private fun fmtTime(date: String, time: String?): String =
    if (time != null) "$date $time" else date

private fun parseDateMillis(date: String): Long? {
    val sdf = SimpleDateFormat("yyyy-MM-dd", Locale.US).apply {
        timeZone = TimeZone.getTimeZone("UTC")
    }
    return runCatching { sdf.parse(date)?.time }.getOrNull()
}

private fun formatDateUtc(millis: Long): String {
    val sdf = SimpleDateFormat("yyyy-MM-dd", Locale.US).apply {
        timeZone = TimeZone.getTimeZone("UTC")
    }
    return sdf.format(Date(millis))
}
