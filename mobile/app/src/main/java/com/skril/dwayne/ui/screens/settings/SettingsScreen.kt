package com.skril.dwayne.ui.screens.settings

import android.content.Context
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Visibility
import androidx.compose.material.icons.filled.VisibilityOff
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.repository.SettingsStore
import com.skril.dwayne.data.sync.SyncWorker
import kotlinx.coroutines.launch

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SettingsScreen(store: SettingsStore, defaultApiBaseUrl: String) {
    val context = LocalContext.current
    val currentUrl by store.apiBaseUrl.collectAsState(initial = defaultApiBaseUrl)
    val currentUsername by store.apiUsername.collectAsState(initial = SettingsStore.DEFAULT_USERNAME)
    val currentPassword by store.apiPassword.collectAsState(initial = "")
    val currentInboxFile by store.inboxFilePath.collectAsState(initial = SettingsStore.DEFAULT_INBOX_FILE)
    val currentSyncInterval by store.syncIntervalMinutes.collectAsState(initial = SettingsStore.DEFAULT_SYNC_INTERVAL_MIN)
    val currentPullWindow by store.pullWindowHours.collectAsState(initial = SettingsStore.DEFAULT_PULL_WINDOW_HOURS)
    val currentDateOnlyReminderTime by store.scheduledDateOnlyReminderTime.collectAsState(
        initial = SettingsStore.DEFAULT_SCHEDULED_DATE_ONLY_REMINDER_TIME,
    )

    var urlDraft by remember(currentUrl) { mutableStateOf(currentUrl) }
    var usernameDraft by remember(currentUsername) { mutableStateOf(currentUsername) }
    var passwordDraft by remember(currentPassword) { mutableStateOf(currentPassword) }
    var inboxDraft by remember(currentInboxFile) { mutableStateOf(currentInboxFile) }
    var syncIntervalDraft by remember(currentSyncInterval) { mutableStateOf(currentSyncInterval.toString()) }
    var pullWindowDraft by remember(currentPullWindow) { mutableStateOf(currentPullWindow.toString()) }
    var dateOnlyReminderTimeDraft by remember(currentDateOnlyReminderTime) {
        mutableStateOf(currentDateOnlyReminderTime)
    }
    var passwordVisible by remember { mutableStateOf(false) }

    val scope = rememberCoroutineScope()
    var savedNotice by remember { mutableStateOf<String?>(null) }

    Column(modifier = Modifier.fillMaxSize()) {
        TopAppBar(title = { Text("Settings") })

        Column(
            modifier = Modifier
                .fillMaxWidth()
                .verticalScroll(rememberScrollState())
                .padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text("API base URL", style = MaterialTheme.typography.titleMedium)
            OutlinedTextField(
                value = urlDraft,
                onValueChange = { urlDraft = it },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
                placeholder = { Text(defaultApiBaseUrl) },
                supportingText = { Text("Default: $defaultApiBaseUrl") },
            )

            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                Button(
                    onClick = {
                        scope.launch {
                            store.setApiBaseUrl(urlDraft)
                            savedNotice = "URL saved"
                        }
                    },
                    enabled = urlDraft.isNotBlank() && urlDraft != currentUrl,
                ) { Text("Save") }

                OutlinedButton(
                    onClick = {
                        scope.launch {
                            store.resetApiBaseUrl()
                            urlDraft = defaultApiBaseUrl
                            savedNotice = "URL reset"
                        }
                    },
                ) { Text("Reset URL") }
            }

            HorizontalDivider()

            Text("API auth (HTTP Basic)", style = MaterialTheme.typography.titleMedium)
            OutlinedTextField(
                value = usernameDraft,
                onValueChange = { usernameDraft = it },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
                label = { Text("Username") },
            )
            OutlinedTextField(
                value = passwordDraft,
                onValueChange = { passwordDraft = it },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
                label = { Text("Password") },
                visualTransformation = if (passwordVisible) VisualTransformation.None else PasswordVisualTransformation(),
                keyboardOptions = KeyboardOptions(autoCorrectEnabled = false),
                trailingIcon = {
                    IconButton(onClick = { passwordVisible = !passwordVisible }) {
                        val icon = if (passwordVisible) Icons.Default.VisibilityOff else Icons.Default.Visibility
                        Icon(icon, contentDescription = if (passwordVisible) "Hide" else "Show")
                    }
                },
            )

            Button(
                onClick = {
                    scope.launch {
                        store.setApiUsername(usernameDraft)
                        store.setApiPassword(passwordDraft)
                        savedNotice = "Auth saved"
                    }
                },
                enabled = usernameDraft != currentUsername || passwordDraft != currentPassword,
            ) { Text("Save auth") }

            HorizontalDivider()

            Text("Sync", style = MaterialTheme.typography.titleMedium)

            OutlinedTextField(
                value = inboxDraft,
                onValueChange = { inboxDraft = it },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
                label = { Text("Inbox file") },
                supportingText = { Text("Captures land here. This phone owns this file.") },
            )

            OutlinedTextField(
                value = syncIntervalDraft,
                onValueChange = { syncIntervalDraft = it.filter { c -> c.isDigit() } },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
                label = { Text("Sync interval (minutes)") },
                supportingText = { Text("Background sync. WorkManager floor is 15 min.") },
                keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Number),
            )

            OutlinedTextField(
                value = pullWindowDraft,
                onValueChange = { pullWindowDraft = it.filter { c -> c.isDigit() } },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
                label = { Text("Pull window (hours)") },
                supportingText = { Text("Re-pull events from N hours before last sync. Tolerates clock skew.") },
                keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Number),
            )

            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                Button(
                    onClick = {
                        scope.launch {
                            store.setInboxFilePath(inboxDraft)
                            val intervalInt = syncIntervalDraft.toIntOrNull() ?: SettingsStore.DEFAULT_SYNC_INTERVAL_MIN
                            val pullInt = pullWindowDraft.toIntOrNull() ?: SettingsStore.DEFAULT_PULL_WINDOW_HOURS
                            store.setSyncIntervalMinutes(intervalInt)
                            store.setPullWindowHours(pullInt)
                            // Reschedule periodic work with new interval.
                            SyncWorker.schedulePeriodic(context, intervalInt.toLong())
                            savedNotice = "Sync settings saved"
                        }
                    },
                ) { Text("Save sync") }

                OutlinedButton(
                    onClick = {
                        SyncWorker.enqueueOneShot(context)
                        savedNotice = "Sync queued"
                    },
                ) { Text("Force sync now") }
            }

            HorizontalDivider()

            Text("Reminders", style = MaterialTheme.typography.titleMedium)

            OutlinedTextField(
                value = dateOnlyReminderTimeDraft,
                onValueChange = { dateOnlyReminderTimeDraft = sanitizeHourMinuteDraft(it) },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
                label = { Text("Date-only scheduled time") },
                supportingText = { Text("Used for scheduled tasks that have a date but no explicit time.") },
                keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Number),
                isError = dateOnlyReminderTimeDraft.isNotBlank() &&
                    !SettingsStore.isValidHourMinute(dateOnlyReminderTimeDraft),
            )

            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                Button(
                    onClick = {
                        scope.launch {
                            store.setScheduledDateOnlyReminderTime(dateOnlyReminderTimeDraft)
                            savedNotice = "Reminder settings saved"
                        }
                    },
                    enabled = SettingsStore.isValidHourMinute(dateOnlyReminderTimeDraft) &&
                        dateOnlyReminderTimeDraft != currentDateOnlyReminderTime,
                ) { Text("Save reminders") }

                OutlinedButton(
                    onClick = {
                        dateOnlyReminderTimeDraft = SettingsStore.DEFAULT_SCHEDULED_DATE_ONLY_REMINDER_TIME
                    },
                ) { Text("Default") }
            }

            savedNotice?.let { Text(it, style = MaterialTheme.typography.bodySmall) }
        }
    }
}

private fun sanitizeHourMinuteDraft(value: String): String {
    val digits = value.filter { it.isDigit() }.take(4)
    return if (digits.length <= 2) digits else "${digits.take(2)}:${digits.drop(2)}"
}
