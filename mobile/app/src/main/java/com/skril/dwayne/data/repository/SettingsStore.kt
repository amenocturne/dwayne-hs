package com.skril.dwayne.data.repository

import android.content.Context
import androidx.datastore.preferences.core.edit
import androidx.datastore.preferences.core.intPreferencesKey
import androidx.datastore.preferences.core.stringPreferencesKey
import androidx.datastore.preferences.preferencesDataStore
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map

private val Context.settingsDataStore by preferencesDataStore(name = "settings")

data class ApiSettings(val baseUrl: String, val username: String, val password: String)

class SettingsStore(context: Context, private val defaultApiBaseUrl: String) {

    private val dataStore = context.applicationContext.settingsDataStore

    val apiBaseUrl: Flow<String> = dataStore.data.map { prefs ->
        prefs[API_BASE_URL_KEY]?.takeIf { it.isNotBlank() } ?: defaultApiBaseUrl
    }

    val apiUsername: Flow<String> = dataStore.data.map { prefs ->
        prefs[API_USERNAME_KEY] ?: DEFAULT_USERNAME
    }

    val apiPassword: Flow<String> = dataStore.data.map { prefs ->
        prefs[API_PASSWORD_KEY] ?: ""
    }

    val apiSettings: Flow<ApiSettings> = dataStore.data.map { prefs ->
        ApiSettings(
            baseUrl = prefs[API_BASE_URL_KEY]?.takeIf { it.isNotBlank() } ?: defaultApiBaseUrl,
            username = prefs[API_USERNAME_KEY] ?: DEFAULT_USERNAME,
            password = prefs[API_PASSWORD_KEY] ?: "",
        )
    }

    val inboxFilePath: Flow<String> = dataStore.data.map { prefs ->
        prefs[INBOX_FILE_KEY]?.takeIf { it.isNotBlank() } ?: DEFAULT_INBOX_FILE
    }

    val syncIntervalMinutes: Flow<Int> = dataStore.data.map { prefs ->
        (prefs[SYNC_INTERVAL_KEY] ?: DEFAULT_SYNC_INTERVAL_MIN).coerceAtLeast(1)
    }

    val pullWindowHours: Flow<Int> = dataStore.data.map { prefs ->
        (prefs[PULL_WINDOW_KEY] ?: DEFAULT_PULL_WINDOW_HOURS).coerceAtLeast(0)
    }

    val scheduledDateOnlyReminderTime: Flow<String> = dataStore.data.map { prefs ->
        prefs[SCHEDULED_DATE_ONLY_REMINDER_TIME_KEY]
            ?.takeIf { isValidHourMinute(it) }
            ?: DEFAULT_SCHEDULED_DATE_ONLY_REMINDER_TIME
    }

    suspend fun setApiBaseUrl(url: String) {
        dataStore.edit { prefs ->
            val trimmed = url.trim()
            if (trimmed.isEmpty()) prefs.remove(API_BASE_URL_KEY)
            else prefs[API_BASE_URL_KEY] = trimmed
        }
    }

    suspend fun resetApiBaseUrl() {
        dataStore.edit { prefs -> prefs.remove(API_BASE_URL_KEY) }
    }

    suspend fun setApiUsername(username: String) {
        dataStore.edit { prefs ->
            val trimmed = username.trim()
            if (trimmed.isEmpty()) prefs.remove(API_USERNAME_KEY)
            else prefs[API_USERNAME_KEY] = trimmed
        }
    }

    suspend fun setApiPassword(password: String) {
        dataStore.edit { prefs ->
            val trimmed = password.trim()
            if (trimmed.isEmpty()) prefs.remove(API_PASSWORD_KEY)
            else prefs[API_PASSWORD_KEY] = trimmed
        }
    }

    suspend fun setInboxFilePath(path: String) {
        dataStore.edit { prefs ->
            val trimmed = path.trim()
            if (trimmed.isEmpty()) prefs.remove(INBOX_FILE_KEY)
            else prefs[INBOX_FILE_KEY] = trimmed
        }
    }

    suspend fun setSyncIntervalMinutes(value: Int) {
        dataStore.edit { prefs ->
            prefs[SYNC_INTERVAL_KEY] = value.coerceAtLeast(1)
        }
    }

    suspend fun setPullWindowHours(value: Int) {
        dataStore.edit { prefs ->
            prefs[PULL_WINDOW_KEY] = value.coerceAtLeast(0)
        }
    }

    suspend fun setScheduledDateOnlyReminderTime(value: String) {
        dataStore.edit { prefs ->
            val trimmed = value.trim()
            if (trimmed.isEmpty()) {
                prefs.remove(SCHEDULED_DATE_ONLY_REMINDER_TIME_KEY)
            } else if (isValidHourMinute(trimmed)) {
                prefs[SCHEDULED_DATE_ONLY_REMINDER_TIME_KEY] = trimmed
            }
        }
    }

    companion object {
        const val DEFAULT_USERNAME = ""
        const val DEFAULT_INBOX_FILE = "Inbox.org"
        // Note: WorkManager enforces a 15-minute floor for periodic work.
        // Setting this lower has no effect on the periodic schedule. The
        // foreground-trigger / "Force sync now" path is unaffected.
        const val DEFAULT_SYNC_INTERVAL_MIN = 5
        const val DEFAULT_PULL_WINDOW_HOURS = 24
        const val DEFAULT_SCHEDULED_DATE_ONLY_REMINDER_TIME = "09:00"

        private val API_BASE_URL_KEY = stringPreferencesKey("api_base_url")
        private val API_USERNAME_KEY = stringPreferencesKey("api_username")
        private val API_PASSWORD_KEY = stringPreferencesKey("api_password")
        private val INBOX_FILE_KEY = stringPreferencesKey("inbox_file_path")
        private val SYNC_INTERVAL_KEY = intPreferencesKey("sync_interval_minutes")
        private val PULL_WINDOW_KEY = intPreferencesKey("pull_window_hours")
        private val SCHEDULED_DATE_ONLY_REMINDER_TIME_KEY =
            stringPreferencesKey("scheduled_date_only_reminder_time")

        fun isValidHourMinute(value: String): Boolean {
            val parts = value.split(":")
            if (parts.size != 2) return false
            val hour = parts[0].toIntOrNull() ?: return false
            val minute = parts[1].toIntOrNull() ?: return false
            return parts[0].length == 2 &&
                parts[1].length == 2 &&
                hour in 0..23 &&
                minute in 0..59
        }
    }
}
