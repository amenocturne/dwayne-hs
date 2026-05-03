package com.skril.dwayne.data.sync

import android.content.Context
import androidx.work.Constraints
import androidx.work.CoroutineWorker
import androidx.work.ExistingPeriodicWorkPolicy
import androidx.work.ExistingWorkPolicy
import androidx.work.NetworkType
import androidx.work.OneTimeWorkRequestBuilder
import androidx.work.PeriodicWorkRequestBuilder
import androidx.work.WorkManager
import androidx.work.WorkerParameters
import com.skril.dwayne.data.events.Database
import com.skril.dwayne.data.events.EventStore
import com.skril.dwayne.data.repository.SettingsStore
import kotlinx.coroutines.flow.first
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.builtins.serializer
import kotlinx.serialization.json.Json
import java.util.concurrent.TimeUnit

/**
 * WorkManager job: pulls events from the server and pushes local writes back.
 *
 * Triggered by:
 *   - periodic schedule (interval from settings)
 *   - app foreground (ProcessLifecycleOwner observer in DwayneApp)
 *   - "Force sync now" button in Settings
 */
class SyncWorker(
    appContext: Context,
    params: WorkerParameters,
) : CoroutineWorker(appContext, params) {

    private val json = Json { ignoreUnknownKeys = true; isLenient = true }
    private val stringListSerializer = ListSerializer(String.serializer())

    override suspend fun doWork(): Result {
        val context = applicationContext
        val settings = SettingsStore(context, defaultApiBaseUrl = "")
        val api = settings.apiSettings.first()
        val inboxFile = settings.inboxFilePath.first()
        val pullWindow = settings.pullWindowHours.first()

        if (api.baseUrl.isBlank()) return Result.success()

        val store = EventStore(Database.get(context))

        val owned = (loadOwnedFiles(store) ?: listOf(inboxFile)).filter { it.isNotBlank() }
        val sync = SyncApi(api.baseUrl, api.username, api.password)
        return try {
            val isInitial = store.count() == 0L
            val engine = SyncEngine(
                store = store,
                api = sync,
                ownedFiles = owned,
                pullWindowHours = pullWindow,
            )
            engine.runOnce(initialFullPull = isInitial)
            // Notify the app so the in-memory projection reloads from DB.
            (context as? com.skril.dwayne.DwayneApp)?.reloadRepositoryFromDb()
            Result.success()
        } catch (t: Throwable) {
            android.util.Log.w(TAG, "Sync failed", t)
            Result.retry()
        } finally {
            sync.close()
        }
    }

    private fun loadOwnedFiles(store: EventStore): List<String>? {
        val raw = store.getSyncState(EventStore.KEY_OWNED_FILES) ?: return null
        return runCatching { json.decodeFromString(stringListSerializer, raw) }.getOrNull()
    }

    companion object {
        private const val TAG = "SyncWorker"
        const val PERIODIC_WORK_NAME = "dwayne-sync-periodic"
        const val ONE_SHOT_WORK_NAME = "dwayne-sync-oneshot"

        fun enqueueOneShot(context: Context) {
            val request = OneTimeWorkRequestBuilder<SyncWorker>()
                .setConstraints(networkConstraints())
                .build()
            WorkManager.getInstance(context.applicationContext).enqueueUniqueWork(
                ONE_SHOT_WORK_NAME,
                ExistingWorkPolicy.REPLACE,
                request,
            )
        }

        fun schedulePeriodic(context: Context, intervalMinutes: Long) {
            // WorkManager minimum periodic interval is 15 minutes; clamp.
            val effective = intervalMinutes.coerceAtLeast(15L)
            val request = PeriodicWorkRequestBuilder<SyncWorker>(effective, TimeUnit.MINUTES)
                .setConstraints(networkConstraints())
                .build()
            WorkManager.getInstance(context.applicationContext).enqueueUniquePeriodicWork(
                PERIODIC_WORK_NAME,
                ExistingPeriodicWorkPolicy.UPDATE,
                request,
            )
        }

        private fun networkConstraints(): Constraints =
            Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build()
    }
}
