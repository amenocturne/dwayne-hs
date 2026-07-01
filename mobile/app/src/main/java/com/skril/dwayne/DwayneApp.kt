package com.skril.dwayne

import android.app.Application
import androidx.lifecycle.DefaultLifecycleObserver
import androidx.lifecycle.LifecycleOwner
import androidx.lifecycle.ProcessLifecycleOwner
import com.skril.dwayne.data.events.Database
import com.skril.dwayne.data.events.EventStore
import com.skril.dwayne.data.repository.LocalTaskRepository
import com.skril.dwayne.data.repository.SettingsStore
import com.skril.dwayne.data.repository.TreeStore
import com.skril.dwayne.data.sync.SyncWorker
import com.skril.dwayne.notifications.AmbientCaptureNotification
import com.skril.dwayne.notifications.ScheduledReminderNotifications
import com.skril.dwayne.notifications.ScheduledReminderScheduler
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.launch

/**
 * App entry. Wires:
 *   - SQLDelight database
 *   - EventStore (DAO over generated queries)
 *   - LocalTaskRepository (single source of truth for the UI)
 *   - WorkManager periodic sync
 *   - foreground-trigger one-shot sync
 *
 * The repository is exposed as a property because Compose accesses it from
 * NavGraph. Initial full pull happens on first sync run if the events table
 * is empty (SyncWorker.runOnce checks count == 0 and pulls without `since`).
 */
class DwayneApp : Application() {

    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    private val inboxFileCache = MutableStateFlow(SettingsStore.DEFAULT_INBOX_FILE)

    lateinit var settingsStore: SettingsStore
        private set
    lateinit var eventStore: EventStore
        private set
    lateinit var taskRepository: LocalTaskRepository
        private set
    lateinit var treeStore: TreeStore
        private set

    override fun onCreate() {
        super.onCreate()
        AmbientCaptureNotification.ensureChannel(this)
        ScheduledReminderNotifications.ensureChannel(this)
        settingsStore = SettingsStore(this, BuildConfig.API_BASE_URL)
        treeStore = TreeStore(this)
        val db = Database.get(this)
        eventStore = EventStore(db)
        taskRepository = LocalTaskRepository(
            store = eventStore,
            inboxFileProvider = { inboxFileCache.value },
        )

        scope.launch {
            // Prime inbox cache once — it's then kept up to date by the collector below.
            val initial = settingsStore.inboxFilePath.first()
            inboxFileCache.value = initial
            settingsStore.inboxFilePath.collect { path ->
                inboxFileCache.value = path
            }
        }

        scope.launch {
            val interval = settingsStore.syncIntervalMinutes.first().toLong()
            SyncWorker.schedulePeriodic(this@DwayneApp, interval)
            SyncWorker.enqueueOneShot(this@DwayneApp)
        }

        scope.launch {
            taskRepository.state.collect { tasks ->
                ScheduledReminderScheduler.rescheduleAll(this@DwayneApp, tasks)
            }
        }

        ProcessLifecycleOwner.get().lifecycle.addObserver(object : DefaultLifecycleObserver {
            override fun onStart(owner: LifecycleOwner) {
                SyncWorker.enqueueOneShot(this@DwayneApp)
            }
        })
    }

    /** Refresh repository state from DB. Called after the sync worker has updated rows. */
    fun reloadRepositoryFromDb() {
        taskRepository.reloadFromDb()
    }
}
