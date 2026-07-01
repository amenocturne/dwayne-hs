package com.skril.dwayne.notifications

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import com.skril.dwayne.DwayneApp
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.launch

class ScheduledReminderReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        val pendingResult = goAsync()
        when (intent.action) {
            Intent.ACTION_BOOT_COMPLETED -> {
                rescheduleFromApp(context, pendingResult)
            }

            else -> {
                val reminder = ScheduledReminderScheduler.reminderFromIntent(intent)
                if (reminder == null) {
                    pendingResult.finish()
                    return
                }
                ScheduledReminderNotifications.showIfPermitted(context, reminder)
                rescheduleFromApp(context, pendingResult)
            }
        }
    }

    private fun rescheduleFromApp(context: Context, pendingResult: PendingResult) {
        val app = context.applicationContext as? DwayneApp
        if (app == null) {
            pendingResult.finish()
            return
        }
        CoroutineScope(Dispatchers.IO).launch {
            try {
                ScheduledReminderScheduler.rescheduleAll(
                    context = context,
                    tasks = app.taskRepository.state.value,
                    dateOnlyReminderTime = app.settingsStore.scheduledDateOnlyReminderTime.first(),
                )
            } finally {
                pendingResult.finish()
            }
        }
    }
}
