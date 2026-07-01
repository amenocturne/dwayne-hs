package com.skril.dwayne.notifications

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import com.skril.dwayne.DwayneApp

class ScheduledReminderReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        when (intent.action) {
            Intent.ACTION_BOOT_COMPLETED -> {
                val app = context.applicationContext as? DwayneApp ?: return
                ScheduledReminderScheduler.rescheduleAll(context, app.taskRepository.state.value)
            }

            else -> {
                val reminder = ScheduledReminderScheduler.reminderFromIntent(intent) ?: return
                ScheduledReminderNotifications.showIfPermitted(context, reminder)
                val app = context.applicationContext as? DwayneApp ?: return
                ScheduledReminderScheduler.rescheduleAll(context, app.taskRepository.state.value)
            }
        }
    }
}
