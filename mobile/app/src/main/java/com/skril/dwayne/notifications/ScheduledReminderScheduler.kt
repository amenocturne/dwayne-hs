package com.skril.dwayne.notifications

import android.app.AlarmManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.os.Build
import android.os.SystemClock
import android.util.Log
import com.skril.dwayne.data.model.Task
import com.skril.dwayne.data.model.TaskPointer

object ScheduledReminderScheduler {
    private const val ACTION_TRIGGER = "com.skril.dwayne.action.SCHEDULED_REMINDER_TRIGGER"
    private const val EXTRA_TITLE = "com.skril.dwayne.extra.TITLE"
    private const val EXTRA_TRIGGER_AT = "com.skril.dwayne.extra.TRIGGER_AT"
    private const val TAG = "DwayneScheduledReminder"

    fun rescheduleAll(
        context: Context,
        tasks: Map<TaskPointer, Task>,
        dateOnlyReminderTime: String,
    ) {
        ScheduledReminderNotifications.ensureChannel(context)
        cancelAll(context, tasks.keys)
        ScheduledReminderPlan.upcoming(tasks, dateOnlyReminderTime).forEach { schedule(context, it) }
    }

    fun reminderFromIntent(intent: Intent?): ScheduledReminder? {
        if (intent?.action != ACTION_TRIGGER) return null
        val file = intent.getStringExtra(ScheduledReminderNotifications.EXTRA_FILE) ?: return null
        val taskIndex = intent.getIntExtra(ScheduledReminderNotifications.EXTRA_TASK_INDEX, -1)
        if (taskIndex < 0) return null
        return ScheduledReminder(
            file = file,
            taskIndex = taskIndex,
            title = intent.getStringExtra(EXTRA_TITLE).orEmpty().ifBlank { "Scheduled task" },
            triggerAtMillis = intent.getLongExtra(EXTRA_TRIGGER_AT, System.currentTimeMillis()),
        )
    }

    private fun schedule(context: Context, reminder: ScheduledReminder) {
        val delayMillis = (reminder.triggerAtMillis - System.currentTimeMillis()).coerceAtLeast(1L)
        val pendingIntent = triggerPendingIntent(context, reminder)
        val alarmManager = context.getSystemService(AlarmManager::class.java)
        val triggerAtElapsed = SystemClock.elapsedRealtime() + delayMillis
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                alarmManager.setExactAndAllowWhileIdle(
                    AlarmManager.ELAPSED_REALTIME_WAKEUP,
                    triggerAtElapsed,
                    pendingIntent,
                )
            } else {
                alarmManager.setExact(
                    AlarmManager.ELAPSED_REALTIME_WAKEUP,
                    triggerAtElapsed,
                    pendingIntent,
                )
            }
        } catch (e: SecurityException) {
            Log.w(TAG, "Exact alarm denied; falling back to inexact reminder", e)
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                alarmManager.setAndAllowWhileIdle(
                    AlarmManager.ELAPSED_REALTIME_WAKEUP,
                    triggerAtElapsed,
                    pendingIntent,
                )
            } else {
                alarmManager.set(
                    AlarmManager.ELAPSED_REALTIME_WAKEUP,
                    triggerAtElapsed,
                    pendingIntent,
                )
            }
        }
    }

    private fun cancelAll(context: Context, pointers: Set<TaskPointer>) {
        val alarmManager = context.getSystemService(AlarmManager::class.java)
        pointers.forEach { pointer ->
            alarmManager.cancel(
                PendingIntent.getBroadcast(
                    context,
                    notificationId(pointer.file, pointer.taskIndex),
                    Intent(context, ScheduledReminderReceiver::class.java).apply { action = ACTION_TRIGGER },
                    PendingIntent.FLAG_NO_CREATE or PendingIntent.FLAG_IMMUTABLE,
                ) ?: return@forEach,
            )
        }
    }

    private fun triggerPendingIntent(context: Context, reminder: ScheduledReminder): PendingIntent {
        val intent = Intent(context, ScheduledReminderReceiver::class.java).apply {
            action = ACTION_TRIGGER
            putExtra(ScheduledReminderNotifications.EXTRA_FILE, reminder.file)
            putExtra(ScheduledReminderNotifications.EXTRA_TASK_INDEX, reminder.taskIndex)
            putExtra(EXTRA_TITLE, reminder.title)
            putExtra(EXTRA_TRIGGER_AT, reminder.triggerAtMillis)
        }
        return PendingIntent.getBroadcast(
            context,
            reminder.notificationId,
            intent,
            PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE,
        )
    }

    private fun notificationId(file: String, taskIndex: Int): Int =
        ScheduledReminder(file, taskIndex, "", 0L).notificationId
}
