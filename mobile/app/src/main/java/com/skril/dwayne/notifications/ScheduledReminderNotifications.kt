package com.skril.dwayne.notifications

import android.Manifest
import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.os.Build
import androidx.core.app.NotificationCompat
import androidx.core.app.NotificationManagerCompat
import androidx.core.content.ContextCompat
import com.skril.dwayne.MainActivity
import com.skril.dwayne.R

object ScheduledReminderNotifications {
    const val ACTION_OPEN_TASK = "com.skril.dwayne.action.OPEN_TASK"
    const val EXTRA_FILE = "com.skril.dwayne.extra.FILE"
    const val EXTRA_TASK_INDEX = "com.skril.dwayne.extra.TASK_INDEX"

    private const val CHANNEL_ID = "scheduled_reminders"

    fun ensureChannel(context: Context) {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) return
        val channel = NotificationChannel(
            CHANNEL_ID,
            context.getString(R.string.scheduled_reminder_channel_name),
            NotificationManager.IMPORTANCE_HIGH,
        ).apply {
            description = context.getString(R.string.scheduled_reminder_channel_description)
        }
        context.getSystemService(NotificationManager::class.java).createNotificationChannel(channel)
    }

    fun showIfPermitted(context: Context, reminder: ScheduledReminder): Boolean {
        ensureChannel(context)
        if (!canPostNotifications(context)) return false
        NotificationManagerCompat.from(context).notify(
            reminder.notificationId,
            NotificationCompat.Builder(context, CHANNEL_ID)
                .setSmallIcon(R.drawable.ic_launcher_foreground)
                .setContentTitle(reminder.title)
                .setContentText(context.getString(R.string.scheduled_reminder_notification_text))
                .setContentIntent(openTaskPendingIntent(context, reminder))
                .setAutoCancel(true)
                .setCategory(NotificationCompat.CATEGORY_REMINDER)
                .setPriority(NotificationCompat.PRIORITY_HIGH)
                .build(),
        )
        return true
    }

    fun taskPointerFromIntent(intent: Intent?): Pair<String, Int>? {
        if (intent?.action != ACTION_OPEN_TASK) return null
        val file = intent.getStringExtra(EXTRA_FILE) ?: return null
        val taskIndex = intent.getIntExtra(EXTRA_TASK_INDEX, -1)
        if (taskIndex < 0) return null
        return file to taskIndex
    }

    private fun canPostNotifications(context: Context): Boolean =
        Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU ||
            ContextCompat.checkSelfPermission(
                context,
                Manifest.permission.POST_NOTIFICATIONS,
            ) == PackageManager.PERMISSION_GRANTED

    private fun openTaskPendingIntent(context: Context, reminder: ScheduledReminder): PendingIntent {
        val intent = Intent(context, MainActivity::class.java).apply {
            action = ACTION_OPEN_TASK
            putExtra(EXTRA_FILE, reminder.file)
            putExtra(EXTRA_TASK_INDEX, reminder.taskIndex)
            addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP or Intent.FLAG_ACTIVITY_SINGLE_TOP)
        }
        return PendingIntent.getActivity(
            context,
            reminder.notificationId,
            intent,
            PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE,
        )
    }
}
