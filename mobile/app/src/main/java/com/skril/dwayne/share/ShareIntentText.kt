package com.skril.dwayne.share

import android.content.Intent

object ShareIntentText {
    fun fromIntent(intent: Intent?): String? =
        parse(
            action = intent?.action,
            type = intent?.type,
            text = intent?.getStringExtra(Intent.EXTRA_TEXT),
            subject = intent?.getStringExtra(Intent.EXTRA_SUBJECT),
        )

    fun parse(action: String?, type: String?, text: String?, subject: String?): String? {
        if (action != Intent.ACTION_SEND) return null
        if (type != "text/plain") return null

        val trimmedText = text?.trim().orEmpty()
        if (trimmedText.isEmpty()) return null

        return format(trimmedText, subject?.trim())
    }

    fun format(text: String, subject: String?): String {
        val isUrl = text.startsWith("http://") || text.startsWith("https://")
        return when {
            isUrl && !subject.isNullOrBlank() -> "[[$text][$subject]] "
            else -> "$text "
        }
    }
}
