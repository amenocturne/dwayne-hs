package com.skril.dwayne

import android.content.Intent
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import com.skril.dwayne.ui.navigation.DwayneNavHost
import com.skril.dwayne.ui.theme.DwayneTheme

class MainActivity : ComponentActivity() {

    private var pendingShareUpdate: ((String?) -> Unit)? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()
        setContent {
            DwayneTheme {
                var shareText by remember { mutableStateOf(extractShareText(intent)) }
                pendingShareUpdate = { shareText = it }
                DwayneNavHost(
                    initialCaptureText = shareText,
                    onCaptureConsumed = { shareText = null },
                )
            }
        }
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        setIntent(intent)
        extractShareText(intent)?.let { text -> pendingShareUpdate?.invoke(text) }
    }

    private fun extractShareText(intent: Intent?): String? {
        if (intent?.action != Intent.ACTION_SEND) return null
        if (intent.type != "text/plain") return null
        val text = intent.getStringExtra(Intent.EXTRA_TEXT)?.trim().orEmpty()
        if (text.isEmpty()) return null
        val subject = intent.getStringExtra(Intent.EXTRA_SUBJECT)?.trim()
        return formatAsOrgLink(text, subject)
    }

    private fun formatAsOrgLink(text: String, subject: String?): String {
        val isUrl = text.startsWith("http://") || text.startsWith("https://")
        return when {
            isUrl && !subject.isNullOrBlank() -> "[[$text][$subject]] "
            else -> "$text "
        }
    }
}
