package com.skril.dwayne

import android.Manifest
import android.content.Intent
import android.content.pm.PackageManager
import android.os.Build
import android.os.Bundle
import android.util.Log
import androidx.activity.ComponentActivity
import androidx.activity.result.contract.ActivityResultContracts
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.compose.runtime.getValue
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.core.content.ContextCompat
import com.skril.dwayne.notifications.AmbientCaptureNotification
import com.skril.dwayne.share.ShareIntentText
import com.skril.dwayne.ui.navigation.DwayneNavHost
import com.skril.dwayne.ui.theme.DwayneTheme
import kotlinx.coroutines.flow.MutableSharedFlow

class MainActivity : ComponentActivity() {

    private val shareTextEvents = MutableSharedFlow<String>(extraBufferCapacity = 1)
    private var pendingOpenCaptureUpdate: (() -> Unit)? = null

    private val notificationPermissionLauncher = registerForActivityResult(
        ActivityResultContracts.RequestPermission(),
    ) { granted ->
        if (granted) {
            val posted = AmbientCaptureNotification.showIfPermitted(this)
            Log.d(TAG_NOTIFICATION, "Ambient capture notification permission granted posted=$posted")
        } else {
            Log.d(TAG_NOTIFICATION, "Ambient capture notification permission denied")
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()
        ensureAmbientCaptureNotification()
        val initialShareText = extractShareText(intent, source = "cold-start")
        val initialOpenCapture = handleCaptureNotificationTap(intent, source = "cold-start")
        setContent {
            DwayneTheme {
                var shareText by remember { mutableStateOf(initialShareText) }
                var shareTextRevision by remember { mutableIntStateOf(if (initialShareText != null) 1 else 0) }
                var openCaptureRequest by remember { mutableIntStateOf(if (initialOpenCapture) 1 else 0) }
                LaunchedEffect(Unit) {
                    shareTextEvents.collect { text ->
                        shareText = text
                        shareTextRevision += 1
                    }
                }
                pendingOpenCaptureUpdate = { openCaptureRequest += 1 }
                DwayneNavHost(
                    initialCaptureText = shareText,
                    initialCaptureTextRevision = shareTextRevision,
                    openCaptureRequest = openCaptureRequest,
                    onCaptureConsumed = { shareText = null },
                )
            }
        }
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        setIntent(intent)
        val shareText = extractShareText(intent, source = "warm-start")
        if (shareText != null) {
            val emitted = shareTextEvents.tryEmit(shareText)
            Log.d(TAG, "Share intent routed to capture source=warm-start emitted=$emitted")
        }
        if (handleCaptureNotificationTap(intent, source = "warm-start")) {
            pendingOpenCaptureUpdate?.invoke()
        }
    }

    private fun ensureAmbientCaptureNotification() {
        AmbientCaptureNotification.ensureChannel(this)
        if (
            Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU &&
            ContextCompat.checkSelfPermission(this, Manifest.permission.POST_NOTIFICATIONS) !=
            PackageManager.PERMISSION_GRANTED
        ) {
            notificationPermissionLauncher.launch(Manifest.permission.POST_NOTIFICATIONS)
            return
        }
        val posted = AmbientCaptureNotification.showIfPermitted(this)
        Log.d(TAG_NOTIFICATION, "Ambient capture notification posted=$posted")
    }

    private fun handleCaptureNotificationTap(intent: Intent?, source: String): Boolean {
        if (!AmbientCaptureNotification.isOpenCaptureIntent(intent)) return false
        Log.d(TAG_NOTIFICATION, "Capture notification tap received source=$source")
        return true
    }

    private fun extractShareText(intent: Intent?, source: String): String? {
        if (intent?.action == Intent.ACTION_SEND) {
            Log.d(
                TAG,
                "Share intent received source=$source type=${intent.type} " +
                    "hasText=${intent.hasExtra(Intent.EXTRA_TEXT)} " +
                    "hasSubject=${intent.hasExtra(Intent.EXTRA_SUBJECT)}",
            )
        }
        val parsed = ShareIntentText.fromIntent(intent)
        if (parsed != null) {
            Log.d(TAG, "Share intent parsed source=$source textLength=${parsed.length}")
        } else if (intent?.action == Intent.ACTION_SEND) {
            Log.d(TAG, "Share intent ignored source=$source")
        }
        return parsed
    }

    companion object {
        private const val TAG = "DwayneShare"
        private const val TAG_NOTIFICATION = "DwayneCaptureNotification"
    }
}
