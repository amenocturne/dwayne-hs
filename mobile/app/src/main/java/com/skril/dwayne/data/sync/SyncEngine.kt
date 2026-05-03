package com.skril.dwayne.data.sync

import com.skril.dwayne.data.events.EventStore
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.builtins.serializer
import kotlinx.serialization.json.Json
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import java.util.TimeZone

/**
 * Pull-then-push sync. The server is just a transport for the events log.
 *
 *   pull: GET /api/events?since=<last_pulled_at - pullWindow>
 *         INSERT OR IGNORE local rows; advance last_pulled_at to serverNow.
 *   push: SELECT events where occurredAt > last_pushed_at AND file IN ownedFiles.
 *         POST. On 2xx, advance last_pushed_at to max(occurredAt) of pushed batch.
 *
 * Errors propagate so WorkManager can retry with backoff.
 */
class SyncEngine(
    private val store: EventStore,
    private val api: SyncApi,
    private val ownedFiles: List<String>,
    private val pullWindowHours: Int,
) {

    private val json = Json { ignoreUnknownKeys = true; isLenient = true }
    private val stringListSerializer = ListSerializer(String.serializer())

    suspend fun runOnce(initialFullPull: Boolean = false): SyncResult {
        val pulled = pull(initialFullPull)
        val pushed = push()
        return SyncResult(pulled = pulled, pushed = pushed)
    }

    private suspend fun pull(initialFullPull: Boolean): Int {
        val since = if (initialFullPull) null else computePullSince()
        val response = api.pull(since)
        val inserted = store.insertAll(response.events)
        store.putSyncState(EventStore.KEY_LAST_PULLED_AT, response.serverNow)
        return inserted
    }

    private fun computePullSince(): String? {
        val last = store.getSyncState(EventStore.KEY_LAST_PULLED_AT) ?: return null
        // Walk back pullWindowHours so we re-pull anything that might have
        // been written with a clock skew or out-of-order occurredAt.
        return shiftIso(last, -pullWindowHours)
    }

    private suspend fun push(): Int {
        val lastPushed = store.getSyncState(EventStore.KEY_LAST_PUSHED_AT) ?: ZERO_TIME
        val candidates = store.selectAfter(lastPushed)
        if (candidates.isEmpty()) return 0
        val owned = ownedFiles.toSet()
        val toPush = candidates.filter { it.filePath in owned }
        if (toPush.isEmpty()) return 0
        api.push(toPush)
        val maxOccurredAt = toPush.maxOf { it.occurredAt }
        store.putSyncState(EventStore.KEY_LAST_PUSHED_AT, maxOccurredAt)
        return toPush.size
    }

    private fun shiftIso(iso: String, hours: Int): String {
        val fmt = SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.US).apply {
            timeZone = TimeZone.getTimeZone("UTC")
        }
        val parsed = runCatching { fmt.parse(iso) }.getOrNull() ?: return iso
        val shifted = Date(parsed.time + hours.toLong() * 3600L * 1000L)
        return fmt.format(shifted)
    }

    companion object {
        private const val ZERO_TIME = "1970-01-01T00:00:00Z"
    }
}

data class SyncResult(val pulled: Int, val pushed: Int)
