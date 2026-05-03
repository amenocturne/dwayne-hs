package com.skril.dwayne.data.events

import com.skril.dwayne.data.model.OrgTime
import com.skril.dwayne.data.model.TextNode
import com.skril.dwayne.db.DwayneDatabase
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.builtins.serializer
import kotlinx.serialization.json.Json

/**
 * Adapter over the SQLDelight-generated DB. Encodes/decodes Event JSON columns
 * (title, tags, scheduled, deadline, created, closed, properties, description)
 * and exposes the rest as primitive types.
 */
class EventStore(private val db: DwayneDatabase) {

    private val json = Json { ignoreUnknownKeys = true; isLenient = true; encodeDefaults = false }
    private val textNodeListSerializer = ListSerializer(TextNode.serializer())
    private val stringListSerializer = ListSerializer(String.serializer())
    private val propertiesSerializer = ListSerializer(ListSerializer(String.serializer()))
    private val orgTimeSerializer = OrgTime.serializer()

    fun selectAll(): List<Event> =
        db.eventsQueries.selectAll().executeAsList().map(::rowToEvent)

    fun selectAfter(lastPushedAt: String): List<Event> =
        db.eventsQueries.selectAfter(lastPushedAt).executeAsList().map(::rowToEvent)

    fun maxTaskIndexForFile(filePath: String): Int? =
        db.eventsQueries.maxTaskIndexForFile(filePath).executeAsOneOrNull()?.MAX?.toInt()

    fun count(): Long = db.eventsQueries.countAll().executeAsOne()

    /**
     * INSERT OR IGNORE for a single event. Returns true if inserted (false if
     * the (file, idx, occurredAt) PK already existed).
     */
    fun insert(event: Event): Boolean {
        val before = count()
        insertRow(event)
        return count() > before
    }

    /** Bulk insert. Returns number of new rows actually written. */
    fun insertAll(events: List<Event>): Int {
        if (events.isEmpty()) return 0
        return db.transactionWithResult {
            val before = count()
            for (e in events) insertRow(e)
            (count() - before).toInt()
        }
    }

    private fun insertRow(e: Event) {
        db.eventsQueries.insertEvent(
            filePath = e.filePath,
            taskIndex = e.taskIndex.toLong(),
            occurredAt = e.occurredAt,
            level = e.level?.toLong(),
            todoKeyword = e.todoKeyword,
            priority = e.priority?.toLong(),
            title = e.title?.let { json.encodeToString(textNodeListSerializer, it) },
            tags = e.tags?.let { json.encodeToString(stringListSerializer, it) },
            scheduled = e.scheduled?.let { json.encodeToString(orgTimeSerializer, it) },
            deadline = e.deadline?.let { json.encodeToString(orgTimeSerializer, it) },
            created = e.created?.let { json.encodeToString(orgTimeSerializer, it) },
            closed = e.closed?.let { json.encodeToString(orgTimeSerializer, it) },
            properties = e.properties?.let { json.encodeToString(propertiesSerializer, it) },
            description = e.description?.let { json.encodeToString(textNodeListSerializer, it) },
        )
    }

    private fun rowToEvent(row: com.skril.dwayne.db.Events): Event = Event(
        filePath = row.file_path,
        taskIndex = row.task_index.toInt(),
        occurredAt = row.occurred_at,
        level = row.level?.toInt(),
        todoKeyword = row.todo_keyword,
        priority = row.priority?.toInt(),
        title = row.title?.let { json.decodeFromString(textNodeListSerializer, it) },
        tags = row.tags?.let { json.decodeFromString(stringListSerializer, it) },
        scheduled = row.scheduled?.let { json.decodeFromString(orgTimeSerializer, it) },
        deadline = row.deadline?.let { json.decodeFromString(orgTimeSerializer, it) },
        created = row.created?.let { json.decodeFromString(orgTimeSerializer, it) },
        closed = row.closed?.let { json.decodeFromString(orgTimeSerializer, it) },
        properties = row.properties?.let { json.decodeFromString(propertiesSerializer, it) },
        description = row.description?.let { json.decodeFromString(textNodeListSerializer, it) },
    )

    // sync_state ---------------------------------------------------------

    fun getSyncState(key: String): String? =
        db.syncStateQueries.get(key).executeAsOneOrNull()

    fun putSyncState(key: String, value: String) {
        db.syncStateQueries.put(key, value)
    }

    companion object {
        const val KEY_LAST_PULLED_AT = "last_pulled_at"
        const val KEY_LAST_PUSHED_AT = "last_pushed_at"
        const val KEY_DEVICE_ID = "device_id"
        const val KEY_OWNED_FILES = "owned_files"
    }
}
