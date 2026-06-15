package com.skril.dwayne.data.repository

import com.skril.dwayne.data.events.Event
import com.skril.dwayne.data.events.EventProjection
import com.skril.dwayne.data.events.EventStore
import com.skril.dwayne.data.events.Nullable
import com.skril.dwayne.data.model.*
import com.skril.dwayne.data.query.applyTaskView
import com.skril.dwayne.data.query.filterTaskSearch
import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import kotlinx.coroutines.withContext
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import java.util.TimeZone

/**
 * Local-first repository. Reads task state from the projection of the local
 * events table; mutations append delta events. The legacy network-per-action
 * model is gone — sync is the worker's job.
 *
 * View/search semantics replicate the server's `Api.Views` projection over
 * the in-memory state. They're an approximation: the server has more views
 * (e.g. project trees) than we replicate here. The goal is parity with what
 * the existing UI tabs expect.
 */
class LocalTaskRepository(
    private val store: EventStore,
    private val inboxFileProvider: () -> String,
    private val ioDispatcher: CoroutineDispatcher = Dispatchers.IO,
) : TaskRepository {

    private val mutex = Mutex()
    private val _state = MutableStateFlow<Map<TaskPointer, Task>>(emptyMap())
    val state: StateFlow<Map<TaskPointer, Task>> = _state.asStateFlow()

    /** Monotonic timestamp source — bumps forward on every emit to avoid PK collisions. */
    private var lastEmittedMillis: Long = 0L

    /** Reload the projection from the events table. Call after sync inserts. */
    fun reloadFromDb() {
        val events = store.selectAll()
        _state.value = EventProjection.project(events)
    }

    init {
        reloadFromDb()
    }

    // -- Reads ---------------------------------------------------------------

    override suspend fun getView(viewName: String, offset: Int, limit: Int): PaginatedResponse {
        val all = currentList()
        val filtered = applyTaskView(viewName, all)
        return paginate(filtered, offset, limit)
    }

    override suspend fun search(query: String, view: String?, offset: Int, limit: Int): PaginatedResponse {
        val base = if (view != null) applyTaskView(view, currentList()) else currentList()
        val filtered = filterTaskSearch(base, query)
        return paginate(filtered, offset, limit)
    }

    override suspend fun recentCaptures(limit: Int): List<TaskWithPointer> = withContext(ioDispatcher) {
        val file = inboxFileProvider()
        if (file.isBlank() || limit <= 0) return@withContext emptyList()
        val tasksByPointer = _state.value
        // The event model currently has no device/source metadata. The inbox
        // file is the closest persisted provenance for phone capture, so this
        // may include synced genesis events in the same configured inbox file.
        store.selectRecentGenesisEventsForFile(file, limit)
            .mapNotNull { event ->
                val pointer = TaskPointer(event.filePath, event.taskIndex)
                tasksByPointer[pointer]?.let { task -> TaskWithPointer(task, pointer) }
            }
    }

    private fun currentList(): List<TaskWithPointer> =
        _state.value.entries
            .map { (ptr, task) -> TaskWithPointer(task, ptr) }
            .sortedWith(compareBy({ it.pointer.file }, { it.pointer.taskIndex }))

    private fun paginate(items: List<TaskWithPointer>, offset: Int, limit: Int): PaginatedResponse {
        val total = items.size
        val page = items.drop(offset).take(limit)
        return PaginatedResponse(data = page, metadata = PaginationMetadata(total = total))
    }

    // -- Mutations -----------------------------------------------------------

    override suspend fun capture(title: String): TaskWithPointer = withContext(ioDispatcher) {
        mutex.withLock {
            val file = inboxFileProvider()
            require(file.isNotBlank()) {
                "Inbox file is not configured. Set it under Settings → Inbox file."
            }
            val nextIndex = (store.maxTaskIndexForFile(file) ?: -1) + 1
            val now = isoNow()
            val task = Task(
                level = 1,
                todoKeyword = "INBOX",
                priority = null,
                title = EventProjection.plainTitle(title.trim()),
                tags = emptyList(),
                scheduled = null,
                deadline = null,
                createdProp = OrgTime(date = today()),
                closed = null,
                properties = emptyList(),
                description = emptyList(),
            )
            val event = EventProjection.genesisEvent(file, nextIndex, now, task)
            applyEvent(event)
            TaskWithPointer(task = task, pointer = TaskPointer(file, nextIndex))
        }
    }

    override suspend fun editTask(request: EditTaskRequest): TaskWithPointer = withContext(ioDispatcher) {
        mutex.withLock {
            val pointer = TaskPointer(request.file, request.taskIndex)
            val now = isoNow()
            val event = Event(
                filePath = request.file,
                taskIndex = request.taskIndex,
                occurredAt = now,
                todoKeyword = request.keyword,
                priority = request.priority?.let { it.value ?: Nullable.PRIORITY_CLEARED },
                title = request.title?.let { listOf(TextNode.Plain(it)) },
                tags = request.tags,
                scheduled = request.scheduled?.let { it.value ?: Nullable.ORG_TIME_CLEARED },
                deadline = request.deadline?.let { it.value ?: Nullable.ORG_TIME_CLEARED },
                description = request.description?.let {
                    if (it.isEmpty()) Nullable.DESCRIPTION_CLEARED else listOf(TextNode.Plain(it))
                },
            )
            applyEvent(event)
            val task = _state.value[pointer]
                ?: error("Task not found after edit: $pointer")
            TaskWithPointer(task = task, pointer = pointer)
        }
    }

    override suspend fun changeKeyword(pointer: TaskPointer, keyword: String): TaskWithPointer =
        editTask(EditTaskRequest(file = pointer.file, taskIndex = pointer.taskIndex, keyword = keyword))

    override suspend fun changePriority(pointer: TaskPointer, priority: Int?): TaskWithPointer =
        editTask(EditTaskRequest(file = pointer.file, taskIndex = pointer.taskIndex, priority = ClearOrSet(priority)))

    override suspend fun addTag(pointer: TaskPointer, tag: String): TaskWithPointer = withContext(ioDispatcher) {
        mutex.withLock {
            val current = _state.value[pointer] ?: error("Task not found: $pointer")
            val newTags = if (tag in current.tags) current.tags else current.tags + tag
            val event = Event(
                filePath = pointer.file,
                taskIndex = pointer.taskIndex,
                occurredAt = isoNow(),
                tags = newTags,
            )
            applyEvent(event)
            TaskWithPointer(task = _state.value[pointer]!!, pointer = pointer)
        }
    }

    override suspend fun removeTag(pointer: TaskPointer, tag: String): TaskWithPointer = withContext(ioDispatcher) {
        mutex.withLock {
            val current = _state.value[pointer] ?: error("Task not found: $pointer")
            val newTags = current.tags - tag
            val event = Event(
                filePath = pointer.file,
                taskIndex = pointer.taskIndex,
                occurredAt = isoNow(),
                tags = if (newTags.isEmpty()) Nullable.TAGS_CLEARED else newTags,
            )
            applyEvent(event)
            TaskWithPointer(task = _state.value[pointer]!!, pointer = pointer)
        }
    }

    override suspend fun deleteTask(pointer: TaskPointer): TaskWithPointer =
        editTask(EditTaskRequest(file = pointer.file, taskIndex = pointer.taskIndex, keyword = "TRASH"))

    /** Append an event to the store and update the projection. */
    private fun applyEvent(event: Event) {
        store.insert(event)
        // Re-project from DB to keep state and DB strictly consistent.
        reloadFromDb()
    }

    private fun isoNow(): String {
        // Monotonic millisecond resolution. Same-millisecond writes from one
        // device would otherwise collide on the events PK
        // (file, idx, occurredAt) and the second one would be silently
        // dropped by INSERT OR IGNORE. We force forward progress by tracking
        // last-emitted millis and bumping +1ms on collision.
        val now = System.currentTimeMillis()
        val ts = if (now > lastEmittedMillis) now else lastEmittedMillis + 1
        lastEmittedMillis = ts
        val sdf = SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", Locale.US).apply {
            timeZone = TimeZone.getTimeZone("UTC")
        }
        return sdf.format(Date(ts))
    }

    private fun today(): String {
        val sdf = SimpleDateFormat("yyyy-MM-dd", Locale.US).apply {
            timeZone = TimeZone.getDefault()
        }
        return sdf.format(Date())
    }
}
