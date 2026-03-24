package com.skril.dwayne.data.repository

import com.skril.dwayne.data.model.*
import io.ktor.client.HttpClient
import io.ktor.client.call.body
import io.ktor.client.engine.android.Android
import io.ktor.client.plugins.contentnegotiation.ContentNegotiation
import io.ktor.client.request.get
import io.ktor.client.request.parameter
import io.ktor.client.request.post
import io.ktor.client.request.put
import io.ktor.client.request.setBody
import io.ktor.http.ContentType
import io.ktor.http.contentType
import io.ktor.serialization.kotlinx.json.json
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonNull
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.encodeToJsonElement
import kotlinx.serialization.json.put
import kotlinx.serialization.json.putJsonArray

class ApiTaskRepository(private val baseUrl: String) : TaskRepository {

    private val client = HttpClient(Android) {
        install(ContentNegotiation) {
            json(Json {
                ignoreUnknownKeys = true
                isLenient = true
            })
        }
    }

    override suspend fun getView(viewName: String, offset: Int, limit: Int): PaginatedResponse {
        return client.get("$baseUrl/api/views/$viewName") {
            parameter("offset", offset)
            parameter("limit", limit)
        }.body()
    }

    override suspend fun search(query: String, view: String?, offset: Int, limit: Int): PaginatedResponse {
        return client.get("$baseUrl/api/search") {
            parameter("query", query)
            if (view != null) parameter("view", view)
            parameter("offset", offset)
            parameter("limit", limit)
        }.body()
    }

    override suspend fun capture(title: String): TaskWithPointer {
        return client.post("$baseUrl/api/tasks/capture") {
            contentType(ContentType.Application.Json)
            setBody(CaptureRequest(captureTitle = title))
        }.body()
    }

    override suspend fun editTask(request: EditTaskRequest): TaskWithPointer {
        val jsonSerializer = Json { ignoreUnknownKeys = true; isLenient = true }
        val body = buildJsonObject {
            put("file", request.file)
            put("taskIndex", request.taskIndex)
            request.keyword?.let { put("keyword", it) }
            request.priority?.let { clearOrSet ->
                if (clearOrSet.value == null) put("priority", JsonNull) else put("priority", clearOrSet.value)
            }
            request.title?.let { put("title", it) }
            request.tags?.let { tagList ->
                putJsonArray("tags") { tagList.forEach { add(kotlinx.serialization.json.JsonPrimitive(it)) } }
            }
            request.scheduled?.let { clearOrSet ->
                if (clearOrSet.value == null) put("scheduled", JsonNull)
                else put("scheduled", jsonSerializer.encodeToJsonElement(clearOrSet.value))
            }
            request.deadline?.let { clearOrSet ->
                if (clearOrSet.value == null) put("deadline", JsonNull)
                else put("deadline", jsonSerializer.encodeToJsonElement(clearOrSet.value))
            }
        }
        return client.put("$baseUrl/api/tasks/edit") {
            contentType(ContentType.Application.Json)
            setBody(body)
        }.body()
    }

    override suspend fun changeKeyword(pointer: TaskPointer, keyword: String): TaskWithPointer =
        editTask(EditTaskRequest(file = pointer.file, taskIndex = pointer.taskIndex, keyword = keyword))

    override suspend fun changePriority(pointer: TaskPointer, priority: Int?): TaskWithPointer =
        editTask(EditTaskRequest(file = pointer.file, taskIndex = pointer.taskIndex, priority = ClearOrSet(priority)))

    override suspend fun addTag(pointer: TaskPointer, tag: String): TaskWithPointer {
        return client.post("$baseUrl/api/tasks/tags/add") {
            contentType(ContentType.Application.Json)
            setBody(TagRequest(
                trFile = pointer.file,
                trTaskIndex = pointer.taskIndex,
                trTag = tag,
            ))
        }.body()
    }

    override suspend fun removeTag(pointer: TaskPointer, tag: String): TaskWithPointer {
        return client.post("$baseUrl/api/tasks/tags/remove") {
            contentType(ContentType.Application.Json)
            setBody(TagRequest(
                trFile = pointer.file,
                trTaskIndex = pointer.taskIndex,
                trTag = tag,
            ))
        }.body()
    }

    override suspend fun deleteTask(pointer: TaskPointer): TaskWithPointer =
        editTask(EditTaskRequest(file = pointer.file, taskIndex = pointer.taskIndex, keyword = "TRASH"))
}
