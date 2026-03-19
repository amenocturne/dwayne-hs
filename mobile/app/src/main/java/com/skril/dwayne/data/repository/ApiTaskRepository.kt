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

    override suspend fun changeKeyword(pointer: TaskPointer, keyword: String): TaskWithPointer {
        return client.put("$baseUrl/api/tasks/keyword") {
            contentType(ContentType.Application.Json)
            setBody(ChangeKeywordRequest(
                ckrFile = pointer.file,
                ckrTaskIndex = pointer.taskIndex,
                ckrKeyword = keyword,
            ))
        }.body()
    }

    override suspend fun changePriority(pointer: TaskPointer, priority: Int?): TaskWithPointer {
        return client.put("$baseUrl/api/tasks/priority") {
            contentType(ContentType.Application.Json)
            setBody(ChangePriorityRequest(
                cprFile = pointer.file,
                cprTaskIndex = pointer.taskIndex,
                cprPriority = priority,
            ))
        }.body()
    }

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

    override suspend fun deleteTask(pointer: TaskPointer): TaskWithPointer {
        return client.post("$baseUrl/api/tasks/delete") {
            contentType(ContentType.Application.Json)
            setBody(TaskPointerRequest(
                tprFile = pointer.file,
                tprTaskIndex = pointer.taskIndex,
            ))
        }.body()
    }
}
