package com.skril.dwayne.data.sync

import android.util.Base64
import com.skril.dwayne.data.events.EventsResponse
import com.skril.dwayne.data.events.PostEventsRequest
import com.skril.dwayne.data.events.PostEventsResponse
import com.skril.dwayne.data.events.Event
import io.ktor.client.HttpClient
import io.ktor.client.call.body
import io.ktor.client.engine.android.Android
import io.ktor.client.plugins.contentnegotiation.ContentNegotiation
import io.ktor.client.plugins.defaultRequest
import io.ktor.client.request.get
import io.ktor.client.request.header
import io.ktor.client.request.parameter
import io.ktor.client.request.post
import io.ktor.client.request.setBody
import io.ktor.http.ContentType
import io.ktor.http.HttpHeaders
import io.ktor.http.contentType
import io.ktor.serialization.kotlinx.json.json
import kotlinx.serialization.json.Json

/** HTTP client for the events sync endpoints. Stateless; one per sync run. */
class SyncApi(
    private val baseUrl: String,
    username: String,
    password: String,
) {

    private val basicAuthHeader: String? = run {
        val u = username.trim()
        val p = password.trim()
        if (u.isNotBlank() && p.isNotEmpty()) {
            "Basic " + Base64.encodeToString("$u:$p".toByteArray(), Base64.NO_WRAP)
        } else null
    }

    private val client = HttpClient(Android) {
        install(ContentNegotiation) {
            json(Json {
                ignoreUnknownKeys = true
                isLenient = true
                encodeDefaults = false
                explicitNulls = false
            })
        }
        basicAuthHeader?.let { header ->
            defaultRequest {
                header(HttpHeaders.Authorization, header)
            }
        }
    }

    suspend fun pull(since: String?): EventsResponse {
        return client.get("$baseUrl/api/events") {
            if (!since.isNullOrBlank()) parameter("since", since)
        }.body()
    }

    suspend fun push(events: List<Event>): PostEventsResponse {
        return client.post("$baseUrl/api/events") {
            contentType(ContentType.Application.Json)
            setBody(PostEventsRequest(events = events))
        }.body()
    }

    fun close() {
        client.close()
    }
}
