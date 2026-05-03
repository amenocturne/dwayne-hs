package com.skril.dwayne.data.events

import com.skril.dwayne.data.model.OrgTime
import com.skril.dwayne.data.model.TextNode
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

/**
 * A partial task update. Field semantics mirror the server's events table:
 *
 * - `null`                 → field not touched by this event
 * - sentinel value         → field cleared
 *   (`-1` for Int, `""` for String, `[]` for list, `1970-01-01` OrgTime,
 *    empty RichText `[]` for title/description)
 * - any other value        → field set to that value
 *
 * A genesis event populates every field; subsequent delta events populate
 * only the changed fields.
 *
 * Wire JSON keys match the Haskell server: `file`, `taskIndex`, `occurredAt`,
 * plus the per-field keys. Note: Event uses `created` whereas Task uses
 * `createdProp` — different naming on the wire.
 */
@Serializable
data class Event(
    @SerialName("file") val filePath: String,
    val taskIndex: Int,
    /** ISO8601 UTC, e.g. `2026-04-30T12:34:56Z`. */
    val occurredAt: String,
    val level: Int? = null,
    val todoKeyword: String? = null,
    val priority: Int? = null,
    val title: List<TextNode>? = null,
    val tags: List<String>? = null,
    val scheduled: OrgTime? = null,
    val deadline: OrgTime? = null,
    val created: OrgTime? = null,
    val closed: OrgTime? = null,
    val properties: List<List<String>>? = null,
    val description: List<TextNode>? = null,
)

@Serializable
data class EventsResponse(
    val events: List<Event>,
    val serverNow: String,
)

@Serializable
data class PostEventsRequest(
    val events: List<Event>,
)

@Serializable
data class PostEventsResponse(
    val accepted: Int,
    val serverNow: String,
)
