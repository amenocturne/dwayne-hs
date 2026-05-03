package com.skril.dwayne.data.events

import com.skril.dwayne.data.model.OrgTime
import com.skril.dwayne.data.model.TextNode

/**
 * Sentinel-based "null" representation for event-sourced fields. Mirrors the
 * server's `Core.Nullable`. Each type has a designated cleared-field sentinel.
 *
 * Encoding rules:
 *   - field == null            → event did not touch the field
 *   - field == sentinel value  → event cleared the field
 *   - field == any other       → event set the field to that value
 */
object Nullable {

    const val PRIORITY_CLEARED: Int = -1
    const val KEYWORD_CLEARED: String = ""
    val TAGS_CLEARED: List<String> = emptyList()
    val PROPERTIES_CLEARED: List<List<String>> = emptyList()
    val TITLE_CLEARED: List<TextNode> = emptyList()
    val DESCRIPTION_CLEARED: List<TextNode> = emptyList()
    /** Sentinel epoch used for cleared org times: 1970-01-01. */
    val ORG_TIME_CLEARED: OrgTime = OrgTime(date = "1970-01-01")

    fun isPriorityNull(v: Int): Boolean = v == PRIORITY_CLEARED
    fun isKeywordNull(v: String): Boolean = v == KEYWORD_CLEARED
    fun isTagsNull(v: List<String>): Boolean = v.isEmpty()
    fun isPropertiesNull(v: List<List<String>>): Boolean = v.isEmpty()
    fun isTitleNull(v: List<TextNode>): Boolean = v.isEmpty()
    fun isDescriptionNull(v: List<TextNode>): Boolean = v.isEmpty()

    fun isOrgTimeNull(v: OrgTime): Boolean {
        if (v.date != "1970-01-01") return false
        // Cleared sentinel either has no time or 00:00. Accept both.
        return v.time == null || v.time == "00:00" || v.time == "00:00:00"
    }
}
