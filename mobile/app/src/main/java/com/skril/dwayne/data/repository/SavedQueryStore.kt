package com.skril.dwayne.data.repository

import android.content.Context
import androidx.datastore.preferences.core.edit
import androidx.datastore.preferences.core.stringPreferencesKey
import androidx.datastore.preferences.preferencesDataStore
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import kotlinx.serialization.Serializable
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.Json

@Serializable
data class SavedQuery(
    val name: String,
    val query: String,
    val viewFilter: String? = null,
)

private val Context.savedQueryDataStore by preferencesDataStore(name = "saved_queries")

class SavedQueryStore(context: Context) {

    private val dataStore = context.applicationContext.savedQueryDataStore
    private val json = Json { ignoreUnknownKeys = true; isLenient = true }
    private val listSerializer = ListSerializer(SavedQuery.serializer())

    val queries: Flow<List<SavedQuery>> = dataStore.data.map { prefs ->
        val raw = prefs[QUERIES_KEY] ?: return@map emptyList()
        runCatching { json.decodeFromString(listSerializer, raw) }.getOrDefault(emptyList())
    }

    suspend fun add(query: SavedQuery) {
        dataStore.edit { prefs ->
            val current = prefs[QUERIES_KEY]?.let {
                runCatching { json.decodeFromString(listSerializer, it) }.getOrDefault(emptyList())
            } ?: emptyList()
            // Replace by name if it already exists, otherwise append.
            val updated = current.filter { it.name != query.name } + query
            prefs[QUERIES_KEY] = json.encodeToString(listSerializer, updated)
        }
    }

    suspend fun remove(name: String) {
        dataStore.edit { prefs ->
            val current = prefs[QUERIES_KEY]?.let {
                runCatching { json.decodeFromString(listSerializer, it) }.getOrDefault(emptyList())
            } ?: emptyList()
            val updated = current.filter { it.name != name }
            prefs[QUERIES_KEY] = json.encodeToString(listSerializer, updated)
        }
    }

    companion object {
        private val QUERIES_KEY = stringPreferencesKey("queries_json")
    }
}
