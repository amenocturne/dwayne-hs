package com.skril.dwayne.data.repository

import android.content.Context
import androidx.datastore.preferences.core.edit
import androidx.datastore.preferences.core.stringPreferencesKey
import androidx.datastore.preferences.preferencesDataStore
import com.skril.dwayne.ui.screens.swipe.Branch
import com.skril.dwayne.ui.screens.swipe.BranchJson
import com.skril.dwayne.ui.screens.swipe.DefaultProcessingTree
import com.skril.dwayne.ui.screens.swipe.migrateProcessingTree
import com.skril.dwayne.ui.screens.swipe.toJson
import com.skril.dwayne.ui.screens.swipe.toRuntime
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import kotlinx.serialization.json.Json

private val Context.processingTreeDataStore by preferencesDataStore(name = "processing_tree")

class TreeStore(context: Context) {

    private val dataStore = context.applicationContext.processingTreeDataStore
    private val json = Json { ignoreUnknownKeys = true; isLenient = true }

    val tree: Flow<Branch> = dataStore.data.map { prefs ->
        val raw = prefs[TREE_KEY] ?: return@map DefaultProcessingTree
        runCatching {
            val node = json.decodeFromString(BranchJson.serializer(), raw)
            migrateProcessingTree(node.toRuntime() as Branch)
        }.getOrDefault(DefaultProcessingTree)
    }

    suspend fun save(tree: Branch) {
        dataStore.edit { prefs ->
            val branchJson = tree.toJson() as BranchJson
            prefs[TREE_KEY] = json.encodeToString(BranchJson.serializer(), branchJson)
        }
    }

    suspend fun reset() {
        dataStore.edit { prefs -> prefs.remove(TREE_KEY) }
    }

    companion object {
        private val TREE_KEY = stringPreferencesKey("tree_json")
    }
}
