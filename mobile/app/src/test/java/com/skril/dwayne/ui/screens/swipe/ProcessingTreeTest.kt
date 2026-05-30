package com.skril.dwayne.ui.screens.swipe

import com.skril.dwayne.ui.theme.KeywordToday
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class ProcessingTreeTest {

    @Test
    fun `default root uses music macro instead of today`() {
        val node = DefaultProcessingTree.children[Dir.Up.ordinal] as Terminal
        val modification = node.modification as Modification.SetKeywordAndTags

        assertEquals("MUSIC", node.label)
        assertEquals("LIST", modification.keyword)
        assertEquals(listOf("music", "download"), modification.tags)
    }

    @Test
    fun `migrate replaces old today terminal with music macro`() {
        val oldTree = DefaultProcessingTree.withChild(
            Dir.Up,
            Terminal("TODAY", KeywordToday, Modification.SetKeyword("TODAY")),
        )

        val migrated = migrateProcessingTree(oldTree)
        val node = migrated.children[Dir.Up.ordinal] as Terminal
        val modification = node.modification as Modification.SetKeywordAndTags

        assertEquals("MUSIC", node.label)
        assertEquals("LIST", modification.keyword)
        assertEquals(listOf("music", "download"), modification.tags)
    }

    @Test
    fun `keyword and tags modification survives json round trip`() {
        val node = Terminal(
            "MUSIC",
            KeywordToday,
            Modification.SetKeywordAndTags("LIST", listOf("music", "download")),
        )

        val roundTripped = node.toJson().toRuntime() as Terminal

        assertTrue(roundTripped.modification is Modification.SetKeywordAndTags)
        val modification = roundTripped.modification as Modification.SetKeywordAndTags
        assertEquals("LIST", modification.keyword)
        assertEquals(listOf("music", "download"), modification.tags)
    }
}
