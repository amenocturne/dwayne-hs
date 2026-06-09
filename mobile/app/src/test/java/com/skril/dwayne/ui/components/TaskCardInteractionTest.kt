package com.skril.dwayne.ui.components

import com.skril.dwayne.data.model.TaskPointer
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class TaskCardInteractionTest {

    @Test
    fun `preview-only cards do not open detail`() {
        assertFalse(TaskCardInteraction.PreviewOnly.opensDetail())
    }

    @Test
    fun `open-detail cards declare detail navigation`() {
        val interaction = TaskCardInteraction.OpenDetail(TaskPointer("/inbox.org", 1)) {}

        assertTrue(interaction.opensDetail())
    }
}
