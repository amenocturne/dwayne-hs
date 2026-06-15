package com.skril.dwayne.ui.navigation

import org.junit.Assert.assertSame
import org.junit.Test

class NavigationDefaultsTest {

    @Test
    fun `app starts on capture`() {
        assertSame(Screen.Capture, DefaultStartScreen)
    }
}
