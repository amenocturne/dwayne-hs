package com.skril.dwayne.share

import android.content.Intent
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

class ShareIntentTextTest {

    @Test
    fun `formats url shares with subject as org link`() {
        assertEquals(
            "[[https://example.com][Example]] ",
            ShareIntentText.parse(
                action = Intent.ACTION_SEND,
                type = "text/plain",
                text = " https://example.com ",
                subject = " Example ",
            ),
        )
    }

    @Test
    fun `plain text shares preserve text without org link formatting`() {
        assertEquals(
            "buy milk ",
            ShareIntentText.parse(
                action = Intent.ACTION_SEND,
                type = "text/plain",
                text = " buy milk ",
                subject = "Errand",
            ),
        )
    }

    @Test
    fun `url without subject stays plain`() {
        assertEquals(
            "https://example.com ",
            ShareIntentText.parse(
                action = Intent.ACTION_SEND,
                type = "text/plain",
                text = "https://example.com",
                subject = " ",
            ),
        )
    }

    @Test
    fun `ignores non send actions`() {
        assertNull(
            ShareIntentText.parse(
                action = Intent.ACTION_VIEW,
                type = "text/plain",
                text = "hello",
                subject = null,
            ),
        )
    }

    @Test
    fun `ignores non text mime types`() {
        assertNull(
            ShareIntentText.parse(
                action = Intent.ACTION_SEND,
                type = "image/png",
                text = "hello",
                subject = null,
            ),
        )
    }

    @Test
    fun `ignores blank text`() {
        assertNull(
            ShareIntentText.parse(
                action = Intent.ACTION_SEND,
                type = "text/plain",
                text = "   ",
                subject = "Blank",
            ),
        )
    }
}
