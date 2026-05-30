package com.skril.dwayne.data.model

enum class TodoKeyword(val label: String) {
    INBOX("INBOX"),
    DEFER("DEFER"),
    TODAY("TODAY"),
    SOON("SOON"),
    TODO("TODO"),
    PROJECT("PROJECT"),
    RELEVANT("RELEVANT"),
    SOMEDAY("SOMEDAY"),
    WAITING("WAITING"),
    NOTES("NOTES"),
    LIST("LIST"),
    DONE("DONE"),
    TRASH("TRASH");

    companion object {
        fun fromString(s: String): TodoKeyword? = entries.find { it.label == s }
    }
}
