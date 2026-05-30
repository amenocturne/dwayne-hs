package com.skril.dwayne.ui.theme

import androidx.compose.ui.graphics.Color

val DarkBackground = Color(0xFF121212)
val DarkSurface = Color(0xFF1E1E1E)
val DarkSurfaceVariant = Color(0xFF2A2A2A)

val Primary = Color(0xFF90CAF9)
val PrimaryVariant = Color(0xFF5D99C6)
val Secondary = Color(0xFFCE93D8)
val OnPrimary = Color(0xFF000000)
val OnSurface = Color(0xFFE0E0E0)
val OnSurfaceVariant = Color(0xFF9E9E9E)

val PriorityA = Color(0xFFEF5350)
val PriorityB = Color(0xFFFFA726)
val PriorityC = Color(0xFF66BB6A)

val KeywordInbox = Color(0xFFFFAB40)
val KeywordDefer = Color(0xFF80CBC4)
val KeywordToday = Color(0xFFEF5350)
val KeywordSoon = Color(0xFFFFA726)
val KeywordTodo = Color(0xFF42A5F5)
val KeywordProject = Color(0xFFAB47BC)
val KeywordDone = Color(0xFF66BB6A)
val KeywordWaiting = Color(0xFFBDBDBD)
val KeywordTrash = Color(0xFF757575)

fun keywordColor(keyword: String): Color = when (keyword) {
    "INBOX" -> KeywordInbox
    "DEFER" -> KeywordDefer
    "TODAY" -> KeywordToday
    "SOON" -> KeywordSoon
    "TODO" -> KeywordTodo
    "PROJECT" -> KeywordProject
    "RELEVANT" -> KeywordTodo
    "SOMEDAY" -> KeywordWaiting
    "WAITING" -> KeywordWaiting
    "NOTES" -> OnSurfaceVariant
    "LIST" -> OnSurfaceVariant
    "DONE" -> KeywordDone
    "TRASH" -> KeywordTrash
    else -> OnSurface
}

fun priorityColor(priority: Int?): Color = when (priority) {
    1 -> PriorityA
    2 -> PriorityB
    3 -> PriorityC
    else -> OnSurfaceVariant
}
