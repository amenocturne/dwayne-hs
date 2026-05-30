package com.skril.dwayne.ui.navigation

import android.net.Uri

sealed class Screen(val route: String) {
    data object Feed : Screen("feed")
    data object Capture : Screen("capture")
    data object Swipe : Screen("swipe")
    data object Search : Screen("search")
    data object Settings : Screen("settings")
    data object TreeBuilder : Screen("tree-builder")
    data object TaskDetail : Screen("task?file={file}&taskIndex={taskIndex}") {
        fun route(file: String, taskIndex: Int): String =
            "task?file=${Uri.encode(file)}&taskIndex=$taskIndex"
    }
}
