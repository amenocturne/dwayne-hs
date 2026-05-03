package com.skril.dwayne.ui.navigation

sealed class Screen(val route: String) {
    data object Feed : Screen("feed")
    data object Capture : Screen("capture")
    data object Swipe : Screen("swipe")
    data object Search : Screen("search")
    data object Settings : Screen("settings")
}
