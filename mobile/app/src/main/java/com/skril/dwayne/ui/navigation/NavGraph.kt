package com.skril.dwayne.ui.navigation

import androidx.compose.foundation.layout.padding
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.automirrored.filled.List
import androidx.compose.material.icons.filled.SwipeRight
import androidx.compose.material3.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.navigation.NavDestination.Companion.hierarchy
import androidx.navigation.NavGraph.Companion.findStartDestination
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.currentBackStackEntryAsState
import androidx.navigation.compose.rememberNavController
import com.skril.dwayne.data.repository.MockTaskRepository
import com.skril.dwayne.ui.screens.capture.CaptureScreen
import com.skril.dwayne.ui.screens.feed.TaskFeedScreen
import com.skril.dwayne.ui.screens.swipe.SwipeProcessingScreen

private data class BottomNavItem(
    val screen: Screen,
    val label: String,
    val icon: ImageVector,
)

private val bottomNavItems = listOf(
    BottomNavItem(Screen.Feed, "Feed", Icons.AutoMirrored.Default.List),
    BottomNavItem(Screen.Capture, "Capture", Icons.Default.Add),
    BottomNavItem(Screen.Swipe, "Process", Icons.Default.SwipeRight),
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun DwayneNavHost() {
    val navController = rememberNavController()
    val repository = remember { MockTaskRepository() }

    Scaffold(
        bottomBar = {
            NavigationBar {
                val navBackStackEntry by navController.currentBackStackEntryAsState()
                val currentDestination = navBackStackEntry?.destination
                bottomNavItems.forEach { item ->
                    NavigationBarItem(
                        icon = { Icon(item.icon, contentDescription = item.label) },
                        label = { Text(item.label) },
                        selected = currentDestination?.hierarchy?.any { it.route == item.screen.route } == true,
                        onClick = {
                            navController.navigate(item.screen.route) {
                                popUpTo(navController.graph.findStartDestination().id) {
                                    saveState = true
                                }
                                launchSingleTop = true
                                restoreState = true
                            }
                        },
                    )
                }
            }
        },
    ) { innerPadding ->
        NavHost(
            navController = navController,
            startDestination = Screen.Feed.route,
            modifier = Modifier.padding(innerPadding),
        ) {
            composable(Screen.Feed.route) {
                TaskFeedScreen(repository = repository)
            }
            composable(Screen.Capture.route) {
                CaptureScreen(repository = repository)
            }
            composable(Screen.Swipe.route) {
                SwipeProcessingScreen(repository = repository)
            }
        }
    }
}
