package com.skril.dwayne.ui.navigation

import androidx.compose.foundation.layout.padding
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.List
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Search
import androidx.compose.material.icons.filled.Settings
import androidx.compose.material.icons.filled.SwipeRight
import androidx.compose.material3.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalContext
import androidx.navigation.NavDestination.Companion.hierarchy
import androidx.navigation.NavGraph.Companion.findStartDestination
import androidx.navigation.NavType
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.currentBackStackEntryAsState
import androidx.navigation.compose.rememberNavController
import androidx.navigation.navArgument
import com.skril.dwayne.BuildConfig
import com.skril.dwayne.DwayneApp
import com.skril.dwayne.data.repository.MockTaskRepository
import com.skril.dwayne.data.repository.SavedQueryStore
import com.skril.dwayne.data.repository.TaskRepository
import com.skril.dwayne.ui.screens.capture.CaptureScreen
import com.skril.dwayne.ui.screens.detail.TaskDetailScreen
import com.skril.dwayne.ui.screens.feed.TaskFeedScreen
import com.skril.dwayne.ui.screens.search.SearchScreen
import com.skril.dwayne.ui.screens.settings.SettingsScreen
import com.skril.dwayne.ui.screens.swipe.SwipeProcessingScreen
import kotlinx.coroutines.launch

private data class BottomNavItem(
    val screen: Screen,
    val label: String,
    val icon: ImageVector,
)

private val bottomNavItems = listOf(
    BottomNavItem(Screen.Feed, "Feed", Icons.AutoMirrored.Default.List),
    BottomNavItem(Screen.Capture, "Capture", Icons.Default.Add),
    BottomNavItem(Screen.Swipe, "Process", Icons.Default.SwipeRight),
    BottomNavItem(Screen.Search, "Search", Icons.Default.Search),
    BottomNavItem(Screen.Settings, "Settings", Icons.Default.Settings),
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun DwayneNavHost(
    initialCaptureText: String? = null,
    onCaptureConsumed: () -> Unit = {},
) {
    val context = LocalContext.current
    val app = context.applicationContext as DwayneApp
    val settingsStore = app.settingsStore
    val savedQueryStore = remember { SavedQueryStore(context) }
    val repository: TaskRepository = remember {
        if (BuildConfig.USE_MOCK_DATA) MockTaskRepository() else app.taskRepository
    }

    // Recompose feeds/searches whenever projection state changes.
    val state by app.taskRepository.state.collectAsState()
    @Suppress("UNUSED_VARIABLE")
    val stateRevision = state.size  // keep state read so collectAsState stays subscribed

    val snackbarHostState = remember { SnackbarHostState() }
    val snackbarScope = rememberCoroutineScope()
    val showError: (String) -> Unit = { message ->
        snackbarScope.launch {
            snackbarHostState.showSnackbar(
                message = message,
                duration = SnackbarDuration.Short,
            )
        }
    }

    val navController = rememberNavController()

    androidx.compose.runtime.LaunchedEffect(initialCaptureText) {
        if (!initialCaptureText.isNullOrEmpty()) {
            navController.navigate(Screen.Capture.route) {
                launchSingleTop = true
                popUpTo(navController.graph.findStartDestination().id) { saveState = true }
            }
        }
    }

    Scaffold(
        snackbarHost = { SnackbarHost(snackbarHostState) },
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
                TaskFeedScreen(
                    repository = repository,
                    refreshKey = state.size,
                    onError = showError,
                    onTaskClick = { pointer ->
                        navController.navigate(Screen.TaskDetail.route(pointer.file, pointer.taskIndex))
                    },
                )
            }
            composable(
                Screen.TaskDetail.route,
                arguments = listOf(
                    navArgument("file") { type = NavType.StringType },
                    navArgument("taskIndex") { type = NavType.IntType },
                ),
            ) { entry ->
                val file = entry.arguments?.getString("file").orEmpty()
                val taskIndex = entry.arguments?.getInt("taskIndex") ?: 0
                TaskDetailScreen(
                    repository = app.taskRepository,
                    file = file,
                    taskIndex = taskIndex,
                    onBack = { navController.popBackStack() },
                    onError = showError,
                )
            }
            composable(Screen.Capture.route) {
                CaptureScreen(
                    repository = repository,
                    onError = showError,
                    initialText = initialCaptureText.orEmpty(),
                    onInitialTextConsumed = onCaptureConsumed,
                )
            }
            composable(Screen.Swipe.route) {
                SwipeProcessingScreen(
                    repository = repository,
                    refreshKey = state.size,
                    onError = showError,
                )
            }
            composable(Screen.Search.route) {
                SearchScreen(repository = repository, store = savedQueryStore, onError = showError)
            }
            composable(Screen.Settings.route) {
                SettingsScreen(store = settingsStore, defaultApiBaseUrl = BuildConfig.API_BASE_URL)
            }
        }
    }
}
