package com.skril.dwayne

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import com.skril.dwayne.ui.navigation.DwayneNavHost
import com.skril.dwayne.ui.theme.DwayneTheme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()
        setContent {
            DwayneTheme {
                DwayneNavHost()
            }
        }
    }
}
