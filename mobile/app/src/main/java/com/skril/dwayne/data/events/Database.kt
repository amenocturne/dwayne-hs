package com.skril.dwayne.data.events

import android.content.Context
import app.cash.sqldelight.driver.android.AndroidSqliteDriver
import com.skril.dwayne.db.DwayneDatabase

/** Lazy singleton SQLDelight database for the app. */
object Database {

    @Volatile private var instance: DwayneDatabase? = null

    fun get(context: Context): DwayneDatabase {
        instance?.let { return it }
        return synchronized(this) {
            instance ?: build(context.applicationContext).also { instance = it }
        }
    }

    private fun build(appContext: Context): DwayneDatabase {
        val driver = AndroidSqliteDriver(
            schema = DwayneDatabase.Schema,
            context = appContext,
            name = "dwayne-events.db",
        )
        return DwayneDatabase(driver)
    }
}
