{-# LANGUAGE OverloadedStrings #-}

-- | One-shot rebuild of the @task_current_state@ read model from the
-- @events@ table.
--
-- Useful when:
--
--   * The materialized table has drifted (bug, manual repair, schema bump).
--   * A fresh DB came in via clone but the events table was repopulated by
--     a sync push without the trigger having seen each insert.
--   * After a schema-evolving migration on the read model that needs
--     re-projection from the canonical event log.
--
-- Implementation: a single transaction that deletes every row from
-- @task_current_state@ and bulk-projects from @events@ — same latest-
-- non-NULL-per-field SQL the trigger uses, but applied in one statement
-- across all distinct (file_path, task_index) pairs. Bypassing the per-
-- event trigger fire keeps this O(distinct tasks) rather than O(events).
module Commands.RebuildState
  ( dbRebuildStateCommand,
  )
where

import Commands.CliHelpers (loadConfig)
import Commands.Command (Command (..))
import qualified Data.Text as T
import DB.Connection (initDatabase, withDatabase)
import Database.SQLite.Simple (Only (..), query_)
import Model.OrgMode (Task)
import Repo.EventStoreRepo (rebuildTaskCurrentState)
import Tui.Types (AppConfig (..))

dbRebuildStateCommand :: Command Task
dbRebuildStateCommand =
  Command
    { cmdName = "Rebuild Read Model",
      cmdAlias = "dbRebuildState",
      cmdDescription =
        T.unwords
          [ "Drop and rebuild task_current_state from the events log.",
            "Use after schema changes, sync push, or to repair drift."
          ],
      cmdTui = Nothing,
      cmdCli = Just $ pure $ do
        conf <- loadConfig
        let dbFile = _database conf
        initDatabase dbFile
        (eventCount, taskCount) <- withDatabase dbFile $ \conn -> do
          rebuildTaskCurrentState conn
          ec <- query_ conn "SELECT COUNT(*) FROM events" :: IO [Only Int]
          tc <- query_ conn "SELECT COUNT(*) FROM task_current_state" :: IO [Only Int]
          pure (extract ec, extract tc)
        putStrLn $
          "dbRebuildState: rebuilt "
            <> show taskCount
            <> " task rows from "
            <> show eventCount
            <> " events",
      cmdApi = Nothing
    }
  where
    extract :: [Only Int] -> Int
    extract (Only n : _) = n
    extract [] = 0
