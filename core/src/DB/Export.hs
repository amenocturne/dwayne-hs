{-# LANGUAGE OverloadedStrings #-}

-- | Read a 'FileState' projected from the events log and (optionally)
-- mirror it back to org files on disk.
--
-- Before Phase 3 cleanup this module read the legacy @tasks@ table that
-- the in-memory TUI cache used to persist into. The events log is now
-- the canonical write model and the @tasks@ table has been dropped
-- (migration 005). 'loadTasksFromDB' therefore folds events through the
-- pure projection — same shape as the TUI repo path
-- (@Repo.EventStoreRepo@), but presented as a 'FileState' so existing
-- consumers (org export, the @DatabaseStore@ TaskStore) keep working.
module DB.Export
  ( loadTasksFromDB,
    exportToOrgFiles,
  )
where

import Core.Types (FileState)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import Database.SQLite.Simple
import Events.Projection (fileStateFromEvents)
import Events.Store (selectAllEvents)
import Model.OrgMode (Task)
import Parser.Parser (ParserResult (..))
import Writer.OrgWriter ()
import Writer.Writer (Writer (..))

-- | Project the events log into a 'FileState Task'. The events log is
-- the canonical write model (see @DB.Schema.eventsSchema@); the legacy
-- @tasks@ table was dropped in migration 005.
loadTasksFromDB :: Connection -> IO (FileState Task)
loadTasksFromDB conn = do
  events <- selectAllEvents conn
  pure (fileStateFromEvents events)

-- | Re-render every file in the projected 'FileState' as org-mode text.
-- ParserFailure entries (files that failed to parse on import, which
-- shouldn't appear in an event-sourced FileState anyway) are silently
-- skipped.
exportToOrgFiles :: Connection -> IO ()
exportToOrgFiles conn = do
  fs <- loadTasksFromDB conn
  mapM_ writeFile' (M.toList fs)
  where
    writeFile' :: (Writer a) => (FilePath, ParserResult a) -> IO ()
    writeFile' (path, ParserSuccess a) = TIO.writeFile path (write a)
    writeFile' (_, ParserFailure _) = pure ()
