{-# LANGUAGE OverloadedStrings #-}

-- | Pluggable backend for the legacy 'TaskStore' interface used by the
-- TUI's reload and save paths.
--
-- After Phase 3 cleanup the events log is the canonical write model and
-- @task_current_state@ the canonical read model. The 'DatabaseStore'
-- variant therefore /reads/ tasks via 'loadTasksFromDB' (a thin shim
-- around the events projection) and /writes/ by mirroring each affected
-- file out to its org file. Mutations themselves are append-only events,
-- emitted directly by the TUI mutation hooks (see
-- 'Tui.MutationEvents.emitMutationEvents'); this seam exists only to
-- keep the file-system mirror in sync with the in-memory FileState that
-- Brick still renders from.
module DB.TaskStore
  ( TaskStore (..),
    OrgFileStore (..),
    DatabaseStore (..),
    mkTaskStoreOps,
  )
where

import Control.Exception (IOException, try)
import Core.Types (FileState, TaskStoreOps (..))
import DB.Connection (withDatabase)
import DB.Export (loadTasksFromDB)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import Model.OrgMode (Task, TaskFile)
import Parser.Parser (Parser, ParserResult (..), runParser)
import System.IO (hPutStrLn, stderr)
import TextUtils (readFileExample)
import Writer.OrgWriter ()
import Writer.Writer (Writer (..))

class TaskStore s where
  loadTasks :: s -> IO (FileState Task)
  saveTasks :: s -> FileState Task -> IO ()

-- | Org file backend: reads/writes org files directly
data OrgFileStore = OrgFileStore
  { orgFiles :: [FilePath],
    orgParser :: Parser (TaskFile Task)
  }

instance TaskStore OrgFileStore where
  loadTasks store = do
    pairs <- mapM readOne (orgFiles store)
    pure $ M.fromList pairs
    where
      readOne fp = do
        txt <- readFileExample fp
        let (_, _, result) = runParser (orgParser store) txt
        pure (fp, result)

  -- \| Writes every entry in the FileState to disk.
  -- The caller is responsible for passing only changed entries
  -- if incremental save is desired (see TUI SaveAllFiles handler).
  saveTasks store fs =
    mapM_ writeOne (M.toList fs)
    where
      writeOne (fp, ParserSuccess taskFile) =
        TIO.writeFile fp (write taskFile)
      writeOne (_, ParserFailure _) =
        pure ()

-- | SQLite backend wired against the events log + read model. Loads via
-- 'loadTasksFromDB' (events projection) and persists by mirroring the
-- given subset of the FileState to org files. The DB itself is not
-- touched here — events have already been emitted by the time we save.
data DatabaseStore = DatabaseStore
  { dbPath :: FilePath
  }

instance TaskStore DatabaseStore where
  loadTasks store =
    withDatabase (dbPath store) $ \conn ->
      loadTasksFromDB conn

  -- The events log is the canonical write target. The TUI's mutation
  -- hooks emit events directly, so this method's only job is to keep
  -- the on-disk org files in step with the in-memory FileState. We
  -- log but don't surface IO failures: a partial mirror failure should
  -- not make the user think their event-sourced edits were lost.
  saveTasks _ fs =
    mapM_ writeOrgFile (M.toList fs)
    where
      writeOrgFile (fp, ParserSuccess taskFile) = do
        result <- try (TIO.writeFile fp (write taskFile)) :: IO (Either IOException ())
        case result of
          Right () -> pure ()
          Left e ->
            hPutStrLn stderr $
              "warning: failed to mirror DB write to org file " ++ fp ++ ": " ++ show e
      writeOrgFile (_, ParserFailure _) = pure ()

-- | Create TaskStoreOps closures from any TaskStore instance
mkTaskStoreOps :: (TaskStore s) => s -> TaskStoreOps Task
mkTaskStoreOps s = TaskStoreOps {storeLoad = loadTasks s, storeSave = saveTasks s}
