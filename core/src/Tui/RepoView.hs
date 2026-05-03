{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Phase 2c CQRS seam: the TUI reads its visible window through this
-- module instead of folding the entire events log into a 'FileState'
-- in-memory.
--
-- The TUI currently keeps a 'FileState Task' in 'AppContext' so its
-- existing rendering machinery (filtered/sorted pointer vectors, traversal
-- by 'TaskPointer', etc.) keeps working. Phase 2c switches the source of
-- truth for that 'FileState' from "fold every event in memory at startup
-- and after every mutation" to "query the materialized
-- @task_current_state@ table maintained by the SQL trigger".
--
-- The shape of the in-memory cache is unchanged; only the read path is.
-- This is the "fallback" approach described in the spec: keep
-- 'LinearHistory (FileState a)' as a client-side cache populated /from/
-- @queryTasks@ results. After every mutation the TUI re-queries via
-- 'refreshFileStateFromRepo' and replaces the current snapshot in
-- LinearHistory; undo/redo still pop snapshots, the events log itself is
-- never rewritten.
--
-- The function signatures here are deliberately TUI-shaped (return
-- 'FileState Task' rather than '[Task]') so callers don't need to change
-- their lens-based render code in this phase. Phase 3 will collapse this
-- shim once 'FileState' is fully retired from the TUI.
module Tui.RepoView
  ( loadFileStateFromRepo,
    requireTaskRepo,
  )
where

import Control.Lens (view)
import Control.Monad (forM)
import Core.Types (FileState)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import DB.Connection (withDatabase)
import Database.SQLite.Simple (query_)
import Model.OrgMode (Task, TaskFile (..))
import Parser.Parser (ParserResult (..))
import Repo.EventStoreRepo (EventStoreRepo, esrDbFile)
import Repo.TaskRepo (TaskRepo (..))
import System.Exit (die)
import Tui.Types (SystemConfig, taskRepo)

-- | Materialize a 'FileState Task' by reading the CQRS read model
-- (@task_current_state@) through the repo.
--
-- Implementation: list every (file_path, task_index) pair, then issue one
-- 'getTask' per pair. The N round-trips matter only at the personal
-- scale this app targets (low thousands of tasks, single user, WAL-mode
-- SQLite); a future optimisation would batch via a richer repo method.
-- Phase 2c keeps the seam narrow on purpose so Phase 3 can move the
-- batching into the repo without TUI churn.
--
-- Tasks whose 'getTask' returns 'Nothing' (e.g. partial events with no
-- genesis coverage) are skipped, matching the existing
-- 'Events.Projection.fileStateFromEvents' semantics.
--
-- The resulting 'FileState' groups tasks by their @file_path@ and
-- preserves task_index ordering inside each file; this is the same shape
-- the legacy event-fold produced and the same shape the rest of the TUI
-- expects.
loadFileStateFromRepo :: EventStoreRepo -> IO (FileState Task)
loadFileStateFromRepo repo = do
  pointers <- listAllPointers repo
  pairs <- forM pointers $ \(fp, idx) -> do
    mTask <- getTask repo (fp, idx)
    pure $ fmap (\t -> (fp, idx, t)) mTask
  let groups :: M.Map FilePath [(Int, Task)]
      groups =
        M.fromListWith
          (++)
          [ (fp, [(idx, t)])
            | Just (fp, idx, t) <- pairs
          ]
  pure (M.map toEntry groups)
  where
    -- Sort by index ascending, mirror the legacy projection layout.
    toEntry :: [(Int, Task)] -> ParserResult (TaskFile Task)
    toEntry items =
      let sorted = L.sortOn fst items
          tasks = V.fromList (map snd sorted)
       in ParserSuccess (TaskFile Nothing tasks)

-- | Pull every (file_path, task_index) pair from @task_current_state@
-- in a stable order. Direct SQL because the repo's 'queryTasks' returns
-- '[Task]' without surfacing the keys we need to rebuild the FileState
-- map. This is the only place the TUI looks past the 'TaskRepo'
-- abstraction; Phase 3 should subsume it.
listAllPointers :: EventStoreRepo -> IO [(FilePath, Int)]
listAllPointers repo =
  withDatabase (esrDbFile repo) $ \conn -> do
    rows <-
      query_
        conn
        "SELECT file_path, task_index FROM task_current_state \
        \ORDER BY file_path, task_index"
        :: IO [(T.Text, Int)]
    pure [(T.unpack fp, idx) | (fp, idx) <- rows]

-- | Extract the configured 'EventStoreRepo' from a 'SystemConfig', or
-- 'die' with a clear error. The TUI cannot run without the read model;
-- silently degrading would surface as "no tasks visible" with no
-- explanation.
requireTaskRepo :: SystemConfig a -> IO EventStoreRepo
requireTaskRepo sysConf =
  case view taskRepo sysConf of
    Just repo -> pure repo
    Nothing ->
      die
        "ERROR: TUI requires a TaskRepo (database backend) but none was \
        \configured. Check that a database path is set in the config and \
        \that the events store has been initialised \
        \(run 'dwayne migrateToEvents' if migrating from a legacy setup)."

