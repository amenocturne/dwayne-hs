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
import Core.Types (FileState, TaskPointer (..))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Model.OrgMode (Task, TaskFile (..))
import Parser.Parser (ParserResult (..))
import Repo.EventStoreRepo (EventStoreRepo, loadAllTasks)
import System.Exit (die)
import Tui.Types (SystemConfig, taskRepo)

-- | Materialize a 'FileState Task' by reading the CQRS read model
-- (@task_current_state@) through the repo, in a single SQL round-trip.
--
-- The resulting 'FileState' groups tasks by their @file_path@ and
-- preserves task_index ordering inside each file; this is the same shape
-- the legacy event-fold produced and the same shape the rest of the TUI
-- expects.
loadFileStateFromRepo :: EventStoreRepo -> IO (FileState Task)
loadFileStateFromRepo repo = do
  rows <- loadAllTasks repo
  let groups :: M.Map FilePath [(Int, Task)]
      groups =
        M.fromListWith
          (++)
          [ (fp, [(idx, t)])
            | (t, TaskPointer fp idx) <- rows
          ]
  pure (M.map toEntry groups)
  where
    toEntry :: [(Int, Task)] -> ParserResult (TaskFile Task)
    toEntry items =
      let sorted = L.sortOn fst items
          tasks = V.fromList (map snd sorted)
       in ParserSuccess (TaskFile Nothing tasks)

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

