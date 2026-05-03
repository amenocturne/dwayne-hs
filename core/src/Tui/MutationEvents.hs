{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Compute and emit events for TUI-driven Task mutations.
--
-- The TUI mutates an in-memory @FileState Task@ via Brick's 'Control.Monad.State.modify'.
-- Each high-level command (change keyword, toggle priority, refile, capture, etc.)
-- produces zero or more (file, taskIndex) mutations. To keep the events table in
-- sync, we diff @before@ vs @after@ for each (file, idx) pair and emit one event
-- per change:
--
--   * task present in @after@ only (newly added, e.g. capture / add task) →
--     emit a 'genesisEvent' carrying every field
--   * task present in both, contents differ → emit a delta event via
--     'diffTaskAsEvent'
--   * task present in @before@ only → ignored. The TUI never deletes a task
--     by removing it from the vector; deletion is modeled as setting the
--     @TRASH@ keyword, which surfaces as a delta event in the second case
--     above. (If a future flow does remove a row, that operation would need
--     its own explicit emission.)
--
-- All emissions share the same wall-clock timestamp so a single user action
-- groups under one @occurredAt@ when read back.
module Tui.MutationEvents
  ( computeMutationEvents,
    emitMutationEvents,
    emitMutationEventsForChange,
  )
where

import Control.Exception (SomeException, try)
import Core.Types (FileState)
import qualified Data.Map.Strict as M
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Vector as V
import DB.Connection (withDatabase)
import Events.Diff (diffTaskAsEvent)
import Events.Store (insertEvents)
import Events.Types (Event, genesisEvent)
import Model.OrgMode (Task, TaskFile (..))
import Parser.Parser (ParserResult (..))
import qualified System.IO as IO

-- | Pure: compute the list of events that describe the difference between
-- @oldState@ and @newState@. Only (file, taskIndex) pairs that exist in
-- @newState@ produce events; pairs that disappeared are ignored (see module
-- docs).
computeMutationEvents :: UTCTime -> FileState Task -> FileState Task -> [Event]
computeMutationEvents now oldState newState =
  let newTasksByFile = extractTasks newState
      oldTasksByFile = extractTasks oldState
   in concatMap (eventsForFile now oldTasksByFile) (M.toList newTasksByFile)

eventsForFile ::
  UTCTime ->
  M.Map FilePath (V.Vector Task) ->
  (FilePath, V.Vector Task) ->
  [Event]
eventsForFile now oldTasksByFile (fp, newTasks) =
  case M.lookup fp oldTasksByFile of
    Nothing ->
      -- file was absent (or unparseable) in oldState: every task is genesis
      [ genesisEvent fp idx now t
      | (idx, t) <- zip [0 ..] (V.toList newTasks)
      ]
    Just oldTasks ->
      let n = V.length newTasks
          perIndex idx =
            let newTask = newTasks V.! idx
             in case oldTasks V.!? idx of
                  Nothing -> Just (genesisEvent fp idx now newTask)
                  Just oldTask
                    | oldTask == newTask -> Nothing
                    | otherwise ->
                        Just (diffTaskAsEvent fp idx now oldTask newTask)
       in [e | Just e <- map perIndex [0 .. n - 1]]

-- | Pull the V.Vector Task content out of every parseable file in a FileState.
extractTasks :: FileState Task -> M.Map FilePath (V.Vector Task)
extractTasks = M.mapMaybe go
  where
    go (ParserSuccess (TaskFile _ tasks)) = Just tasks
    go (ParserFailure _) = Nothing

-- | Side-effect: compute events for the (oldState, newState) diff and insert
-- them into the events table at @dbPath@. Failures are logged to stderr but
-- never thrown — we don't want a DB hiccup to crash the TUI mid-edit.
emitMutationEvents :: FilePath -> FileState Task -> FileState Task -> IO ()
emitMutationEvents dbPath oldState newState = do
  now <- getCurrentTime
  let events = computeMutationEvents now oldState newState
  case events of
    [] -> pure ()
    _ -> do
      result <- try $ withDatabase dbPath $ \conn ->
        insertEvents conn events
      case result of
        Right _ -> pure ()
        Left (e :: SomeException) ->
          IO.hPutStrLn IO.stderr $
            "warning: failed to emit "
              ++ show (length events)
              ++ " mutation event(s) to "
              ++ dbPath
              ++ ": "
              ++ show e

-- | Convenience: only emit events when the states actually differ.
emitMutationEventsForChange ::
  FilePath -> FileState Task -> FileState Task -> IO ()
emitMutationEventsForChange dbPath oldState newState
  | oldState == newState = pure ()
  | otherwise = emitMutationEvents dbPath oldState newState
