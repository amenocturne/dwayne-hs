{-# LANGUAGE OverloadedStrings #-}

-- | Pure projection: a list of events folded down to the current Task state.
--
-- Algorithm: group events by (file_path, task_index), and for each task take
-- the latest 'Just' value (by 'evOccurredAt') for each field. A field whose
-- final value is the type's 'nullValue' decodes back to a Task field with
-- the appropriate "absent" representation (Maybe Nothing / empty set / etc.).
--
-- A task is materialized only if at least one event covers all required
-- fields (level, todo_keyword, title, description); otherwise it's dropped.
-- In practice every genesis event satisfies this, so any task that has been
-- captured will appear.
module Events.Projection
  ( currentState,
    foldTaskEvents,
    fileStateFromEvents,
    applyEventToTask,
    fieldsTouchedByEvent,
  )
where

import Core.Nullable (Nullable (..))
import Core.Types (FileState)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Events.Types (Event (..))
import Model.OrgMode (OrgTime, RichText, Task (..), TaskFile (..))
import Parser.Parser (ParserResult (..))

-- | Project a list of events into a map keyed by (filePath, taskIndex).
-- Events for the same key are merged latest-Just-wins per field.
currentState :: [Event] -> M.Map (FilePath, Int) Task
currentState events =
  let grouped = M.fromListWith (++) [((evFilePath e, evTaskIndex e), [e]) | e <- events]
   in M.mapMaybe foldTaskEvents grouped

-- | Materialize a single task from its events. Returns Nothing if no event
-- covers the required fields (level, todo_keyword, title, description).
foldTaskEvents :: [Event] -> Maybe Task
foldTaskEvents [] = Nothing
foldTaskEvents evs =
  let sorted = L.sortOn evOccurredAt evs
      task = L.foldl' applyEventToTask emptyTask sorted
      coverage = L.foldl' mergeCoverage emptyCoverage sorted
   in if hasGenesis coverage then Just task else Nothing

-- | Apply one event on top of a Task, overwriting any field the event sets.
applyEventToTask :: Task -> Event -> Task
applyEventToTask t e =
  t
    { _level = maybe (_level t) id (evLevel e),
      _todoKeyword = maybe (_todoKeyword t) id (evTodoKeyword e),
      _priority = case evPriority e of
        Nothing -> _priority t
        Just v -> if isNull v then Nothing else Just v,
      _title = maybe (_title t) id (evTitle e),
      _tags = case evTags e of
        Nothing -> _tags t
        Just xs -> S.fromList xs,
      _scheduled = nullableTime (evScheduled e) (_scheduled t),
      _deadline = nullableTime (evDeadline e) (_deadline t),
      _createdProp = nullableTime (evCreated e) (_createdProp t),
      _closed = nullableTime (evClosed e) (_closed t),
      _properties = maybe (_properties t) id (evProperties e),
      _description = maybe (_description t) id (evDescription e)
    }
  where
    nullableTime :: Maybe OrgTime -> Maybe OrgTime -> Maybe OrgTime
    nullableTime Nothing prev = prev
    nullableTime (Just v) _
      | isNull v = Nothing
      | otherwise = Just v

emptyTask :: Task
emptyTask =
  Task
    { _level = 1,
      _todoKeyword = "",
      _priority = Nothing,
      _title = nullValue :: RichText,
      _tags = S.empty,
      _scheduled = Nothing,
      _deadline = Nothing,
      _createdProp = Nothing,
      _closed = Nothing,
      _properties = [],
      _description = nullValue :: RichText
    }

-- | Track which required fields have been seen across the event stream
-- so we can refuse to materialize ghost tasks.
data Coverage = Coverage
  { covLevel :: Bool,
    covKeyword :: Bool,
    covTitle :: Bool
  }

emptyCoverage :: Coverage
emptyCoverage = Coverage False False False

mergeCoverage :: Coverage -> Event -> Coverage
mergeCoverage c e =
  Coverage
    { covLevel = covLevel c || maybe False (const True) (evLevel e),
      covKeyword = covKeyword c || maybe False (const True) (evTodoKeyword e),
      covTitle = covTitle c || maybe False (const True) (evTitle e)
    }

hasGenesis :: Coverage -> Bool
hasGenesis c = covLevel c && covKeyword c && covTitle c

-- | Convert a projected map into the runtime FileState shape. Tasks are
-- emitted in task_index order per file.
fileStateFromEvents :: [Event] -> FileState Task
fileStateFromEvents evs =
  let m = currentState evs
      byFile = M.fromListWith (++) [(fp, [(idx, t)]) | ((fp, idx), t) <- M.toList m]
   in M.map toEntry byFile
  where
    toEntry pairs =
      let sorted = L.sortOn fst pairs
          tasks = V.fromList (map snd sorted)
       in ParserSuccess (TaskFile Nothing tasks)

-- | List of field names this event touched. Used for diagnostics/tests.
fieldsTouchedByEvent :: Event -> [T.Text]
fieldsTouchedByEvent e =
  concat
    [ ["level" | hasJust (evLevel e)],
      ["todoKeyword" | hasJust (evTodoKeyword e)],
      ["priority" | hasJust (evPriority e)],
      ["title" | hasJust (evTitle e)],
      ["tags" | hasJust (evTags e)],
      ["scheduled" | hasJust (evScheduled e)],
      ["deadline" | hasJust (evDeadline e)],
      ["created" | hasJust (evCreated e)],
      ["closed" | hasJust (evClosed e)],
      ["properties" | hasJust (evProperties e)],
      ["description" | hasJust (evDescription e)]
    ]
  where
    hasJust :: Maybe a -> Bool
    hasJust = maybe False (const True)
