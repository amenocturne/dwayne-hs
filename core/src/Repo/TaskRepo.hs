{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Abstraction over the CQRS read model + event log.
--
-- Implementations satisfy this contract:
--
--   * 'getTask' is a single-row lookup keyed by (file_path, task_index).
--   * 'queryTasks' / 'countTasks' run against the materialized
--     @task_current_state@ table; they are O(rows-matching-filter).
--   * 'appendEvent' / 'appendEvents' write to the @events@ table; the
--     SQLite trigger projects them into @task_current_state@ atomically.
--
-- Phase 1 establishes this seam without migrating callers. Consumers move
-- over in Phase 2; both the legacy in-memory 'FileState' path and this
-- repo coexist until then.
module Repo.TaskRepo
  ( View (..),
    SortField (..),
    Query (..),
    emptyQuery,
    TaskRepo (..),
  )
where

import qualified Data.Text as T
import Events.Types (Event)
import Model.OrgMode (Task)

-- | A symbolic view name. Each value maps to a fixed @WHERE@ clause in
-- 'Repo.EventStoreRepo'. View semantics mirror 'Commands.Views':
--
--   * 'ViewInbox' — file_path = inbox file
--   * 'ViewToday' / 'ViewSoon' / 'ViewTodo' / 'ViewDone' / 'ViewSomeday' /
--     'ViewWaiting' / 'ViewProject' / 'ViewRelevant' — todo_keyword match
--   * 'ViewNotes' / 'ViewList' — todo_keyword = NOTES / LIST
--   * 'ViewWorkQueue' — todo_keyword IN (TODAY, SOON)
--   * 'ViewTrash' — todo_keyword = TRASH (the only view that surfaces TRASH)
--   * 'ViewAll' — no extra filter beyond the default TRASH exclusion
--
-- 'qFilePath', 'qKeyword', 'qSearchTerm' compose with the view's WHERE clause.
data View
  = ViewAll
  | ViewInbox
  | ViewToday
  | ViewSoon
  | ViewTodo
  | ViewDone
  | ViewSomeday
  | ViewWaiting
  | ViewProject
  | ViewRelevant
  | ViewNotes
  | ViewList
  | ViewWorkQueue
  | ViewTrash
  deriving (Eq, Show)

-- | Columns the repo can sort by. @SortLastEventAt@ is the analogue of
-- "most recently touched". @SortPriority@ uses ASC by default since lower
-- numbers mean higher priority in the existing semantics.
data SortField
  = SortPriority
  | SortDeadline
  | SortScheduled
  | SortLastEventAt
  | SortNoOrder
  deriving (Eq, Show)

-- | A query against the read model. All fields are optional; the empty
-- query returns every non-TRASH task in unspecified order.
data Query = Query
  { qView :: Maybe View,
    qKeyword :: Maybe T.Text,
    qFilePath :: Maybe FilePath,
    qSearchTerm :: Maybe T.Text,
    qLimit :: Maybe Int,
    qOffset :: Maybe Int,
    qSortBy :: SortField
  }
  deriving (Eq, Show)

-- | A query with no filters and no sort. Useful starting point for
-- incremental construction.
emptyQuery :: Query
emptyQuery =
  Query
    { qView = Nothing,
      qKeyword = Nothing,
      qFilePath = Nothing,
      qSearchTerm = Nothing,
      qLimit = Nothing,
      qOffset = Nothing,
      qSortBy = SortNoOrder
    }

-- | Read/write contract for tasks. Parameterised on the repo handle @r@
-- and the effect monad @m@.
class (Monad m) => TaskRepo r m where
  getTask :: r -> (FilePath, Int) -> m (Maybe Task)
  queryTasks :: r -> Query -> m [Task]
  countTasks :: r -> Query -> m Int
  appendEvent :: r -> Event -> m ()
  appendEvents :: r -> [Event] -> m Int
