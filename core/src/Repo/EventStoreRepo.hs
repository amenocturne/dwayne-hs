{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | SQLite implementation of 'Repo.TaskRepo' over the events log + the
-- materialized @task_current_state@ table maintained by the
-- @events_to_state@ trigger.
--
--   * Reads (getTask, queryTasks, countTasks) hit @task_current_state@.
--   * Writes (appendEvent, appendEvents) hit @events@; the trigger
--     projects them into @task_current_state@ atomically.
--
-- The repo carries a @dbFile@ path and opens a fresh connection per call
-- via 'DB.Connection.withDatabase'. This matches the existing pattern in
-- 'Commands.MigrateToEvents' and friends; SQLite is single-writer-friendly
-- and the cost of opening is dominated by the WAL setup. A future
-- optimization could thread a long-lived connection.
module Repo.EventStoreRepo
  ( EventStoreRepo (..),
    mkEventStoreRepo,
    nextTaskIndex,
    rebuildTaskCurrentState,
    loadAllTasks,
  )
where

import Core.Nullable (Nullable (..))
import Core.Types (TaskPointer (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple hiding (Query)
import qualified Database.SQLite.Simple as SQL
import DB.Connection (withDatabase)
import DB.Schema (projectStateFromEventsSql)
import qualified Events.Store as ES
import Events.Types (Event)
import Model.OrgMode (OrgTime, RichText, Task (..))
import Repo.TaskRepo
  ( Query (..),
    SortField (..),
    TaskRepo (..),
    View (..),
  )

-- | Repository handle. Carries the DB file path and the inbox file path
-- (needed to map 'ViewInbox' → @file_path = ?@). Connections are opened
-- per operation.
data EventStoreRepo = EventStoreRepo
  { esrDbFile :: FilePath,
    esrInboxFile :: FilePath
  }

mkEventStoreRepo :: FilePath -> FilePath -> EventStoreRepo
mkEventStoreRepo = EventStoreRepo

-- ---------------------------------------------------------------------------
-- TaskRepo instance
-- ---------------------------------------------------------------------------

instance TaskRepo EventStoreRepo IO where
  getTask repo (fp, idx) =
    withDatabase (esrDbFile repo) $ \conn -> getTaskConn conn fp idx

  queryTasks repo q =
    map fst <$> withDatabase (esrDbFile repo) (\conn -> queryTasksWithPointersConn conn (esrInboxFile repo) q)

  queryTasksWithPointers repo q =
    withDatabase (esrDbFile repo) $ \conn -> queryTasksWithPointersConn conn (esrInboxFile repo) q

  countTasks repo q =
    withDatabase (esrDbFile repo) $ \conn -> countTasksConn conn (esrInboxFile repo) q

  appendEvent repo e =
    withDatabase (esrDbFile repo) $ \conn -> ES.insertEvent conn e

  appendEvents repo evs =
    withDatabase (esrDbFile repo) $ \conn -> ES.insertEvents conn evs

-- | Next free @task_index@ for a file, computed from the events log.
-- The capture handler uses this to pick the index for a freshly created
-- task without relying on an in-memory 'FileState' projection. Returns
-- @MAX(task_index) + 1@ for the file, or @0@ if no events exist.
nextTaskIndex :: EventStoreRepo -> FilePath -> IO Int
nextTaskIndex repo fp =
  withDatabase (esrDbFile repo) $ \conn -> ES.nextTaskIndex conn fp

-- ---------------------------------------------------------------------------
-- Connection-level operations (used by the instance and tests)
-- ---------------------------------------------------------------------------

-- | Look up a single task by (file_path, task_index). Returns 'Nothing'
-- when no row exists in @task_current_state@.
getTaskConn :: Connection -> FilePath -> Int -> IO (Maybe Task)
getTaskConn conn fp idx = do
  rows <-
    query
      conn
      ("SELECT " <> stateColumns <> " FROM task_current_state WHERE file_path = ? AND task_index = ?")
      (T.pack fp, idx) ::
      IO [StateRow]
  pure $ case rows of
    [] -> Nothing
    (r : _) -> Just (rowToTask r)

-- | Run a structured query against @task_current_state@. Returns the
-- task value paired with its 'TaskPointer'; callers that don't need the
-- pointer can drop it (see the 'TaskRepo' instance).
queryTasksWithPointersConn :: Connection -> FilePath -> Query -> IO [(Task, TaskPointer)]
queryTasksWithPointersConn conn inboxFp q = do
  let (whereClause, params) = buildWhere inboxFp q
      orderClause = buildOrder (qSortBy q)
      limitClause = buildLimit (qLimit q) (qOffset q)
      sql =
        SQL.Query $
          "SELECT "
            <> renderQuery stateColumns
            <> " FROM task_current_state"
            <> whereClause
            <> orderClause
            <> limitClause
  rows <- query conn sql params :: IO [StateRow]
  pure (map rowToTaskWithPointer rows)

-- | Count rows matching the query (ignores 'qLimit' and 'qOffset').
countTasksConn :: Connection -> FilePath -> Query -> IO Int
countTasksConn conn inboxFp q = do
  let (whereClause, params) = buildWhere inboxFp q
      sql = SQL.Query $ "SELECT COUNT(*) FROM task_current_state" <> whereClause
  rows <- query conn sql params :: IO [Only Int]
  pure $ case rows of
    (Only n : _) -> n
    [] -> 0

-- | Drop and rebuild @task_current_state@ from the events log. Used by
-- 'Commands.RebuildState' as a one-shot repair / rebuild step.
--
-- Implementation: clear the read model in the same transaction, then
-- run 'projectStateFromEventsSql' to bulk-project from events. The
-- bulk SQL bypasses the per-event trigger fire and runs as one
-- statement across all touched tasks. The same SQL is also appended
-- to migration 004 so an upgrade from v3 backfills in one shot.
rebuildTaskCurrentState :: Connection -> IO ()
rebuildTaskCurrentState conn = withTransaction conn $ do
  execute_ conn "DELETE FROM task_current_state"
  execute_ conn projectStateFromEventsSql

-- | Load every materialized task in a single round-trip. Used by the TUI
-- bootstrap to avoid the N+1 'getTask'-per-pointer pattern, which on
-- macOS can take 5+ seconds for ~13k rows because each call opens its
-- own SQLite connection.
loadAllTasks :: EventStoreRepo -> IO [(Task, TaskPointer)]
loadAllTasks repo =
  withDatabase (esrDbFile repo) $ \conn -> do
    rows <-
      query_
        conn
        ( SQL.Query $
            "SELECT "
              <> renderQuery stateColumns
              <> " FROM task_current_state ORDER BY file_path, task_index"
        ) ::
        IO [StateRow]
    pure (map rowToTaskWithPointer rows)

-- ---------------------------------------------------------------------------
-- Query construction helpers
-- ---------------------------------------------------------------------------

buildWhere :: FilePath -> Query -> (T.Text, [SQLData])
buildWhere inboxFp q =
  let viewClauses = viewWhere inboxFp (qView q)
      kwClauses = case qKeyword q of
        Just k -> [(" todo_keyword = ?", [SQLText k])]
        Nothing -> []
      fpClauses = case qFilePath q of
        Just fp -> [(" file_path = ?", [SQLText (T.pack fp)])]
        Nothing -> []
      searchClauses = case qSearchTerm q of
        Just s ->
          [ ( " (title LIKE ? OR description LIKE ?)",
              [SQLText (likePattern s), SQLText (likePattern s)]
            )
          ]
        Nothing -> []
      -- Default: exclude TRASH unless the view explicitly wants it or
      -- the caller has opted in via 'qIncludeTrash'.
      trashClause
        | qView q == Just ViewTrash = []
        | qIncludeTrash q = []
        | otherwise = [(" (todo_keyword IS NULL OR todo_keyword <> 'TRASH')", [])]
      pieces = viewClauses ++ kwClauses ++ fpClauses ++ searchClauses ++ trashClause
   in case pieces of
        [] -> ("", [])
        _ ->
          let (texts, paramLists) = unzip pieces
              joined = T.intercalate " AND " (map T.strip texts)
           in (" WHERE " <> joined, concat paramLists)

likePattern :: T.Text -> T.Text
likePattern s = T.concat ["%", s, "%"]

viewWhere :: FilePath -> Maybe View -> [(T.Text, [SQLData])]
viewWhere _ Nothing = []
viewWhere _ (Just ViewAll) = []
viewWhere inboxFp (Just ViewInbox) = [(" file_path = ?", [SQLText (T.pack inboxFp)])]
viewWhere _ (Just ViewDefer) = kwOnly "DEFER"
viewWhere _ (Just ViewToday) = kwOnly "TODAY"
viewWhere _ (Just ViewSoon) = kwOnly "SOON"
viewWhere _ (Just ViewTodo) = kwOnly "TODO"
viewWhere _ (Just ViewDone) = kwOnly "DONE"
viewWhere _ (Just ViewSomeday) = kwOnly "SOMEDAY"
viewWhere _ (Just ViewWaiting) = kwOnly "WAITING"
viewWhere _ (Just ViewProject) = kwOnly "PROJECT"
viewWhere _ (Just ViewRelevant) = kwOnly "RELEVANT"
viewWhere _ (Just ViewNotes) = kwOnly "NOTES"
viewWhere _ (Just ViewList) = kwOnly "LIST"
viewWhere _ (Just ViewWorkQueue) = [(" todo_keyword IN ('TODAY', 'SOON')", [])]
viewWhere _ (Just ViewTrash) = kwOnly "TRASH"

kwOnly :: T.Text -> [(T.Text, [SQLData])]
kwOnly k = [(" todo_keyword = ?", [SQLText k])]

buildOrder :: SortField -> T.Text
buildOrder SortNoOrder = ""
buildOrder SortPriority = " ORDER BY priority IS NULL, priority ASC"
buildOrder SortDeadline = " ORDER BY deadline IS NULL, deadline ASC"
buildOrder SortScheduled = " ORDER BY scheduled IS NULL, scheduled ASC"
buildOrder SortLastEventAt = " ORDER BY last_event_at DESC"
-- Tasks without a 'created' property sort to the bottom of the DESC list.
-- The @created@ column stores the JSON-encoded OrgTime, which preserves
-- ISO-8601-like ordering for typical capture timestamps.
buildOrder SortCreatedDesc = " ORDER BY created IS NULL, created DESC"
-- Mirrors 'Tui.Keybindings.sortByPriorityThenDeadline': priority ASC
-- (NULLs last), then deadline ASC (NULLs last).
buildOrder SortPriorityThenDeadline =
  " ORDER BY priority IS NULL, priority ASC, deadline IS NULL, deadline ASC"
-- Stable file-order traversal needed by the project-tree handler:
-- tasks must come back in their original (file_path, task_index)
-- ordering so the indentation-based parent/child grouping is correct.
buildOrder SortTaskIndex = " ORDER BY file_path ASC, task_index ASC"

buildLimit :: Maybe Int -> Maybe Int -> T.Text
buildLimit Nothing Nothing = ""
buildLimit (Just l) Nothing = " LIMIT " <> tshow l
buildLimit Nothing (Just o) = " LIMIT -1 OFFSET " <> tshow o
buildLimit (Just l) (Just o) = " LIMIT " <> tshow l <> " OFFSET " <> tshow o

tshow :: Int -> T.Text
tshow = T.pack . show

renderQuery :: SQL.Query -> T.Text
renderQuery (SQL.Query t) = t

-- ---------------------------------------------------------------------------
-- Row mapping for task_current_state
-- ---------------------------------------------------------------------------

stateColumns :: SQL.Query
stateColumns =
  "file_path, task_index, level, todo_keyword, priority, title, tags, \
  \scheduled, deadline, created, closed, properties, description, last_event_at"

-- | Wire shape for a task_current_state row. We don't need file_path /
-- task_index / last_event_at for 'Task' construction but we read them out
-- to keep the column order stable and parsing trivial.
type StateRow =
  ( T.Text, -- file_path
    Int, -- task_index
    Int, -- level
    Maybe T.Text, -- todo_keyword
    Maybe Int, -- priority
    Maybe T.Text, -- title (json richtext, nullable per schema but always set in practice)
    Maybe T.Text -- tags (json array)
  )
    :. ( Maybe T.Text, -- scheduled
         Maybe T.Text, -- deadline
         Maybe T.Text, -- created
         Maybe T.Text, -- closed
         Maybe T.Text, -- properties (json)
         Maybe T.Text, -- description (json richtext)
         T.Text -- last_event_at
       )

rowToTask :: StateRow -> Task
rowToTask = fst . rowToTaskWithPointer

rowToTaskWithPointer :: StateRow -> (Task, TaskPointer)
rowToTaskWithPointer ((fp, idx, lvl, kw, pri, ttl, tgs) :. (sch, dl, crt, cls, props, desc, _last)) =
  let task =
        Task
          { _level = lvl,
            _todoKeyword = case kw of
              Just k -> k
              Nothing -> "",
            _priority = case pri of
              Nothing -> Nothing
              Just v -> if isNull v then Nothing else Just v,
            _title = maybe (nullValue :: RichText) decodeRichText ttl,
            _tags = S.fromList (maybe [] decodeTags tgs),
            _scheduled = decodeNullableTime sch,
            _deadline = decodeNullableTime dl,
            _createdProp = decodeNullableTime crt,
            _closed = decodeNullableTime cls,
            _properties = maybe [] decodeProps props,
            _description = maybe (nullValue :: RichText) decodeRichText desc
          }
   in (task, TaskPointer (T.unpack fp) idx)

decodeNullableTime :: Maybe T.Text -> Maybe OrgTime
decodeNullableTime Nothing = Nothing
decodeNullableTime (Just t) =
  let o = decodeOrgTime t
   in if isNull o then Nothing else Just o

-- ---------------------------------------------------------------------------
-- JSON decoding helpers (mirror Events.Store conventions)
-- ---------------------------------------------------------------------------

decodeRichText :: T.Text -> RichText
decodeRichText t =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right rt -> rt
    Left _ -> nullValue

decodeTags :: T.Text -> [T.Text]
decodeTags t =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right xs -> xs
    Left _ -> []

decodeProps :: T.Text -> [(T.Text, T.Text)]
decodeProps t =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right pairs -> map toTuple pairs
    Left _ -> []
  where
    toTuple [k, v] = (k, v)
    toTuple _ = ("", "")

decodeOrgTime :: T.Text -> OrgTime
decodeOrgTime t =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right o -> o
    Left _ -> (nullValue :: OrgTime)
