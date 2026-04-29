{-# LANGUAGE OverloadedStrings #-}

module DB.Import
  ( importFileState,
    saveFileState,
    importTask,
  )
where

import Core.Types (FileState)
import DB.Query (deleteAllTasksQuery, deleteTasksByFileQuery, insertTaskQuery)
import DB.TaskRow (DBTask (..))
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Database.SQLite.Simple
import Database.SQLite.Simple.Types ((:.) (..))
import Model.OrgMode (Task, TaskFile (..))
import Parser.Parser (ParserResult (..))

-- | Wholesale import: wipe the entire tasks table, then insert every task
-- from the given FileState. Used by `dwayne dbImport` to seed/refresh the
-- DB from org files. Not safe for incremental writes — pass a partial
-- FileState here and you will lose tasks for the files you omitted.
importFileState :: Connection -> FileState Task -> IO Int
importFileState conn fs = do
  execute_ conn deleteAllTasksQuery
  let files = M.toList fs
  counts <- mapM (importFile conn) files
  pure (sum counts)

-- | Per-file replace: delete tasks for each file in the given FileState,
-- then insert the new tasks. Tasks belonging to files not present in the
-- FileState are left untouched. This is the safe path for runtime writes
-- (TUI :w, API mutations).
saveFileState :: Connection -> FileState Task -> IO Int
saveFileState conn fs =
  withTransaction conn $ do
    let files = M.toList fs
    counts <- mapM (replaceFile conn) files
    pure (sum counts)

replaceFile :: Connection -> (FilePath, ParserResult (TaskFile Task)) -> IO Int
replaceFile conn (fp, ParserSuccess (TaskFile _ tasks)) = do
  execute conn deleteTasksByFileQuery (Only fp)
  let indexed = zip [0 ..] (V.toList tasks)
  mapM_ (\(idx, task) -> importTask conn fp idx task) indexed
  pure (length indexed)
replaceFile _ (_, ParserFailure _) = pure 0

importFile :: Connection -> (FilePath, ParserResult (TaskFile Task)) -> IO Int
importFile conn (fp, ParserSuccess (TaskFile _ tasks)) = do
  let indexed = zip [0 ..] (V.toList tasks)
  mapM_ (\(idx, task) -> importTask conn fp idx task) indexed
  pure (length indexed)
importFile _ (_, ParserFailure _) = pure 0

importTask :: Connection -> FilePath -> Int -> Task -> IO ()
importTask conn fp idx task =
  execute
    conn
    insertTaskQuery
    ((fp, idx) :. DBTask task)
