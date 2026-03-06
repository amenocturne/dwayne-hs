{-# LANGUAGE OverloadedStrings #-}

module DB.Import
  ( importFileState,
    importTask,
  )
where

import DB.TaskRow (DBTask (..))
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Database.SQLite.Simple
import Database.SQLite.Simple.Types ((:.) (..))
import Model.OrgMode (Task, TaskFile (..))
import Parser.Parser (ParserResult (..))

type FileState a = M.Map FilePath (ParserResult (TaskFile a))

importFileState :: Connection -> FileState Task -> IO Int
importFileState conn fs = do
  execute_ conn "DELETE FROM tasks"
  let files = M.toList fs
  counts <- mapM (importFile conn) files
  pure (sum counts)

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
    "INSERT INTO tasks (file_path, task_index, level, todo_keyword, priority, \
    \title, tags, scheduled, deadline, created, closed, properties, description) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ((fp, idx) :. DBTask task)
