{-# LANGUAGE OverloadedStrings #-}

module DB.Import
  ( importFileState,
    importTask,
  )
where

import Core.Types (FileState)
import DB.Query (deleteAllTasksQuery, insertTaskQuery)
import DB.TaskRow (DBTask (..))
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Database.SQLite.Simple
import Database.SQLite.Simple.Types ((:.) (..))
import Model.OrgMode (Task, TaskFile (..))
import Parser.Parser (ParserResult (..))

importFileState :: Connection -> FileState Task -> IO Int
importFileState conn fs = do
  execute_ conn deleteAllTasksQuery
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
    insertTaskQuery
    ((fp, idx) :. DBTask task)
