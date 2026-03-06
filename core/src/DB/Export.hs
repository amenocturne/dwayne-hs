{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module DB.Export
  ( loadTasksFromDB,
    exportToOrgFiles,
  )
where

import DB.TaskRow (DBTask (..))
import Data.List (groupBy)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Database.SQLite.Simple
import Database.SQLite.Simple.Types ((:.) (..))
import Model.OrgMode (Task, TaskFile (..))
import Parser.Parser (ParserResult (..))
import Writer.OrgWriter ()
import Writer.Writer (Writer (..))

type FileState a = M.Map FilePath (ParserResult (TaskFile a))

type DBRow = (T.Text, Int) :. DBTask

loadTasksFromDB :: Connection -> IO (FileState Task)
loadTasksFromDB conn = do
  rows <-
    query_
      conn
      "SELECT file_path, task_index, level, todo_keyword, priority, \
      \title, tags, scheduled, deadline, created, closed, properties, description \
      \FROM tasks ORDER BY file_path, task_index" ::
      IO [DBRow]
  let grouped = groupBy (\a b -> filePath a == filePath b) rows
      pairs = map toFileEntry grouped
  pure $ M.fromList pairs
  where
    filePath :: DBRow -> T.Text
    filePath ((fp, _) :. _) = fp

    toFileEntry :: [DBRow] -> (FilePath, ParserResult (TaskFile Task))
    toFileEntry [] = error "groupBy produced empty group"
    toFileEntry rows@(((fp, _) :. _) : _) =
      let tasks = V.fromList (map (\((_ :: T.Text, _ :: Int) :. dt) -> unDBTask dt) rows)
       in (T.unpack fp, ParserSuccess (TaskFile Nothing tasks))

exportToOrgFiles :: Connection -> IO ()
exportToOrgFiles conn = do
  fs <- loadTasksFromDB conn
  mapM_ writeFile' (M.toList fs)
  where
    writeFile' :: (Writer a) => (FilePath, ParserResult a) -> IO ()
    writeFile' (path, ParserSuccess a) = TIO.writeFile path (write a)
    writeFile' (_, ParserFailure _) = pure ()
