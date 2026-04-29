{-# LANGUAGE OverloadedStrings #-}

module DB.Query
  ( taskFields,
    insertTaskQuery,
    selectTaskQuery,
    deleteAllTasksQuery,
    deleteTasksByFileQuery,
    intercalateQuery,
  )
where

import Data.List (intersperse)
import Database.SQLite.Simple (Query)

taskFields :: [Query]
taskFields =
  [ "file_path",
    "task_index",
    "level",
    "todo_keyword",
    "priority",
    "title",
    "tags",
    "scheduled",
    "deadline",
    "created",
    "closed",
    "properties",
    "description"
  ]

intercalateQuery :: Query -> [Query] -> Query
intercalateQuery sep = mconcat . intersperse sep

insertTaskQuery :: Query
insertTaskQuery =
  "INSERT INTO tasks ("
    <> intercalateQuery ", " taskFields
    <> ") VALUES ("
    <> intercalateQuery ", " (map (const "?") taskFields)
    <> ")"

selectTaskQuery :: Query
selectTaskQuery =
  "SELECT "
    <> intercalateQuery ", " taskFields
    <> " FROM tasks ORDER BY file_path, task_index"

deleteAllTasksQuery :: Query
deleteAllTasksQuery = "DELETE FROM tasks"

deleteTasksByFileQuery :: Query
deleteTasksByFileQuery = "DELETE FROM tasks WHERE file_path = ?"
