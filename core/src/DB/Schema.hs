{-# LANGUAGE OverloadedStrings #-}

module DB.Schema
  ( initialSchema,
    schemaVersion,
  )
where

import Database.SQLite.Simple (Query)

schemaVersion :: Int
schemaVersion = 1

initialSchema :: [Query]
initialSchema =
  [ "CREATE TABLE IF NOT EXISTS migrations (\
    \  id INTEGER PRIMARY KEY,\
    \  name TEXT NOT NULL UNIQUE,\
    \  applied_at TEXT NOT NULL DEFAULT (datetime('now'))\
    \)",
    "CREATE TABLE IF NOT EXISTS tasks (\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \  file_path TEXT NOT NULL,\
    \  task_index INTEGER NOT NULL,\
    \  level INTEGER NOT NULL DEFAULT 1,\
    \  todo_keyword TEXT NOT NULL DEFAULT '',\
    \  priority INTEGER,\
    \  title TEXT NOT NULL DEFAULT '',\
    \  tags TEXT NOT NULL DEFAULT '[]',\
    \  scheduled TEXT,\
    \  deadline TEXT,\
    \  created TEXT,\
    \  closed TEXT,\
    \  properties TEXT NOT NULL DEFAULT '[]',\
    \  description TEXT NOT NULL DEFAULT '',\
    \  updated_at TEXT NOT NULL DEFAULT (datetime('now')),\
    \  UNIQUE(file_path, task_index)\
    \)",
    "CREATE TABLE IF NOT EXISTS task_history (\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \  task_id INTEGER NOT NULL REFERENCES tasks(id),\
    \  old_keyword TEXT NOT NULL,\
    \  new_keyword TEXT NOT NULL,\
    \  changed_at TEXT NOT NULL DEFAULT (datetime('now'))\
    \)"
  ]
