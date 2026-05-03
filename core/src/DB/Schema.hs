{-# LANGUAGE OverloadedStrings #-}

module DB.Schema
  ( initialSchema,
    syncSchema,
    eventsSchema,
    schemaVersion,
  )
where

import Database.SQLite.Simple (Query)

schemaVersion :: Int
schemaVersion = 3

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

-- | v2 sync schema additions:
-- - synced_at on tasks: timestamp of last successful sync to a remote (NULL = never synced)
-- - sync_state table: generic key/value bookkeeping for the sync client (e.g. last_pull_ts)
--
-- These are client-side bookkeeping. They exist on the server's DB too but are
-- harmlessly unused there.
syncSchema :: [Query]
syncSchema =
  [ "ALTER TABLE tasks ADD COLUMN synced_at TEXT",
    "CREATE TABLE IF NOT EXISTS sync_state (\
    \  key TEXT PRIMARY KEY,\
    \  value TEXT NOT NULL\
    \)"
  ]

-- | v3 event sourcing additions.
--
-- The events table is the new source of truth. Each row is a partial Task
-- update at (file_path, task_index, occurred_at). NULL in any field column
-- means "this event did not touch this field"; a non-NULL value sets it
-- (sentinel values denote "cleared", see Core.Nullable).
--
-- The legacy `tasks` table is intentionally left in place. It is not used
-- by runtime read/write paths going forward, but staying behind lets us
-- ship the schema migration without coordinating a data migration in the
-- same release. A separate command, `dwayne migrateToEvents`, populates
-- the events table from the existing tasks data.
eventsSchema :: [Query]
eventsSchema =
  [ "CREATE TABLE IF NOT EXISTS events (\
    \  file_path     TEXT     NOT NULL,\
    \  task_index    INTEGER  NOT NULL,\
    \  occurred_at   TEXT     NOT NULL,\
    \  level         INTEGER,\
    \  todo_keyword  TEXT,\
    \  priority      INTEGER,\
    \  title         TEXT,\
    \  tags          TEXT,\
    \  scheduled     TEXT,\
    \  deadline      TEXT,\
    \  created       TEXT,\
    \  closed        TEXT,\
    \  properties    TEXT,\
    \  description   TEXT,\
    \  PRIMARY KEY (file_path, task_index, occurred_at)\
    \)",
    "CREATE INDEX IF NOT EXISTS idx_events_occurred_at ON events(occurred_at)",
    "CREATE INDEX IF NOT EXISTS idx_events_file ON events(file_path)"
  ]
