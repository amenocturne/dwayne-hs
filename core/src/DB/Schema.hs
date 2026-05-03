{-# LANGUAGE OverloadedStrings #-}

module DB.Schema
  ( initialSchema,
    syncSchema,
    eventsSchema,
    cqrsReadModelSchema,
    schemaVersion,
  )
where

import Database.SQLite.Simple (Query)

schemaVersion :: Int
schemaVersion = 4

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

-- | v4 CQRS read model.
--
-- The events table remains the canonical write log. This migration adds a
-- materialized read model — task_current_state — kept in sync by an AFTER
-- INSERT trigger on events. All read paths (TUI list views, API queries,
-- CLI views) will eventually go through this table via 'Repo.TaskRepo';
-- writes still go to events and the trigger projects them.
--
-- The trigger recomputes the affected (file_path, task_index) row by
-- selecting the latest non-NULL value per field across all events for that
-- task, ordered by occurred_at. This is correct even when events arrive
-- out of order (e.g. a sync push delivering an older event after a newer
-- one). The genesis-coverage WHERE EXISTS guard mirrors
-- Events.Projection.foldTaskEvents — a task is materialized only once at
-- least one event covers level + todo_keyword + title.
cqrsReadModelSchema :: [Query]
cqrsReadModelSchema =
  [ "CREATE TABLE IF NOT EXISTS task_current_state (\
    \  file_path     TEXT    NOT NULL,\
    \  task_index    INTEGER NOT NULL,\
    \  level         INTEGER NOT NULL,\
    \  todo_keyword  TEXT,\
    \  priority      INTEGER,\
    \  title         TEXT    NOT NULL,\
    \  tags          TEXT,\
    \  scheduled     TEXT,\
    \  deadline      TEXT,\
    \  created       TEXT,\
    \  closed        TEXT,\
    \  properties    TEXT,\
    \  description   TEXT,\
    \  last_event_at TEXT    NOT NULL,\
    \  PRIMARY KEY (file_path, task_index)\
    \)",
    "CREATE INDEX IF NOT EXISTS idx_tcs_keyword ON task_current_state(todo_keyword)",
    "CREATE INDEX IF NOT EXISTS idx_tcs_keyword_prio ON task_current_state(todo_keyword, priority)",
    "CREATE INDEX IF NOT EXISTS idx_tcs_deadline ON task_current_state(deadline) WHERE deadline IS NOT NULL",
    "CREATE INDEX IF NOT EXISTS idx_tcs_scheduled ON task_current_state(scheduled) WHERE scheduled IS NOT NULL",
    "CREATE INDEX IF NOT EXISTS idx_tcs_file ON task_current_state(file_path)",
    "CREATE TRIGGER IF NOT EXISTS events_to_state\
    \ AFTER INSERT ON events\
    \ BEGIN\
    \   INSERT INTO task_current_state (\
    \     file_path, task_index, level, todo_keyword, priority, title, tags,\
    \     scheduled, deadline, created, closed, properties, description, last_event_at\
    \   )\
    \   SELECT\
    \     NEW.file_path,\
    \     NEW.task_index,\
    \     (SELECT level FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND level IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT todo_keyword FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND todo_keyword IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT priority FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND priority IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT title FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND title IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT tags FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND tags IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT scheduled FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND scheduled IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT deadline FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND deadline IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT created FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND created IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT closed FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND closed IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT properties FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND properties IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT description FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index AND description IS NOT NULL ORDER BY occurred_at DESC LIMIT 1),\
    \     (SELECT MAX(occurred_at) FROM events WHERE file_path = NEW.file_path AND task_index = NEW.task_index)\
    \   WHERE EXISTS (\
    \     SELECT 1 FROM events\
    \     WHERE file_path = NEW.file_path AND task_index = NEW.task_index\
    \       AND level IS NOT NULL AND todo_keyword IS NOT NULL AND title IS NOT NULL\
    \   )\
    \   ON CONFLICT (file_path, task_index) DO UPDATE SET\
    \     level         = excluded.level,\
    \     todo_keyword  = excluded.todo_keyword,\
    \     priority      = excluded.priority,\
    \     title         = excluded.title,\
    \     tags          = excluded.tags,\
    \     scheduled     = excluded.scheduled,\
    \     deadline      = excluded.deadline,\
    \     created       = excluded.created,\
    \     closed        = excluded.closed,\
    \     properties    = excluded.properties,\
    \     description   = excluded.description,\
    \     last_event_at = excluded.last_event_at;\
    \ END"
  ]
