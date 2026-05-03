{-# LANGUAGE OverloadedStrings #-}

module DB.Migration
  ( Migration (..),
    runMigrations,
    allMigrations,
  )
where

import Control.Monad (forM_)
import DB.Schema (cqrsReadModelSchema, dropLegacyTablesSchema, eventsSchema, initialSchema, syncSchema)
import qualified Data.Text as T
import Database.SQLite.Simple

data Migration = Migration
  { migrationName :: T.Text,
    migrationStatements :: [Query]
  }

allMigrations :: [Migration]
allMigrations =
  [ Migration
      "001_initial_schema"
      initialSchema,
    Migration
      "002_sync_schema"
      syncSchema,
    Migration
      "003_events_schema"
      eventsSchema,
    Migration
      "004_cqrs_read_model"
      cqrsReadModelSchema,
    Migration
      "005_drop_legacy_tasks"
      dropLegacyTablesSchema
  ]

runMigrations :: Connection -> [Migration] -> IO ()
runMigrations conn migrations = do
  ensureMigrationsTable conn
  applied <- getAppliedMigrations conn
  let pending = filter (\m -> migrationName m `notElem` applied) migrations
  forM_ pending $ \m -> do
    withTransaction conn $ do
      mapM_ (execute_ conn) (migrationStatements m)
      execute conn "INSERT INTO migrations (name) VALUES (?)" (Only (migrationName m))

ensureMigrationsTable :: Connection -> IO ()
ensureMigrationsTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS migrations (\
    \  id INTEGER PRIMARY KEY,\
    \  name TEXT NOT NULL UNIQUE,\
    \  applied_at TEXT NOT NULL DEFAULT (datetime('now'))\
    \)"

getAppliedMigrations :: Connection -> IO [T.Text]
getAppliedMigrations conn = do
  rows <- query_ conn "SELECT name FROM migrations" :: IO [Only T.Text]
  pure (map fromOnly rows)
