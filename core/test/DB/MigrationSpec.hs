{-# LANGUAGE OverloadedStrings #-}

-- | Migration tests. After Phase 3 cleanup the legacy @tasks@ /
-- @task_history@ tables are dropped by migration 005, so the post-init
-- table inventory only contains the events log, the read model, the
-- migration registry, and the sync bookkeeping table.
module DB.MigrationSpec (spec) where

import DB.Connection (initDatabase, withDatabase)
import DB.Migration (allMigrations, runMigrations)
import qualified Data.Text as T
import Database.SQLite.Simple
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec

spec :: Spec
spec = do
  describe "initDatabase" $ do
    it "creates a DB file with the post-cleanup table set" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      initDatabase dbPath
      conn <- open dbPath
      tables <- getTableNames conn
      close conn
      removeFile dbPath
      tables `shouldContain` ["migrations"]
      tables `shouldContain` ["events"]
      tables `shouldContain` ["task_current_state"]
      tables `shouldContain` ["sync_state"]
      tables `shouldNotContain` ["tasks"]
      tables `shouldNotContain` ["task_history"]

    it "enables WAL mode" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      withDatabase dbPath $ \conn -> do
        [Only mode] <- query_ conn "PRAGMA journal_mode" :: IO [Only T.Text]
        mode `shouldBe` "wal"
      removeFile dbPath

    it "enables foreign keys" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      withDatabase dbPath $ \conn -> do
        [Only fk] <- query_ conn "PRAGMA foreign_keys" :: IO [Only Int]
        fk `shouldBe` 1
      removeFile dbPath

  describe "runMigrations" $ do
    it "is idempotent — running twice does not error" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        runMigrations conn allMigrations
        runMigrations conn allMigrations
      removeFile dbPath

    it "records every applied migration" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        rows <- query_ conn "SELECT name FROM migrations" :: IO [Only T.Text]
        let names = map fromOnly rows
        names `shouldContain` ["001_initial_schema"]
        names `shouldContain` ["002_sync_schema"]
        names `shouldContain` ["003_events_schema"]
        names `shouldContain` ["004_cqrs_read_model"]
        names `shouldContain` ["005_drop_legacy_tasks"]
      removeFile dbPath

    it "does not re-apply already applied migrations" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        runMigrations conn allMigrations
        rows <- query_ conn "SELECT count(*) FROM migrations WHERE name = '001_initial_schema'" :: IO [Only Int]
        let [Only count] = rows
        count `shouldBe` 1
      removeFile dbPath

  describe "v5 drop-legacy-tables migration" $ do
    it "drops tasks and task_history if they were created by an earlier migration" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      -- Bootstrap with migrations 1..4: this leaves the legacy tables in place.
      withDatabase dbPath $ \conn -> do
        runMigrations conn (take 4 allMigrations)
        tables <- getTableNames conn
        tables `shouldContain` ["tasks"]
        tables `shouldContain` ["task_history"]
      -- Now apply the rest, which should drop them.
      withDatabase dbPath $ \conn -> do
        runMigrations conn allMigrations
        tables <- getTableNames conn
        tables `shouldNotContain` ["tasks"]
        tables `shouldNotContain` ["task_history"]
      removeFile dbPath

    it "is a no-op on installs that never had the legacy tables" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      initDatabase dbPath
      -- Re-running keeps the post-cleanup table set stable.
      withDatabase dbPath $ \conn ->
        runMigrations conn allMigrations
      withDatabase dbPath $ \conn -> do
        tables <- getTableNames conn
        tables `shouldNotContain` ["tasks"]
        tables `shouldNotContain` ["task_history"]
      removeFile dbPath

  describe "v4 cqrs-read-model migration backfill" $ do
    -- An upgrade from v3 carries an already-populated events table. The
    -- @events_to_state@ trigger only fires on future inserts, so without
    -- a backfill task_current_state would stay empty until the user
    -- manually ran `dwayne dbRebuildState`. Migration 004 runs the same
    -- bulk-projection SQL as part of the migration so the upgrade is
    -- one-shot.
    it "backfills task_current_state for events that existed before migration 004" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      -- Set up a v3-shaped DB: only migrations 1..3.
      withDatabase dbPath $ \conn ->
        runMigrations conn (take 3 allMigrations)
      -- Insert events directly into the events table — this simulates a
      -- user who has been running v3 and now upgrades.
      withDatabase dbPath $ \conn -> do
        execute_
          conn
          "INSERT INTO events \
          \ (file_path, task_index, occurred_at, level, todo_keyword, title) \
          \ VALUES \
          \ ('/inbox.org', 0, '2026-04-29T00:00:00', 1, 'INBOX', '\"buy milk\"'),\
          \ ('/inbox.org', 1, '2026-04-29T00:00:00', 1, 'TODO', '\"write report\"'),\
          \ ('/projects.org', 0, '2026-04-29T00:00:00', 1, 'PROJECT', '\"refactor\"')"
        -- A delta on top of task 0: latest todo_keyword should win.
        execute_
          conn
          "INSERT INTO events \
          \ (file_path, task_index, occurred_at, todo_keyword) \
          \ VALUES ('/inbox.org', 0, '2026-04-29T01:00:00', 'DONE')"
        -- A partial event with no genesis coverage: must not produce a row.
        execute_
          conn
          "INSERT INTO events \
          \ (file_path, task_index, occurred_at, todo_keyword) \
          \ VALUES ('/inbox.org', 99, '2026-04-29T00:00:00', 'TODO')"
      -- Apply remaining migrations (004 + 005). The backfill runs as
      -- part of 004.
      withDatabase dbPath $ \conn ->
        runMigrations conn allMigrations
      -- Assert the read model now reflects the pre-existing events.
      withDatabase dbPath $ \conn -> do
        [Only n] <- query_ conn "SELECT COUNT(*) FROM task_current_state" :: IO [Only Int]
        n `shouldBe` 3
        -- Genesis-only task projects to its initial keyword.
        [Only kw1] <-
          query_
            conn
            "SELECT todo_keyword FROM task_current_state WHERE file_path = '/inbox.org' AND task_index = 1" ::
            IO [Only T.Text]
        kw1 `shouldBe` "TODO"
        -- Task with delta picks up the latest non-NULL value.
        [Only kw0] <-
          query_
            conn
            "SELECT todo_keyword FROM task_current_state WHERE file_path = '/inbox.org' AND task_index = 0" ::
            IO [Only T.Text]
        kw0 `shouldBe` "DONE"
        -- Genesis-less partial event must not appear.
        rows <-
          query_
            conn
            "SELECT 1 FROM task_current_state WHERE file_path = '/inbox.org' AND task_index = 99" ::
            IO [Only Int]
        rows `shouldBe` []
      removeFile dbPath

getTableNames :: Connection -> IO [T.Text]
getTableNames conn = do
  rows <- query_ conn "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" :: IO [Only T.Text]
  pure (map fromOnly rows)
