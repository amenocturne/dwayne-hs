{-# LANGUAGE OverloadedStrings #-}

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
    it "creates a DB file with expected tables" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      initDatabase dbPath
      conn <- open dbPath
      tables <- getTableNames conn
      close conn
      removeFile dbPath
      tables `shouldContain` ["migrations"]
      tables `shouldContain` ["tasks"]
      tables `shouldContain` ["task_history"]

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

    it "records applied migrations" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        rows <- query_ conn "SELECT name FROM migrations" :: IO [Only T.Text]
        let names = map fromOnly rows
        names `shouldContain` ["001_initial_schema"]
        names `shouldContain` ["002_sync_schema"]
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

  describe "v2 sync schema" $ do
    it "adds synced_at column to tasks" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        cols <- getTableColumns conn "tasks"
        cols `shouldContain` ["synced_at"]
      removeFile dbPath

    it "creates sync_state table" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        tables <- getTableNames conn
        tables `shouldContain` ["sync_state"]
      removeFile dbPath

    it "migrates a v1 DB cleanly: existing rows preserved, synced_at NULL" $ do
      dbPath <- emptySystemTempFile "dwayne-test.db"
      -- Bootstrap a v1-style DB: only run the first migration.
      withDatabase dbPath $ \conn ->
        runMigrations conn (take 1 allMigrations)
      withDatabase dbPath $ \conn -> do
        execute_ conn "INSERT INTO tasks (file_path, task_index, title) VALUES ('/v1.org', 0, 'old task')"
        cols <- getTableColumns conn "tasks"
        cols `shouldNotContain` ["synced_at"]
      -- Now apply v2.
      withDatabase dbPath $ \conn ->
        runMigrations conn allMigrations
      withDatabase dbPath $ \conn -> do
        cols <- getTableColumns conn "tasks"
        cols `shouldContain` ["synced_at"]
        rows <- query_ conn "SELECT title, synced_at FROM tasks WHERE file_path = '/v1.org'" :: IO [(T.Text, Maybe T.Text)]
        rows `shouldBe` [("old task", Nothing)]
      removeFile dbPath

getTableNames :: Connection -> IO [T.Text]
getTableNames conn = do
  rows <- query_ conn "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" :: IO [Only T.Text]
  pure (map fromOnly rows)

getTableColumns :: Connection -> T.Text -> IO [T.Text]
getTableColumns conn tbl = do
  -- pragma_table_info returns: cid, name, type, notnull, dflt_value, pk
  rows <- query_ conn (Query ("SELECT name FROM pragma_table_info('" <> tbl <> "')")) :: IO [Only T.Text]
  pure (map fromOnly rows)
