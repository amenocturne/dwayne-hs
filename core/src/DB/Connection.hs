{-# LANGUAGE OverloadedStrings #-}

module DB.Connection
  ( withDatabase,
    initDatabase,
  )
where

import Control.Exception (bracket)
import DB.Migration (allMigrations, runMigrations)
import Database.SQLite.Simple

withDatabase :: FilePath -> (Connection -> IO a) -> IO a
withDatabase dbPath action =
  bracket (open dbPath) close $ \conn -> do
    setupConnection conn
    action conn

initDatabase :: FilePath -> IO ()
initDatabase dbPath =
  withDatabase dbPath $ \conn ->
    runMigrations conn allMigrations

setupConnection :: Connection -> IO ()
setupConnection conn = do
  execute_ conn "PRAGMA journal_mode=WAL"
  execute_ conn "PRAGMA foreign_keys=ON"
