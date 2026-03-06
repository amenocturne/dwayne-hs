{-# LANGUAGE OverloadedStrings #-}

module DB.Connection
  ( withDatabase,
    initDatabase,
    isDatabaseEmpty,
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

isDatabaseEmpty :: FilePath -> IO Bool
isDatabaseEmpty path =
  withDatabase path $ \conn -> do
    [Only count] <- query_ conn "SELECT COUNT(*) FROM tasks"
    return (count == (0 :: Int))

setupConnection :: Connection -> IO ()
setupConnection conn = do
  execute_ conn "PRAGMA journal_mode=WAL"
  execute_ conn "PRAGMA foreign_keys=ON"
