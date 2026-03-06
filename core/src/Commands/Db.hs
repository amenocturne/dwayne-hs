{-# LANGUAGE OverloadedStrings #-}

module Commands.Db
  ( dbInitCommand,
    dbImportCommand,
    dbExportCommand,
    dbStatsCommand,
    dbCheckCommand,
  )
where

import Commands.CliHelpers (loadConfig, loadFileState)
import Commands.Command (Command (..))
import DB.Connection (initDatabase, withDatabase)
import DB.Export (exportToOrgFiles, loadTasksFromDB)
import DB.Import (importFileState)
import qualified Data.Map.Strict as M
import Database.SQLite.Simple (Only (..), query_)
import Model.OrgMode (Task, TaskFile (..))
import Options.Applicative (help, long, switch)
import Parser.OrgParser (orgFileParser)
import Parser.Parser (ParserResult (..), runParser)
import System.Directory (doesFileExist)
import TextUtils (readFileExample)
import Tui.Types (AppConfig (..), getAllFiles)

dbInitCommand :: Command Task
dbInitCommand =
  Command
    { cmdName = "Database Init",
      cmdAlias = "dbInit",
      cmdDescription = "Initialize the database (create file, run migrations)",
      cmdTui = Nothing,
      cmdCli =
        Just $
          fmap
            ( \force -> do
                conf <- loadConfig
                let dbPath = _database conf
                exists <- doesFileExist dbPath
                if exists && not force
                  then putStrLn $ "Database already exists at: " ++ dbPath ++ " (use --force to reinitialize)"
                  else do
                    initDatabase dbPath
                    putStrLn $ "Database initialized at: " ++ dbPath
            )
            (switch (long "force" <> help "Initialize even if database already exists")),
      cmdApi = Nothing
    }

dbImportCommand :: Command Task
dbImportCommand =
  Command
    { cmdName = "Database Import",
      cmdAlias = "dbImport",
      cmdDescription = "Import org files into the database",
      cmdTui = Nothing,
      cmdCli = Just $ pure $ do
        (conf, fState) <- loadFileState
        let dbFile = _database conf
        -- loadFileState already imports, but we re-import to get the count
        count <- withDatabase dbFile $ \conn ->
          importFileState conn fState
        putStrLn $ "Imported " ++ show count ++ " tasks from " ++ show (M.size fState) ++ " files",
      cmdApi = Nothing
    }

dbExportCommand :: Command Task
dbExportCommand =
  Command
    { cmdName = "Database Export",
      cmdAlias = "dbExport",
      cmdDescription = "Export database tasks to org files",
      cmdTui = Nothing,
      cmdCli = Just $ pure $ do
        conf <- loadConfig
        let dbFile = _database conf
        withDatabase dbFile $ \conn -> do
          fs <- loadTasksFromDB conn
          let fileCount = M.size fs
              taskCount = sum $ map countTasks (M.elems fs)
          exportToOrgFiles conn
          putStrLn $ "Exported " ++ show taskCount ++ " tasks to " ++ show fileCount ++ " files",
      cmdApi = Nothing
    }
  where
    countTasks (ParserSuccess tf) = length (_content tf)
    countTasks (ParserFailure _) = 0

dbStatsCommand :: Command Task
dbStatsCommand =
  Command
    { cmdName = "Database Stats",
      cmdAlias = "dbStats",
      cmdDescription = "Show database statistics",
      cmdTui = Nothing,
      cmdCli = Just $ pure $ do
        conf <- loadConfig
        let dbFile = _database conf
        withDatabase dbFile $ \conn -> do
          [Only taskCount] <- query_ conn "SELECT COUNT(*) FROM tasks" :: IO [Only Int]
          [Only fileCount] <- query_ conn "SELECT COUNT(DISTINCT file_path) FROM tasks" :: IO [Only Int]
          putStrLn $ "Database: " ++ dbFile
          putStrLn $ "Tasks:    " ++ show taskCount
          putStrLn $ "Files:    " ++ show fileCount,
      cmdApi = Nothing
    }

dbCheckCommand :: Command Task
dbCheckCommand =
  Command
    { cmdName = "Database Check",
      cmdAlias = "dbCheck",
      cmdDescription = "Verify database integrity and compare with org files",
      cmdTui = Nothing,
      cmdCli = Just $ pure $ do
        conf <- loadConfig
        let dbFile = _database conf
            allFiles = getAllFiles conf
        withDatabase dbFile $ \conn -> do
          results <- query_ conn "PRAGMA integrity_check" :: IO [Only String]
          case results of
            [Only "ok"] -> putStrLn "Integrity check: OK"
            _ -> do
              putStrLn "Integrity check: FAILED"
              mapM_ (\(Only msg) -> putStrLn $ "  " ++ msg) results
          [Only dbCount] <- query_ conn "SELECT COUNT(*) FROM tasks" :: IO [Only Int]
          parsedFiles <- mapM readOrgFile allFiles
          let orgCount = sum $ map (countTasks . snd) parsedFiles
          putStrLn $ "DB tasks:   " ++ show dbCount
          putStrLn $ "Org tasks:  " ++ show orgCount
          if dbCount == orgCount
            then putStrLn "Counts match: OK"
            else putStrLn $ "MISMATCH: DB has " ++ show dbCount ++ ", org files have " ++ show orgCount,
      cmdApi = Nothing
    }
  where
    readOrgFile f = do
      content <- readFileExample f
      let (_, _, taskFile) = runParser orgFileParser content
      return (f, taskFile)
    countTasks (ParserSuccess tf) = length (_content tf)
    countTasks (ParserFailure _) = 0
