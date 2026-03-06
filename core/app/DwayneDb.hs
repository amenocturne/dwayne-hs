{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import DB.Connection (initDatabase, withDatabase)
import DB.Export (exportToOrgFiles, loadTasksFromDB)
import DB.Import (importFileState)
import qualified Data.Map.Strict as M
import Data.Yaml.Aeson (ParseException, decodeFileEither)
import Database.SQLite.Simple (Only (..), query_)
import Model.OrgMode (Task, TaskFile (..))
import Parser.OrgParser (orgFileParser)
import Parser.Parser (ParserResult (..), runParser)
import System.Environment (getArgs)
import System.Exit (die)
import TextUtils (getConfigPath, readFileExample)
import Tui.Types (AppConfig (..), expandConfigPaths, getAllFiles)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"]   -> cmdInit
    ["import"] -> cmdImport
    ["export"] -> cmdExport
    ["stats"]  -> cmdStats
    ["check"]  -> cmdCheck
    _          -> printUsage

loadConfig :: IO (AppConfig Task)
loadConfig = do
  configFilePath <- getConfigPath
  parsedConfig <- decodeFileEither configFilePath :: IO (Either ParseException (AppConfig Task))
  case parsedConfig of
    Left err -> die $ "Failed to load config: " ++ show err
    Right conf -> expandConfigPaths conf

cmdInit :: IO ()
cmdInit = do
  conf <- loadConfig
  let dbPath = _database conf
  initDatabase dbPath
  putStrLn $ "Database initialized at: " ++ dbPath

cmdImport :: IO ()
cmdImport = do
  conf <- loadConfig
  let dbPath = _database conf
      allFiles = getAllFiles conf
  initDatabase dbPath
  parsedFiles <- mapM readTaskFile allFiles
  let fState = M.fromList parsedFiles
  count <- withDatabase dbPath $ \conn ->
    importFileState conn fState
  putStrLn $ "Imported " ++ show count ++ " tasks from " ++ show (length allFiles) ++ " files"

cmdExport :: IO ()
cmdExport = do
  conf <- loadConfig
  let dbPath = _database conf
  withDatabase dbPath $ \conn -> do
    fs <- loadTasksFromDB conn
    let fileCount = M.size fs
        taskCount = sum $ map countTasks (M.elems fs)
    exportToOrgFiles conn
    putStrLn $ "Exported " ++ show taskCount ++ " tasks to " ++ show fileCount ++ " files"
  where
    countTasks :: ParserResult (TaskFile Task) -> Int
    countTasks (ParserSuccess tf) = length (_content tf)
    countTasks (ParserFailure _) = 0

cmdStats :: IO ()
cmdStats = do
  conf <- loadConfig
  let dbPath = _database conf
  withDatabase dbPath $ \conn -> do
    [Only taskCount] <- query_ conn "SELECT COUNT(*) FROM tasks" :: IO [Only Int]
    [Only fileCount] <- query_ conn "SELECT COUNT(DISTINCT file_path) FROM tasks" :: IO [Only Int]
    putStrLn $ "Database: " ++ dbPath
    putStrLn $ "Tasks:    " ++ show taskCount
    putStrLn $ "Files:    " ++ show fileCount

cmdCheck :: IO ()
cmdCheck = do
  conf <- loadConfig
  let dbPath = _database conf
      allFiles = getAllFiles conf
  withDatabase dbPath $ \conn -> do
    -- Integrity check
    results <- query_ conn "PRAGMA integrity_check" :: IO [Only String]
    case results of
      [Only "ok"] -> putStrLn "Integrity check: OK"
      _ -> do
        putStrLn "Integrity check: FAILED"
        mapM_ (\(Only msg) -> putStrLn $ "  " ++ msg) results

    -- Compare counts
    [Only dbCount] <- query_ conn "SELECT COUNT(*) FROM tasks" :: IO [Only Int]
    parsedFiles <- mapM readTaskFile allFiles
    let orgCount = sum $ map (countTasks . snd) parsedFiles
    putStrLn $ "DB tasks:   " ++ show dbCount
    putStrLn $ "Org tasks:  " ++ show orgCount
    if dbCount == orgCount
      then putStrLn "Counts match: OK"
      else putStrLn $ "MISMATCH: DB has " ++ show dbCount ++ ", org files have " ++ show orgCount
  where
    countTasks :: ParserResult (TaskFile Task) -> Int
    countTasks (ParserSuccess tf) = length (_content tf)
    countTasks (ParserFailure _) = 0

readTaskFile :: FilePath -> IO (FilePath, ParserResult (TaskFile Task))
readTaskFile f = do
  content <- readFileExample f
  let (_, _, taskFile) = runParser orgFileParser content
  return (f, taskFile)

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: dwayne-db <command>"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  init     Initialize the database"
  putStrLn "  import   Import org files into the database"
  putStrLn "  export   Export database tasks to org files"
  putStrLn "  stats    Show database statistics"
  putStrLn "  check    Verify database integrity"
