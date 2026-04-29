{-# LANGUAGE OverloadedStrings #-}

module Commands.CliHelpers
  ( loadConfig,
    loadFileState,
    loadFileStateFromOrg,
    formatTaskLine,
  )
where

import Control.Monad (when)
import Core.Types (FileState, TaskPointer (..))
import DB.Connection (initDatabase)
import DB.TaskStore (DatabaseStore (..), TaskStore (loadTasks))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml.Aeson (ParseException, decodeFileEither)
import Model.OrgFormat (formatHeaderLine)
import Model.OrgMode (Task (..), TaskFile)
import Parser.OrgParser (orgFileParser)
import Parser.Parser (ParserResult, runParser)
import System.Exit (die)
import TextUtils (getConfigPath, readFileExample)
import Tui.Types (AppConfig (..), expandConfigPaths, getAllFiles)

-- | Load and expand the application config
loadConfig :: IO (AppConfig Task)
loadConfig = do
  configFilePath <- getConfigPath
  parsedConfig <- decodeFileEither configFilePath :: IO (Either ParseException (AppConfig Task))
  case parsedConfig of
    Left err -> die $ "Failed to load config: " ++ show err
    Right conf -> expandConfigPaths conf

-- | Load FileState from the SQLite-backed task store. The database is
-- the canonical source of truth at runtime; org files are export-only.
loadFileState :: IO (AppConfig Task, FileState Task)
loadFileState = do
  conf <- loadConfig
  let dbFile = _database conf
  initDatabase dbFile
  fState <- loadTasks (DatabaseStore dbFile)
  when (M.null fState) $
    putStrLn "Database is empty. Run 'dwayne dbImport' to import org files."
  return (conf, fState)

-- | Parse the configured org files into a FileState without touching the
-- database. Used by `dwayne dbImport` to seed the DB; not called from any
-- runtime path.
loadFileStateFromOrg :: IO (AppConfig Task, FileState Task)
loadFileStateFromOrg = do
  conf <- loadConfig
  let allFiles = getAllFiles conf
  parsedFiles <- mapM readOrgFile allFiles
  let fState = M.fromList parsedFiles
  return (conf, fState)
  where
    readOrgFile :: FilePath -> IO (FilePath, ParserResult (TaskFile Task))
    readOrgFile f = do
      txt <- readFileExample f
      let (_, _, result) = runParser orgFileParser txt
      return (f, result)

-- | Format a task for CLI output using the org-mode header format
formatTaskLine :: Task -> TaskPointer -> String
formatTaskLine task ptr =
  T.unpack (formatHeaderLine (_level task) (_todoKeyword task) (_priority task) (_title task) (_tags task))
    <> "  ("
    <> _file ptr
    <> ":"
    <> show (_taskIndex ptr)
    <> ")"
