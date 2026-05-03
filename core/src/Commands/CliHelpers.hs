{-# LANGUAGE OverloadedStrings #-}

module Commands.CliHelpers
  ( loadConfig,
    loadFileState,
    loadFileStateFromOrg,
    loadFileStateFromEvents,
    formatTaskLine,
  )
where

import Core.Types (FileState, TaskPointer (..))
import DB.Connection (initDatabase, withDatabase)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml.Aeson (ParseException, decodeFileEither)
import Events.Projection (fileStateFromEvents)
import Events.Store (selectAllEvents)
import Model.OrgFormat (formatHeaderLine)
import Model.OrgMode (Task (..), TaskFile)
import Parser.OrgParser (orgFileParser)
import Parser.Parser (ParserResult, runParser)
import System.Exit (die)
import qualified System.IO as IO
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

-- | Load FileState by replaying events from the @events@ table. The events
-- log is the canonical source of truth at runtime; the legacy @tasks@ table
-- is preserved only as a fallback / migration scaffold and is not consulted
-- here.
--
-- If the events table is empty we emit a hint to stderr and return an
-- empty FileState — the caller will see "no tasks" and can run
-- @dwayne migrateToEvents@ to seed the log.
loadFileState :: IO (AppConfig Task, FileState Task)
loadFileState = do
  conf <- loadConfig
  let dbFile = _database conf
  initDatabase dbFile
  fState <- loadFileStateFromEvents dbFile
  return (conf, fState)

-- | Replay every event in the configured DB into a 'FileState Task'.
-- Returns 'M.empty' when the events table is empty (with a stderr hint).
loadFileStateFromEvents :: FilePath -> IO (FileState Task)
loadFileStateFromEvents dbFile = do
  events <- withDatabase dbFile selectAllEvents
  case events of
    [] -> do
      IO.hPutStrLn IO.stderr $
        "warning: events table is empty at "
          ++ dbFile
          ++ ". Run 'dwayne migrateToEvents' to seed it from your org files."
      pure M.empty
    _ -> pure (fileStateFromEvents events)

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
