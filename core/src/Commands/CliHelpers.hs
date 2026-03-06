{-# LANGUAGE OverloadedStrings #-}

module Commands.CliHelpers
  ( loadConfig,
    loadFileState,
    formatTaskLine,
  )
where

import Control.Monad (void)
import Core.Types (FileState, TaskPointer (..))
import DB.Connection (initDatabase, withDatabase)
import DB.Import (importFileState)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Yaml.Aeson (ParseException, decodeFileEither)
import Model.OrgMode (Task (..), TaskFile, richTextToPlain)
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

-- | Load FileState from DB (initializing and importing org files first)
loadFileState :: IO (AppConfig Task, FileState Task)
loadFileState = do
  conf <- loadConfig
  let dbFile = _database conf
      allFiles = getAllFiles conf
  initDatabase dbFile
  parsedFiles <- mapM readOrgFile allFiles
  let fState = M.fromList parsedFiles
  void $ withDatabase dbFile $ \conn ->
    importFileState conn fState
  return (conf, fState)
  where
    readOrgFile :: FilePath -> IO (FilePath, ParserResult (TaskFile Task))
    readOrgFile f = do
      txt <- readFileExample f
      let (_, _, result) = runParser orgFileParser txt
      return (f, result)

-- | Format a task as a single line for CLI output
-- Format: [KEYWORD] title  :tags:  (file:index)
formatTaskLine :: Task -> TaskPointer -> String
formatTaskLine task ptr =
  let kw =
        let k = _todoKeyword task
         in if T.null k then "" else "[" <> T.unpack k <> "] "
      ttl = T.unpack (richTextToPlain (_title task))
      tags' =
        if S.null (_tags task)
          then ""
          else "  :" <> T.unpack (T.intercalate ":" (S.toAscList (_tags task))) <> ":"
      loc = "  (" <> _file ptr <> ":" <> show (_taskIndex ptr) <> ")"
   in kw <> ttl <> tags' <> loc
