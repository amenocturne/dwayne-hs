{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Api.Server (runServer)
import Commands.Command (Command (..))
import Commands.Registry (allCommands)
import Control.Monad (join, void)
import DB.Connection (initDatabase, withDatabase)
import DB.Import (importFileState)
import DB.TaskStore (DatabaseStore (..), mkTaskStoreOps)
import Data.Char (isUpper, toLower)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Yaml.Aeson (ParseException, decodeFileEither)
import Model.OrgMode (Task, TaskFile)
import Options.Applicative (CommandFields, Mod, command, execParser, fullDesc, header, helper, info, progDesc, subparser, (<**>))
import Parser.OrgParser (anyTaskparser, orgFileParser)
import Parser.Parser (Location, Parser, ParserResult, errorToMaybe, runParser)
import Refile.OrgRefileable ()
import Render.OrgRender ()
import Searcher.OrgSearcher ()
import System.Environment (getArgs)
import System.Exit (die)
import TextUtils (getConfigPath, readFileExample)
import Tui.Keybindings (orgKeyBindings, sortByCreatedDesc, todoKeywordFilter)
import Tui.Tui
import Tui.Types
import Writer.OrgWriter ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> startTui
    ["--serve"] -> startWebServer
    _ -> runCli

-- | Run CLI subcommands dispatched from the command registry
runCli :: IO ()
runCli = join $ execParser opts
  where
    cliCommands = mapMaybe mkSubcommand allCommands
    parser = subparser (mconcat cliCommands) <**> helper
    opts = info parser (fullDesc <> header "dwayne - GTD task manager")

-- | Build an optparse-applicative subcommand from a Command with a CLI binding
mkSubcommand :: Command Task -> Maybe (Mod CommandFields (IO ()))
mkSubcommand cmd = do
  cliParser <- cmdCli cmd
  let name' = toKebabCase (cmdAlias cmd)
      desc = T.unpack (cmdDescription cmd)
  return $ command name' (info cliParser (progDesc desc))

-- | Convert camelCase to kebab-case: "dbInit" -> "db-init", "viewInbox" -> "view-inbox"
toKebabCase :: T.Text -> String
toKebabCase = go . T.unpack
  where
    go [] = []
    go (c : cs)
      | isUpper c = '-' : toLower c : go cs
      | otherwise = c : go cs

startWebServer :: IO ()
startWebServer = do
  putStrLn "Starting server on http://localhost:8080"
  ctx <- initializeAppContext
  runServer 8080 ctx

initializeAppContext :: IO (AppContext Task)
initializeAppContext = do
  configFilePath <- getConfigPath
  parsedConfig <- decodeFileEither configFilePath :: IO (Either ParseException (AppConfig Task))
  conf <- case parsedConfig of
    Left err ->
      die $
        unlines
          [ "ERROR: Failed to load configuration file",
            "Location: " ++ configFilePath,
            "",
            "Reason: " ++ show err,
            "",
            "Please check that:",
            "  - The configuration file exists",
            "  - The YAML syntax is valid",
            "  - All required fields are present"
          ]
    Right c -> expandConfigPaths c

  let allFiles = getAllFiles conf
      dbFile = _database conf
  initDatabase dbFile

  parsedFiles <- mapM (readTaskFile orgFileParser) allFiles
  let fState = M.fromList (fmap (\(fp, (_, result)) -> (fp, result)) parsedFiles)
      parsingErrors = extractParsingErrors parsedFiles
  case parsingErrors of
    [] -> return ()
    errs -> do
      putStrLn "WARNING: Parsing errors found:"
      mapM_ printParsingError errs

  void $ withDatabase dbFile $ \conn ->
    importFileState conn fState

  let ops = mkTaskStoreOps (DatabaseStore dbFile)
      sysConf = (mkSystemConfig (_commands conf)) {_taskStoreOps = Just ops}

  return $ initializeAppContextForServer sysConf conf fState
  where
    extractParsingErrors parsedFiles =
      mapMaybe (\(fp, (loc, result)) -> fmap (\e -> (fp, loc, e)) (errorToMaybe result)) parsedFiles

    printParsingError (fp, loc, err) =
      putStrLn $ "  " ++ fp ++ ": " ++ err ++ " at " ++ show loc

startTui :: IO ()
startTui = do
  configFilePath <- getConfigPath
  parsedConfig <- decodeFileEither configFilePath :: IO (Either ParseException (AppConfig Task))
  conf <- case parsedConfig of
    Left err ->
      die $
        unlines
          [ "ERROR: Failed to load configuration file",
            "Location: " ++ configFilePath,
            "",
            "Reason: " ++ show err
          ]
    Right c -> expandConfigPaths c
  let dbFile = _database conf
  let allFiles = getAllFiles conf
  initDatabase dbFile
  parsedFiles <- mapM (readTaskFile orgFileParser) allFiles
  let fState = M.fromList (fmap (\(fp, (_, result)) -> (fp, result)) parsedFiles)
  void $ withDatabase dbFile $ \conn ->
    importFileState conn fState
  let ops = mkTaskStoreOps (DatabaseStore dbFile)
      sysConf = (mkSystemConfig (_commands conf)) {_taskStoreOps = Just ops}
  tui sysConf

readTaskFile :: Parser (TaskFile Task) -> FilePath -> IO (FilePath, (Location, ParserResult (TaskFile Task)))
readTaskFile parser f = do
  content <- readFileExample f
  let (loc, _, taskFile) = runParser parser content
  return (f, (loc, taskFile))

mkSystemConfig :: S.Set T.Text -> SystemConfig Task
mkSystemConfig commandsConfig =
  SystemConfig
    { _taskParser = anyTaskparser,
      _fileParser = orgFileParser,
      _keybindings = orgKeyBindings allCommands commandsConfig,
      _defaultFilters = [todoKeywordFilter "INBOX"],
      _defaultSorter = sortByCreatedDesc,
      _taskStoreOps = Nothing
    }
