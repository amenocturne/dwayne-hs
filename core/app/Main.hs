{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Api.Server (runServer)
import Commands.Registry (allCommands)
import Data.Aeson (Object)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import Data.Yaml.Aeson (ParseException, decodeFileEither)
import Model.OrgMode (Task, TaskFile)
import Parser.OrgParser (anyTaskparser, orgFileParser)
import Parser.Parser (Location, ParserResult, errorToMaybe, runParser)
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
    ["--serve"] -> startWebServer
    _ -> startTui

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
    Right conf -> expandConfigPaths conf

  let allFiles = getAllFiles conf
      sysConf = mkSystemConfig (_commands conf)
  
  parsedFiles <- mapM readTaskFile allFiles
  let fState = fileStateFromParsedFiles parsedFiles
      parsingErrors = extractParsingErrors parsedFiles

  case parsingErrors of
    [] -> return ()
    errs -> do
      putStrLn "WARNING: Parsing errors found:"
      mapM_ printParsingError errs

  return $ initializeAppContextForServer sysConf conf fState
  where
    readTaskFile :: FilePath -> IO (FilePath, (Location, ParserResult (TaskFile Task)))
    readTaskFile f = do
      content <- readFileExample f
      let (loc, _, taskFile) = runParser orgFileParser content
      return (f, (loc, taskFile))
    
    fileStateFromParsedFiles = M.fromList . fmap (\(fp, (_, result)) -> (fp, result))
    
    extractParsingErrors = mapMaybe (\(fp, (loc, result)) -> fmap (\e -> (fp, loc, e)) (errorToMaybe result))
    
    printParsingError (fp, loc, err) =
      putStrLn $ "  " ++ fp ++ ": " ++ err ++ " at " ++ show loc

startTui :: IO ()
startTui = do
  configFilePath <- getConfigPath
  parsedConfig <- decodeFileEither configFilePath :: IO (Either ParseException (AppConfig Task))

  let commandsConfig = case parsedConfig of
        Right appConfig -> _commands appConfig
        Left _ -> Nothing
  tui (mkSystemConfig commandsConfig)

mkSystemConfig :: Maybe Object -> SystemConfig Task
mkSystemConfig commandsConfig =
  SystemConfig
    { _taskParser = anyTaskparser,
      _fileParser = orgFileParser,
      _keybindings = orgKeyBindings allCommands commandsConfig,
      _defaultFilters = [todoKeywordFilter "INBOX"],
      _defaultSorter = sortByCreatedDesc
    }
