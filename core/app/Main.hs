{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Api.Server (runServer)
import Commands.CliHelpers (loadFileState)
import Commands.Command (Command (..))
import Commands.Registry (allCommands)
import Control.Monad (join)
import DB.TaskStore (DatabaseStore (..), mkTaskStoreOps)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Version (showVersion)
import Model.OrgMode (Task)
import Options.Applicative (CommandFields, Mod, command, execParser, fullDesc, header, helper, info, progDesc, subparser, (<**>))
import Parser.OrgParser (anyTaskparser, orgFileParser)
import Paths_dwayne_hs (version)
import Refile.OrgRefileable ()
import Render.OrgRender ()
import Searcher.OrgSearcher ()
import System.Environment (getArgs)
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
    ["--version"] -> putStrLn $ "dwayne " ++ showVersion version
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
  let name' = T.unpack (cmdAlias cmd)
      desc = T.unpack (cmdDescription cmd)
  return $ command name' (info cliParser (progDesc desc))

startWebServer :: IO ()
startWebServer = do
  putStrLn "Starting server on http://localhost:8080"
  ctx <- initializeAppContext
  runServer 8080 ctx

initializeAppContext :: IO (AppContext Task)
initializeAppContext = do
  (conf, fState) <- loadFileState
  let dbFile = _database conf
      ops = mkTaskStoreOps (DatabaseStore dbFile)
      sysConf = (mkSystemConfig (_commands conf)) {_taskStoreOps = Just ops}
  return $ initializeAppContextForServer sysConf conf fState

startTui :: IO ()
startTui = do
  (conf, _fState) <- loadFileState
  let dbFile = _database conf
      ops = mkTaskStoreOps (DatabaseStore dbFile)
      sysConf = (mkSystemConfig (_commands conf)) {_taskStoreOps = Just ops}
  tui sysConf

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
