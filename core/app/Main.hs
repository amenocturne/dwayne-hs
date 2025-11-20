{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Commands.Registry (allCommands)
import Data.Yaml.Aeson (ParseException, decodeFileEither)
import Model.OrgMode (Task)
import Parser.OrgParser (anyTaskparser, orgFileParser)
import Refile.OrgRefileable ()
import Render.OrgRender ()
import Searcher.OrgSearcher ()
import TextUtils (getConfigPath)
import Tui.Keybindings (orgKeyBindings, sortByCreatedDesc, todoKeywordFilter)
import Tui.Tui
import Tui.Types
import Writer.OrgWriter ()

-- | Main entry point for the application
main :: IO ()
main = runTui

-- | Function to run the TUI
runTui :: IO ()
runTui = do
  -- Load config first to get enabled commands list
  configFilePath <- getConfigPath
  parsedConfig <- decodeFileEither configFilePath :: IO (Either ParseException (AppConfig Task))

  let commandsConfig = case parsedConfig of
        Right appConfig -> _commands appConfig
        Left _ -> Nothing -- If config fails to parse, enable all commands
  tui
    SystemConfig
      { _taskParser = anyTaskparser,
        _fileParser = orgFileParser,
        _keybindings = orgKeyBindings allCommands commandsConfig,
        _defaultFilters = [todoKeywordFilter "INBOX"],
        _defaultSorter = sortByCreatedDesc
      }
