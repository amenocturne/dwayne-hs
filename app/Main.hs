{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Parser.OrgParser (anyTaskparser, orgFileParser)
import Render.OrgRender ()
import Searcher.OrgSearcher ()
import Tui.Keybindings (orgKeyBindings, sortByCreatedDesc, todoKeywordFilter)
import Tui.Tui
import Tui.Types
import Writer.OrgWriter ()
import Refile.OrgRefileable ()

-- TODO: add cli tool to quickly add tasks and integrate with other apps

main :: IO ()
main =
  tui
    SystemConfig
      { _taskParser = anyTaskparser
      , _fileParser = orgFileParser
      , _keybindings = orgKeyBindings
      , _defaultFilters = [todoKeywordFilter "INBOX"]
      , _defaultSorter = sortByCreatedDesc
      }
