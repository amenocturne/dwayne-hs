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
import Tui.Keybindings (normalModeBindings)
import Tui.Tui
import Tui.Types
import Writer.OrgWriter ()

main :: IO ()
main = do
  tui $
    AppConfig
      { _taskParser = anyTaskparser
      , _fileParser = orgFileParser
      , _files =
          [ "./resources/Sample.org"
          , "./resources/SampleInbox.org"
          ]
      , _inboxFile = "./resources/SampleInbox.org"
      , _scrollingMargin = 6
      , _keybindings = normalModeBindings
      , _keyTimeoutMs = 1000
      , _autoSave = True
      , _colorScheme = "default"
      }
