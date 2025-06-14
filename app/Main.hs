{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

-- import qualified Control.Lens as L
-- import Data.Text as T
-- import Data.Text.Internal.Search as T
-- import qualified Data.Vector as V
-- import Model.OrgMode
-- import Parser.Parser
import Parser.OrgParser (anyTaskparser, orgFileParser)
import Render.OrgRender ()
import Searcher.OrgSearcher ()
import Tui.Keybindings (normalModeBindings)
import Tui.Tui
import Tui.Types
import Writer.OrgWriter ()

-- filtered :: Parser (TaskFile Task) -> Parser (TaskFile Task)
-- filtered = fmap $ L.over content $ V.filter hasAstericsInDesc
--  where
--   hasAstericsInDesc task = not (Prelude.null (indices "*" (L.view description task)))
--   hasEmptyTodoKeyword task = T.null $ L.view todoKeyword task

main :: IO ()
main = do
  tui $ withDefaultColors
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
      , _colorScheme = defaultColorScheme -- This will be overridden by withDefaultColors, but needed for completeness
      }
