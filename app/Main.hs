{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

-- TODO: Should handle external edits to files when the app is opened and update its state correctly

import Parser.OrgParser (anyTaskparser, orgFileParser)
import Render.OrgRender ()
import Tui.Tui
import Tui.Types
import Writer.OrgWriter ()

-- filtered :: Parser (TaskFile Task)
-- filtered = fmap (\t -> t{content = filter hasBrokenProperty (content t)}) orgFileParser
--  where
--   hasBrokenProperty task = elem "BROKEN_PROPERTIES" $ fmap fst (properties task)
--   hasBrokenDescription task = elem "BROKEN_DESCRIPTION" $ fmap fst (properties task)
main :: IO ()
main = do
  tui
    AppConfig
      { _taskParser = anyTaskparser
      , _fileParser = orgFileParser
      , _files = ["./resources/Sample.org"]
      , _scrollingMargin = 6
      }
