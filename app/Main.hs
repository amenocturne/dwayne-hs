{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Model.OrgMode
import Parser.OrgParser (anyTaskparser, orgFileParser)
import Parser.Parser
import Render.OrgRender ()
import Tui.Tui
import Writer.OrgWriter ()

-- filtered :: Parser (TaskFile Task)
-- filtered = fmap (\t -> t{content = filter hasBrokenProperty (content t)}) orgFileParser
--  where
--   hasBrokenProperty task = elem "BROKEN_PROPERTIES" $ fmap fst (properties task)
--   hasBrokenDescription task = elem "BROKEN_DESCRIPTION" $ fmap fst (properties task)
main :: IO ()
main = tui AppConfig{taskParser = anyTaskparser, fileParser = orgFileParser, files = ["./resources/Phone.org"], scrollingMargin = 6}
