{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Parser.OrgParser (anyTaskparser, orgFileParser)
import Tui.Tui
import Writer.OrgWriter ()
import Render.OrgRender ()

main :: IO ()
main = tui AppConfig{taskParser = anyTaskparser, fileParser = orgFileParser, files = ["./resources/Phone.org"], scrollingMargin = 6}
