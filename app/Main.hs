{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Parser.OrgParser
import Parser.Parser (runParser)
import TextUtils (readFileExample)

main :: IO ()
main = do
  content <- readFileExample "./resources/Sample.org"
  let (_, _, tasks) = runParser allTasksParser content
  print (fmap (fmap (\x -> (level x, title x))) tasks)
