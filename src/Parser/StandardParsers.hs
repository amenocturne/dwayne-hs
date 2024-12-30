{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use ==" #-}

module Parser.StandardParsers where

import Data.Char (isLetter, isSpace)
import Data.Functor (void)
import qualified Data.Text as T
import Parser.Parser
import TextUtils

---------------------------- STANDARD PARSERS ---------------------------------

charParser :: Char -> Parser Char
charParser c = Parser f
 where
  makeError got = ParserFailure $ "Expected character '" ++ [c] ++ "', but got '" ++ got ++ "'"
  f (modLoc, str) = case T.uncons str of
    Nothing -> ((modLoc, ""), makeError "empty")
    Just (x, xs) -> if x == c then ((modLoc . shiftLocationByString (T.pack [x]), xs), ParserSuccess x) else ((modLoc, T.cons x xs), makeError [x])

singleCharParser :: Parser Char
singleCharParser = Parser f
 where
  f (modLoc, str) = case T.uncons str of
    Nothing -> ((modLoc, ""), ParserFailure "Input is empty, but expected a character")
    Just (x, xs) -> ((modLoc . shiftLocationByString (T.pack [x]), xs), ParserSuccess x)

-- TODO: Make nice errors, so that it would display that it expected the whole
-- word, not just character
-- stringParser :: Text -> Parser Text
-- stringParser t = fmap T.pack (traverse charParser (T.unpack t))
stringParser :: T.Text -> Parser T.Text
stringParser t = fmap T.pack (traverse charParser (T.unpack t))

splitParser :: (T.Text -> (T.Text, T.Text)) -> Parser T.Text
splitParser f = Parser $ \(loc, str) ->
  let (matches, rest) = f str
   in ((loc . shiftLocationByString matches, rest), ParserSuccess matches)

takeWhileParser :: (Char -> Bool) -> Parser T.Text
takeWhileParser f = splitParser $ T.span f

takeUntilDelimParser :: T.Text -> Parser T.Text
takeUntilDelimParser delim = splitParser $ splitByFirstDelimiter delim

wordParser :: Parser T.Text
wordParser = takeWhileParser (\c -> isLetter c || elem c ['_'])

-- NOTE: skips all white spaces except new lines
skipBlanksExceptNewLinesParser :: Parser ()
skipBlanksExceptNewLinesParser = void $ takeWhileParser isWhitespaceExceptNewline

skipBlanksParser :: Parser ()
skipBlanksParser = void $ takeWhileParser isSpace

tillTheEndOfStringParser :: Parser T.Text
tillTheEndOfStringParser =
  takeWhileParser (/= '\n') >>= \case
    "" -> Parser $ \i -> (i, ParserFailure "Nothing to parse, line already ended")
    l -> Parser $ \i -> (i, ParserSuccess l)

failOnConditionParser :: Parser a -> (a -> Bool) -> ParserError -> Parser a
failOnConditionParser p cond err = p >>= \r -> if cond r then failingParser err else pure r

maybeParser :: Parser a -> Parser (Maybe a)
maybeParser (Parser p) = Parser $ \i ->
  let (i', r) = p i
   in case r of
        ParserSuccess s -> (i', ParserSuccess (Just s))
        ParserFailure _ -> (i, ParserSuccess Nothing)
