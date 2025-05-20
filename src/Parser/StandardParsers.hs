{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use ==" #-}

module Parser.StandardParsers where

import Data.Char (digitToInt, isDigit, isLetter, isSpace)
import Data.Foldable
import Data.Functor (void, ($>))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import GHC.Base
import Model.Injection
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
    Just (x, xs) -> ((modLoc . (`shiftLocationByChar` x), xs), ParserSuccess x)

singleDigitParser :: Parser Int
singleDigitParser = Parser f
 where
  f (modLoc, str) = case T.uncons str of
    Nothing -> ((modLoc, ""), ParserFailure "Input is empty, but expected a digit")
    Just (x, xs) ->
      ( if isDigit x
          then ((modLoc . (`shiftLocationByChar` x), xs), ParserSuccess (digitToInt x))
          else ((modLoc, T.cons x xs), ParserFailure $ "Expected a digit, but got " ++ [x])
      )

positiveIntParser :: Parser Int
positiveIntParser = sum . fmap (\(i, d) -> d * 10 ^ i) . zip [0 ..] . reverse <$> parser
 where
  parser = tryParser $ (:) <$> singleDigitParser <*> many singleDigitParser

stringParser :: T.Text -> Parser T.Text
stringParser t = tryParser $ mapError (\e -> "Expected `" ++ T.unpack t ++ "` but failed with: " ++ e) $ fmap T.pack (traverse charParser (T.unpack t))

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
    "" -> failingParser "Nothing to parse, line already ended"
    l -> succeedingParser l

failOnConditionParser :: Parser a -> (a -> Bool) -> ParserError -> Parser a
failOnConditionParser p cond err = tryParser $ p >>= \r -> if cond r then failingParser err else pure r

mapError :: (ParserError -> ParserError) -> Parser a -> Parser a
mapError f (Parser p) = Parser $ \i ->
  let (i', r) = p i
   in case r of
        ParserSuccess s -> (i', ParserSuccess s)
        ParserFailure e' -> (i, ParserFailure $ f e')

maybeParser :: Parser a -> Parser (Maybe a)
maybeParser (Parser p) = Parser $ \i ->
  let (i', r) = p i
   in case r of
        ParserSuccess s -> (i', ParserSuccess (Just s))
        ParserFailure _ -> (i, ParserSuccess Nothing)

unMaybeParser :: ParserError -> Parser (Maybe a) -> Parser a
unMaybeParser e (Parser p) = Parser $ \i ->
  let (i', r) = p i
   in case r of
        ParserSuccess (Just s) -> (i', ParserSuccess s)
        ParserSuccess Nothing -> (i, ParserFailure e)
        ParserFailure e' -> (i, ParserFailure e')

parseEnum :: (Injection b (Maybe a), Injection a b) => (b -> Parser b) -> [a] -> Parser a
parseEnum makeConstParser enumValues = tryParser $ unMaybeParser "MUST NEVER HAPPEN" $ fmap to (asum $ fmap (makeConstParser . to) enumValues)

takeParser :: Int -> Parser T.Text
takeParser n = splitParser (\t -> (T.take n t, T.drop n t))

manyStrict :: Parser a -> Parser [a]
manyStrict (Parser parserFn) = Parser $ \input ->
  let go acc (currentLocFun, remainingText) =
        case parserFn (currentLocFun, remainingText) of
          ((newLocFun, newRemaining), ParserSuccess x) ->
            go (acc ++ [x]) (newLocFun, newRemaining)
          ((_, _), ParserFailure err) ->
            if not (null acc) || T.null remainingText
              then
                ((currentLocFun, remainingText), ParserSuccess acc)
              else ((currentLocFun, remainingText), ParserFailure err)
   in go [] input

-- a tiny combinator that “tries p but rolls back location on failure”
tryParser :: Parser a -> Parser a
tryParser (Parser runP) = Parser $ \(locFun, txt) ->
  case runP (locFun, txt) of
    ((newLocFun, rest), ParserSuccess x) -> ((newLocFun, rest), ParserSuccess x)
    (_, ParserFailure e) -> ((locFun, txt), ParserFailure e)

-- Generalized combinator for trying all parsers and selecting based on a rule
tryAllWith :: (Eq a) => ([a] -> Maybe a) -> [Parser a] -> Parser a
tryAllWith selector parsers = Parser $ \input@(modLoc, _) ->
  let results =
        [ (modLoc', remaining, res)
        | (Parser p) <- parsers
        , let ((modLoc', remaining), res) = p input
        , isParserSuccess res
        ]
      ss = mapMaybe (\(_, _, c) -> resultToMaybe c) results
   in case selector ss of
        Just val ->
          let (bestModLoc, bestInput, bestRes) = head $ filter (\(_, _, c) -> Just val == resultToMaybe c) results
           in ((modLoc . bestModLoc, bestInput), bestRes)
        _ -> (input, ParserFailure "No valid selection")

-- Turn a parser to a parser that does not consume input string or modifies location
notConsumingInput :: Parser a -> Parser a
notConsumingInput (Parser runP) = Parser $ \(modLoc, str) ->
  case runP (modLoc, str) of
    (_, x) -> ((modLoc, str), x)

-- TODO: to fix performance with this function I can take until "\n*" and only
-- then check if it parses to title line and if not repeat this action

-- consumes all and outputs all the text char by char until supplied parser succeeds
takeUntilSucceeds :: Parser a -> Parser T.Text
takeUntilSucceeds stop = Parser $ \(locFn, input) ->
  let go accText remainingText =
        if T.null remainingText
          then ((locFn . shiftLocationByString accText, remainingText), ParserSuccess accText)
          else
            let stopParser = notConsumingInput stop
                ((_, _), stopResult) = runP stopParser (id, remainingText)
             in case stopResult of
                  ParserSuccess _ -> ((locFn . shiftLocationByString accText, remainingText), ParserSuccess accText)
                  _ -> case T.uncons remainingText of
                    Just (c, rest) ->
                      go (T.snoc accText c) rest
                    Nothing -> ((locFn, ""), ParserSuccess accText)
   in go T.empty input
 where
  runP (Parser p) = p

-- TODO: refactor
-- Efficiently takes text until finding a delimiter followed by text matching the given parser

{- | Parser that efficiently takes text until finding a valid boundary.
A valid boundary is a delimiter followed by text that matches the stop parser.
-}
takeUntilDelimThenSucceeds :: T.Text -> Parser a -> Parser T.Text
takeUntilDelimThenSucceeds delim stopP = Parser $ \input ->
  parseText T.empty input
 where
  parseText :: T.Text -> ParserInput -> (ParserInput, ParserResult T.Text)
  parseText acc (locFn, remaining)
    -- End of input or empty input case
    | T.null remaining =
        finishWith acc "" locFn
    -- Still have text to process
    | otherwise =
        let (before, after) = splitByFirstDelimiter delim remaining
            newAcc = T.append acc before
            newLoc = locFn . shiftLocationByString newAcc
         in handleAfterText newAcc after newLoc

  -- Process the text after finding a potential delimiter
  handleAfterText :: T.Text -> T.Text -> (Location -> Location) -> (ParserInput, ParserResult T.Text)
  handleAfterText acc after locFn
    -- No delimiter was found (or end of input)
    | T.null after =
        finishWith acc "" locFn
    -- Found delimiter - check if it's followed by a valid stop point
    | isValidStopPoint after =
        finishWith acc after locFn
    -- Delimiter followed by invalid stop point - continue parsing
    | otherwise =
        let nextAcc = T.append acc delim
            nextRemaining = T.drop (T.length delim) after
         in parseText nextAcc (locFn, nextRemaining)

  -- Create successful result with the given accumulated text and remainder
  finishWith :: T.Text -> T.Text -> (Location -> Location) -> (ParserInput, ParserResult T.Text)
  finishWith acc remainder locFn =
    ((locFn, remainder), ParserSuccess acc)

  -- Check if text begins with a pattern matching the stop parser
  isValidStopPoint :: T.Text -> Bool
  isValidStopPoint text =
    case runNonConsumingParser text of
      ParserSuccess _ -> True
      _ -> False

  -- Run parser without consuming input
  runNonConsumingParser text =
    let (Parser pFn) = notConsumingInput stopP
        ((_, _), result) = pFn (id, text)
     in result
