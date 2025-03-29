{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parser.OrgParser where

import Data.Char (isDigit, isLower)
import Data.Foldable
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time
import GHC.Base hiding (foldr)
import Model.Injection
import Model.OrgMode
import Parser.Parser
import Parser.StandardParsers
import TextUtils

------------------------------- Title Line -------------------------------------

taskLevelParser :: Parser Int
taskLevelParser = failOnConditionParser parser (<= 0) errorMsg
 where
  parser = fmap T.length (takeWhileParser (== '*'))
  errorMsg = "Task level must be specified with at least one '*'"

todoKeyWordParser :: Parser T.Text
todoKeyWordParser = wordParser

priorityParser :: Parser Int
priorityParser = stringParser "[#" *> letterToPriorityParser <* charParser ']'
 where
  letterToPriorityParser = failOnConditionParser (fmap (\c -> ord c - ord 'A') singleCharParser) (\i -> i < 0 || i > ord 'Z' - ord 'A') "Got invalid priority letter"

isTagChar :: Char -> Bool
isTagChar c = isLower c || isDigit c

titleAndTagsParser :: Parser (T.Text, [T.Text])
titleAndTagsParser = fmap splitToTitleAndTags tillTheEndOfStringParser

splitToTitleAndTags :: T.Text -> (T.Text, [T.Text])
splitToTitleAndTags input = (T.strip actualTitle, actualTags)
 where
  parts = split ':' input
  (titleParts, tagParts) = break isTag parts
  title = T.concat titleParts
  tags = filter (not . T.null) $ fmap stripLeadingColumn tagParts

  (actualTitle, actualTags) = case reverse tagParts of
    [] -> (title, [])
    x : _ -> if x == ":" then (title, tags) else (input, [])

  isTag :: T.Text -> Bool
  isTag str = case T.uncons str of
    Nothing -> False
    Just (x, xs) -> x == ':' && T.all isTagChar xs

  stripLeadingColumn :: T.Text -> T.Text
  stripLeadingColumn str = case T.uncons str of
    Nothing -> ""
    Just (x, xs) -> if x == ':' then xs else str

------------------------------- Time Fields ------------------------------------

parseInterval :: (Injection a T.Text, Injection T.Text (Maybe a)) => (a -> Int -> TimeUnit -> b) -> [a] -> Parser b
parseInterval construct allTypes = construct <$> parseEnum stringParser allTypes <*> positiveIntParser <*> parseEnum charParser allTimeUnits

parseRepeatInterval :: Parser RepeatInterval
parseRepeatInterval = parseInterval RepeatInterval allRepeatTypes

parseDelayInterval :: Parser DelayInterval
parseDelayInterval = parseInterval DelayInterval allDelayTypes

parseDateAndWeek :: Parser Day
parseDateAndWeek =
  takeParser (4 + 1 + 2 + 1 + 2 + 1 + 3) -- Example: 2005-10-01 Sat
    >>= ( \case
            Just tt -> succeedingParser tt
            Nothing -> failingParser $ "Could not parse date, expected format: " ++ orgDayFormat
        )
      . parseTimeWith orgDayFormat

parseTime :: Parser TimeOfDay
parseTime =
  takeParser (2 + 1 + 2) -- Example: 10:30
    >>= ( \case
            Just tt -> succeedingParser tt
            Nothing -> failingParser $ "Could not parse time, expected format: " ++ orgTimeFormat
        )
      . parseTimeWith orgTimeFormat

dateTimeParserReimplemented :: Parser OrgTime
dateTimeParserReimplemented =
  ( \dateAndWeek maybeTime maybeRepeatedInterval maybeDelayInterval ->
      OrgTime
        { time = makeTime dateAndWeek maybeTime
        , repeater = maybeRepeatedInterval
        , delay = maybeDelayInterval
        }
  )
    <$> parseDateAndWeek
    <*> maybeParser parseTime
    <*> maybeParser parseRepeatInterval
    <*> maybeParser parseDelayInterval
 where
  makeTime d t = case t of
    Just tt -> Right $ LocalTime d tt
    Nothing -> Left d

timePropertyParser :: T.Text -> (Char, Char) -> Parser OrgTime
timePropertyParser field (delimiterLeft, delimiterRight) =
  stringParser field
    *> charParser ':'
    *> skipBlanksParser
    *> charParser delimiterLeft
    *> dateTimeParserReimplemented
    <* charParser delimiterRight

scheduledOrDeadLineParser :: Parser (Maybe (T.Text, OrgTime))
scheduledOrDeadLineParser = maybeParser $ asum (fmap makeP orgTimeFields)
 where
  makeP (TimeField field delim) = (field,) <$> timePropertyParser field (delims delim)

------------------------------- Properties -------------------------------------

propertyParser :: Parser (T.Text, T.Text)
propertyParser =
  charParser ':'
    *> ( (\a _ _ b -> (a, b))
          <$> wordParser
          <*> stringParser ":"
          <*> skipBlanksExceptNewLinesParser
          <*> tillTheEndOfStringParser
          <* charParser '\n'
       )

propertiesParser :: Parser [(T.Text, T.Text)]
propertiesParser =
  stringParser orgPropertiesBegin
    *> skipBlanksParser
    *> many propertyParser
    <* skipBlanksParser
    <* stringParser orgPropertiesEnd

------------------------------- Properties -------------------------------------

descriptionParser :: Parser T.Text
descriptionParser = removeLeadingSpaces <$> takeUntilDelimParser "\n*"

findProp :: TimeField -> [(T.Text, a)] -> Maybe a
findProp field l = snd <$> find (\(n, _) -> n == timeFieldName field) l

properTaskParser :: Parser Task
properTaskParser =
  ( \level todoKeyword priority (title, tags) timeProp1 timeProp2 timeProp3 properties description ->
      let
        propsList = catMaybes [timeProp1, timeProp2, timeProp3]
       in
        Task
          level
          todoKeyword
          priority
          title
          tags
          (findProp orgScheduledField propsList)
          (findProp orgDeadlineField propsList)
          (findProp orgClosedField propsList)
          properties
          description
  )
    <$> (skipBlanksParser *> taskLevelParser)
    <*> (skipBlanksExceptNewLinesParser *> todoKeyWordParser)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser priorityParser)
    <*> (skipBlanksExceptNewLinesParser *> titleAndTagsParser)
    <*> (skipBlanksParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksParser *> propertiesParser)
    <*> descriptionParser

brokenDescriptionTaskParser :: Parser Task
brokenDescriptionTaskParser =
  ( \level todoKeyword priority (title, tags) description timeProp1 timeProp2 timeProp3 properties ->
      let
        propsList = catMaybes [timeProp1, timeProp2, timeProp3]
       in
        Task
          level
          todoKeyword
          priority
          title
          tags
          (findProp orgScheduledField propsList)
          (findProp orgDeadlineField propsList)
          (findProp orgClosedField propsList)
          (("BROKEN_DESCRIPTION", "TRUE") : properties)
          description
  )
    <$> (skipBlanksParser *> taskLevelParser)
    <*> (skipBlanksExceptNewLinesParser *> todoKeyWordParser)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser priorityParser)
    <*> (skipBlanksExceptNewLinesParser *> titleAndTagsParser)
    <*> (skipBlanksParser *> descriptionParser)
    <*> (skipBlanksParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksParser *> propertiesParser)

brokenPropertiesTaskParser :: Parser Task
brokenPropertiesTaskParser =
  ( \level todoKeyword priority (title, tags) timeProp1 timeProp2 timeProp3 description ->
      let
        propsList = catMaybes [timeProp1, timeProp2, timeProp3]
       in
        Task
          level
          todoKeyword
          priority
          title
          tags
          (findProp orgScheduledField propsList)
          (findProp orgDeadlineField propsList)
          (findProp orgClosedField propsList)
          [("BROKEN_PROPERTIES", "TRUE")]
          description
  )
    <$> (skipBlanksParser *> taskLevelParser)
    <*> (skipBlanksExceptNewLinesParser *> todoKeyWordParser)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser priorityParser)
    <*> (skipBlanksExceptNewLinesParser *> titleAndTagsParser)
    <*> (skipBlanksParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> descriptionParser

anyTaskparser :: Parser Task
anyTaskparser =
  properTaskParser
    <|> brokenDescriptionTaskParser
    <|> brokenPropertiesTaskParser

-- allTasksParser :: Parser (Forest Task)
-- allTasksParser =
--   many anyTaskparser
--     >>= ( \case
--             Left err -> failingParser $ "Forest Construction Failed: " ++ err
--             Right forest -> succeedingParser forest
--         )
--       . (`makeForest` (\t -> level t - 1))
allTasksParser :: Parser [Task]
allTasksParser = many anyTaskparser

orgFileParser :: Parser TaskFile
orgFileParser = fmap (uncurry TaskFile) parser
 where
  fileTitleParser = maybeParser $ stringParser "#+TITLE: " *> tillTheEndOfStringParser <* skipBlanksParser
  parser = (,) <$> fileTitleParser <*> allTasksParser
