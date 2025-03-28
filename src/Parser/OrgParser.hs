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
import Model.OrgMode
import Parser.Parser
import Parser.StandardParsers
import TextUtils

-------------------------------- ORG MODE -------------------------------------

taskLevelParser :: Parser Int
taskLevelParser = failOnConditionParser parser (<= 0) errorMsg
 where
  parser = fmap T.length (takeWhileParser (== '*'))
  errorMsg = "Task level must be specified with at least one '*'"

-- TODO: make it read only uppercase letters
todoKeyWordParser :: Parser T.Text
todoKeyWordParser = wordParser

priorityParser :: Parser Int
priorityParser = stringParser "[#" *> letterToPriorityParser <* charParser ']'
 where
  letterToPriorityParser = failOnConditionParser (fmap (\c -> ord c - ord 'A') singleCharParser) (\i -> i < 0 || i > ord 'Z' - ord 'A') "Got invalid priority letter"

isTagChar :: Char -> Bool
isTagChar c = isLower c || isDigit c

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

-- TODO: Generalize all those functions below
toOrgDateTime :: T.Text -> Maybe LocalTime
toOrgDateTime str = parseTimeM True defaultTimeLocale orgDayTimeFormat (T.unpack str)

toOrgDate :: T.Text -> Maybe Day
toOrgDate str = parseTimeM True defaultTimeLocale orgDayFormat (T.unpack str)

toOrgTime :: T.Text -> Maybe TimeOfDay
toOrgTime str = parseTimeM True defaultTimeLocale orgTimeFormat (T.unpack str)

parseRepeatInterval :: Parser RepeatInterval
parseRepeatInterval = RepeatInterval <$> parseEnum stringParser allRepeatTypes <*> positiveIntParser <*> parseEnum charParser allTimeUnits

parseDelayInterval :: Parser DelayInterval
parseDelayInterval = DelayInterval <$> parseEnum stringParser allDelayTypes <*> positiveIntParser <*> parseEnum charParser allTimeUnits

-- parseDateOrDateTime :: T.Text -> ParserResult OrgTime
-- parseDateOrDateTime input = do
--   let parts = T.splitOn " " input
--   -- TODO: parse by parts instead of parsing full date at once
--   case parts of
--     [] -> ParserFailure "Can't parse dateTime, no input provided"
--     [date] -> ParserFailure "Invalid date format, should have day of the week"
--     [_, _] -> case toOrgDate input of
--       Just t -> ParserSuccess $ OrgTime (Left t) Nothing Nothing
--       Nothing -> ParserFailure "Could not parse dateTime"
--     -- [date, weekDay, something] -> case (toOrgTime something, parseDelayInterval something) of {}
--     -- (Just time, _) -> _
--     (_, ParserSuccess delayInterval) -> _

-- Something is either a time or scheduled thing
--   Just t -> Just $ OrgTime (Right t) Nothing Nothing
--   Nothing -> _ -- parse something into repeat interval or delay interval
-- _ -> Nothing

-- parseDateOrDateTime :: T.Text -> Maybe OrgTime
-- parseDateOrDateTime input =
--   case (toOrgDateTime input, toOrgDate input) of
--     (Just dateTime, _) -> Just $ OrgTime (Right dateTime) Nothing Nothing -- TODO:
--     (Nothing, Just date) -> Just $ OrgTime (Left date) Nothing Nothing -- TODO:
--     _ -> Nothing

-- (parseDelim leftDelim) *>
-- parseDateAndWeek <*>
-- maybeParser parseTime <*>
-- maybeParser parseRepeatInterval <*
-- (parseDelim rightDelim)

-- Example:
-- 2005-10-01 Sat +1m -3d
--

parseDateAndWeek :: Parser Day
parseDateAndWeek =
  splitP
    >>= ( \case
            Just tt -> succeedingParser tt
            Nothing -> failingParser $ "Could not parse date, expected format: " ++ orgDayFormat
        )
      . toOrgDate
 where
  splitP = splitParser (\t -> (T.take dateAndWeekLen t, T.drop dateAndWeekLen t))
  -- Example: 2005-10-01 Sat
  dateAndWeekLen = 4 + 1 + 2 + 1 + 2 + 1 + 3

parseTime :: Parser TimeOfDay
parseTime =
  splitP
    >>= ( \case
            Just tt -> succeedingParser tt
            Nothing -> failingParser $ "Could not parse time, expected format: " ++ orgTimeFormat
        )
      . toOrgTime
 where
  splitP = splitParser (\t -> (T.take dateAndWeekLen t, T.drop dateAndWeekLen t))
  -- Example: 2005-10-01 Sat
  dateAndWeekLen = 2 + 1 + 2

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

-- dateTimeParser :: Char -> Parser (Maybe OrgTime)
-- dateTimeParser delimiterRight = fmap parseDateOrDateTime (splitParser (T.span (/= delimiterRight)))

-- TODO: return error instead of nothing
-- TODO: parse time schedules like .+1m
timePropertyParser :: T.Text -> (Char, Char) -> Parser OrgTime
timePropertyParser field (delimiterLeft, delimiterRight) =
  stringParser field
    *> charParser ':'
    *> skipBlanksParser
    *> charParser delimiterLeft
    *> dateTimeParserReimplemented
    -- \*> dateTimeParser delimiterRight
    <* charParser delimiterRight

-- TODO: return error instead of nothing, so the last pure Nothing won't be
-- necessary
scheduledOrDeadLineParser :: Parser (Maybe (T.Text, OrgTime))
scheduledOrDeadLineParser = foldr ((<|>) . makeP) (pure Nothing) orgTimeFields
 where
  makeP (TimeField field delim) = fmap (fmap (field,)) (maybeParser $ timePropertyParser field (delims delim))

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

descriptionParser :: Parser T.Text
descriptionParser = removeLeadingSpaces <$> takeUntilDelimParser "\n*"

findProp :: TimeField -> [(T.Text, a)] -> Maybe a
findProp field l = snd <$> find (\(n, _) -> n == timeFieldName field) l

removeDelimiters :: T.Text -> Maybe T.Text
removeDelimiters t
  | T.length t >= 2 = Just $ T.init $ T.tail t
  | otherwise = Nothing

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
    <*> (skipBlanksParser *> descriptionParser)

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
