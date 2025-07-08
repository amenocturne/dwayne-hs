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

import Control.Monad (guard, void)
import Data.Bifunctor
import Data.Char (isDigit, isLower)
import Data.Foldable
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
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
todoKeyWordParser =
  mapError
    (const $ "Could not parse any of existing todo keywords: " ++ T.unpack (T.intercalate " " orgTodoKeyWords))
    (asum $ fmap stringParser orgTodoKeyWords)

priorityParser :: Parser Int
priorityParser = tryParser $ stringParser "[#" *> letterToPriorityParser <* charParser ']'
 where
  letterToPriorityParser = failOnConditionParser (fmap (\c -> ord c - ord 'A') singleCharParser) (\i -> i < 0 || i > ord 'Z' - ord 'A') "Got invalid priority letter"

isTagChar :: Char -> Bool
isTagChar c = isLower c || isDigit c || c == '_'

titleAndTagsParser :: Parser (T.Text, [T.Text])
titleAndTagsParser = fmap splitToTitleAndTags tillTheEndOfStringParser

splitToTitleAndTags :: T.Text -> (T.Text, [T.Text])
splitToTitleAndTags input = fromMaybe (T.strip input, []) $ do
  let tagsPart = T.takeWhileEnd isTagPartChar input
  let titlePart = T.dropWhileEnd isTagPartChar input
  let (nonTagPrefix, actualTagsPart) = T.breakOn ":" tagsPart

  -- A tag string must have at least two characters, e.g., "::"
  guard (T.length actualTagsPart >= 2)

  -- Must start and end with a colon
  guard (T.head actualTagsPart == ':' && T.last actualTagsPart == ':')

  let title = T.strip (titlePart <> nonTagPrefix)

  -- All characters in tags must be valid tag characters
  let tags = filter (not . T.null) $ T.splitOn ":" actualTagsPart
  guard (all (T.all isTagChar) tags)

  return (title, tags)
 where
  isTagPartChar c = isTagChar c || c == ':'

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
    <*> (skipBlanksExceptNewLinesParser *> maybeParser parseTime)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser parseRepeatInterval)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser parseDelayInterval)
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

scheduledClosedDeadLineParser :: Parser (Maybe (T.Text, OrgTime))
scheduledClosedDeadLineParser = maybeParser $ asum (fmap makeP orgTimeFields)
 where
  makeP (TimeField field delim) = (field,) <$> timePropertyParser field (to delim)

------------------------------- Properties -------------------------------------

propertyParser :: Parser (T.Text, T.Text)
propertyParser =
  mapError (const "Could not read property") $
    tryParser $
      (,)
        <$> (skipBlanksExceptNewLinesParser *> charParser ':' *> skipBlanksExceptNewLinesParser *> wordParser)
        <*> ( skipBlanksExceptNewLinesParser
                *> charParser ':'
                *> skipBlanksExceptNewLinesParser
                *> failOnConditionParser tillTheEndOfStringParser T.null "Got empty property value"
            )
        <* charParser '\n'

propertiesParser :: Parser [(T.Text, T.Text)]
propertiesParser =
  skipBlanksExceptNewLinesParser
    *> stringParser orgPropertiesBegin
    *> charParser '\n'
    *> many propertyParser
    <* skipBlanksExceptNewLinesParser
    <* stringParser orgPropertiesEnd

------------------------------- Properties -------------------------------------

titleLineParser = do
  skipBlanksParser
  taskLevelParser
  charParser ' '
  skipBlanksExceptNewLinesParser
  todoKeyWordParser
  return ()

descriptionParser :: Parser T.Text
descriptionParser =
  T.strip
    <$> takeUntilDelimThenSucceeds "\n" titleLineParser

brokenDescriptionParser :: Parser T.Text
brokenDescriptionParser =
  unMaybeParser "Read empty description" $
    fmap (\t -> if T.null t then Nothing else Just t) $
      T.strip
        <$> takeUntilSucceeds
          (void propertiesParser <|> titleLineParser)

findProp :: TimeField -> [(T.Text, a)] -> Maybe a
findProp field l = snd <$> find (\(n, _) -> n == timeFieldName field) l

properTaskParser :: Parser Task
properTaskParser =
  ( \level todoKeyword priority (title, tags) timeProp1 timeProp2 timeProp3 properties description ->
      let
        propsList = catMaybes [timeProp1, timeProp2, timeProp3]
        mCreated = (snd <$> find (\p -> fst p == orgCreatedProperty) properties)
        createdParser = charParser '[' *> dateTimeParserReimplemented <* charParser ']'
        mCreatedProp = case fmap (runParser createdParser) mCreated of
          Just (_, _, ParserSuccess t) -> Just t
          _ -> Nothing
       in
        Task
          level
          todoKeyword
          priority
          title
          (S.fromList tags)
          (findProp orgScheduledField propsList)
          (findProp orgDeadlineField propsList)
          (findProp orgClosedField propsList)
          mCreatedProp
          properties
          description
  )
    <$> (skipBlanksParser *> taskLevelParser <* charParser ' ')
    <*> (skipBlanksExceptNewLinesParser *> todoKeyWordParser)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser priorityParser)
    <*> (skipBlanksExceptNewLinesParser *> titleAndTagsParser)
    <*> (skipBlanksParser *> scheduledClosedDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledClosedDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledClosedDeadLineParser)
    <*> (skipBlanksParser *> propertiesParser)
    <*> descriptionParser

brokenDescriptionTaskParser :: Parser Task
brokenDescriptionTaskParser =
  ( \level todoKeyword priority (title, tags) description properties description2 ->
      let
       in Task
            level
            todoKeyword
            priority
            title
            (S.fromList tags)
            Nothing
            Nothing
            Nothing
            Nothing
            properties
            (T.unlines [description, description2])
  )
    <$> (skipBlanksParser *> taskLevelParser <* charParser ' ')
    <*> (skipBlanksExceptNewLinesParser *> todoKeyWordParser)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser priorityParser)
    <*> (skipBlanksExceptNewLinesParser *> titleAndTagsParser)
    <*> brokenDescriptionParser
    <*> (skipBlanksParser *> propertiesParser)
    <*> descriptionParser

-- <*> (skipBlanksParser *> propertiesParser)

noPropertiesTaskParser :: Parser Task
noPropertiesTaskParser =
  ( \level todoKeyword priority (title, tags) description ->
      let
       in Task
            level
            todoKeyword
            priority
            title
            (S.fromList tags)
            Nothing
            Nothing
            Nothing
            Nothing
            []
            description
  )
    <$> (skipBlanksParser *> taskLevelParser <* charParser ' ')
    <*> (skipBlanksExceptNewLinesParser *> todoKeyWordParser)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser priorityParser)
    <*> (skipBlanksExceptNewLinesParser *> titleAndTagsParser)
    <*> descriptionParser

anyTaskparser :: Parser Task
anyTaskparser =
  properTaskParser
    <|> brokenDescriptionTaskParser
    <|> noPropertiesTaskParser

-- allTasksParser :: Parser (Forest Task)
-- allTasksParser =
--   many anyTaskparser
--     >>= ( \case
--             Left err -> failingParser $ "Forest Construction Failed: " ++ err
--             Right forest -> succeedingParser forest
--         )
--       . (`makeForest` (\t -> level t - 1))

orgFileParser :: Parser (TaskFile Task)
orgFileParser = fmap (uncurry TaskFile . second V.fromList) parser
 where
  fileTitleParser = maybeParser $ stringParser "#+TITLE: " *> tillTheEndOfStringParser <* skipBlanksParser
  parser = (,) <$> fileTitleParser <*> manyStrict anyTaskparser
