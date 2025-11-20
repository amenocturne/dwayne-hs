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
import Data.Char (isDigit, isLower, isSpace)
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

------------------------------- Rich Text --------------------------------------

parseTextToRichText :: T.Text -> RichText
parseTextToRichText text
  | T.null text = RichText [PlainText ""]
  | otherwise = RichText (go text [])
  where
    go :: T.Text -> [TextNode] -> [TextNode]
    go remaining acc
      | T.null remaining = reverse acc
      | otherwise =
          case findNextLink remaining of
            Nothing -> reverse (PlainText remaining : acc)
            Just (beforeLink, linkNode, afterLink) ->
              let acc' = if T.null beforeLink then acc else PlainText beforeLink : acc
               in go afterLink (linkNode : acc')

    findNextLink :: T.Text -> Maybe (T.Text, TextNode, T.Text)
    findNextLink t =
      let orgLinkResult = findOrgLink t
          plainUrlResult = findPlainUrl t
       in case (orgLinkResult, plainUrlResult) of
            (Just (before1, link1, after1), Just (before2, link2, after2)) ->
              if T.length before1 <= T.length before2
                then Just (before1, link1, after1)
                else Just (before2, link2, after2)
            (Just result, Nothing) -> Just result
            (Nothing, Just result) -> Just result
            (Nothing, Nothing) -> Nothing

    findOrgLink :: T.Text -> Maybe (T.Text, TextNode, T.Text)
    findOrgLink t =
      case T.breakOn "[[" t of
        (_, "") -> Nothing
        (before, rest) ->
          let afterOpen = T.drop 2 rest
           in case T.breakOn "]]" afterOpen of
                (_, "") -> Nothing
                (linkContent, afterClose) ->
                  let after = T.drop 2 afterClose
                   in case T.breakOn "][" linkContent of
                        (url, "") -> Just (before, OrgLink url Nothing, after)
                        (url, titlePart) ->
                          let titleText = T.drop 2 titlePart
                           in Just (before, OrgLink url (Just titleText), after)

    findPlainUrl :: T.Text -> Maybe (T.Text, TextNode, T.Text)
    findPlainUrl t =
      let httpPos = T.breakOn "http://" t
          httpsPos = T.breakOn "https://" t
          (before, protocol, rest) = case (httpPos, httpsPos) of
            ((b1, ""), (b2, "")) -> ("", "", "")
            ((b1, r1), (_, "")) -> (b1, "http://", T.drop 7 r1)
            ((_, ""), (b2, r2)) -> (b2, "https://", T.drop 8 r2)
            ((b1, r1), (b2, r2)) ->
              if T.length b1 <= T.length b2
                then (b1, "http://", T.drop 7 r1)
                else (b2, "https://", T.drop 8 r2)
       in if T.null protocol
            then Nothing
            else
              let urlPart = T.takeWhile (\c -> not (isSpace c)) rest
                  after = T.drop (T.length urlPart) rest
                  fullUrl = protocol <> urlPart
               in Just (before, OrgLink fullUrl Nothing, after)

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

titleAndTagsParser :: Parser (RichText, [T.Text])
titleAndTagsParser = do
  fullLine <- tillTheEndOfStringParser
  let (titleText, tags) = splitToTitleAndTags fullLine
      richTitle = parseTextToRichText titleText
  pure (richTitle, tags)

splitToTitleAndTags :: T.Text -> (T.Text, [T.Text])
splitToTitleAndTags input = fromMaybe (T.strip input, []) $ do
  let tagsPart = T.takeWhileEnd isTagPartChar input
  let titlePart = T.dropWhileEnd isTagPartChar input
  let (nonTagPrefix, actualTagsPart) = T.breakOn ":" tagsPart

  guard (T.length actualTagsPart >= 2)
  firstChar <- fst <$> T.uncons actualTagsPart
  lastChar <- snd <$> T.unsnoc actualTagsPart
  guard (firstChar == ':' && lastChar == ':')

  let title = T.strip (titlePart <> nonTagPrefix)
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
        { time = makeTime dateAndWeek maybeTime,
          repeater = maybeRepeatedInterval,
          delay = maybeDelayInterval
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

descriptionParser :: Parser RichText
descriptionParser = do
  text <- T.strip <$> takeUntilDelimThenSucceeds "\n" titleLineParser
  pure (parseTextToRichText text)

brokenDescriptionParser :: Parser RichText
brokenDescriptionParser = do
  text <-
    unMaybeParser "Read empty description" $
      fmap (\t -> if T.null t then Nothing else Just t) $
        T.strip
          <$> takeUntilSucceeds
            (void propertiesParser <|> titleLineParser)
  pure (parseTextToRichText text)

findProp :: TimeField -> [(T.Text, a)] -> Maybe a
findProp field l = snd <$> find (\(n, _) -> n == timeFieldName field) l

properTaskParser :: Parser Task
properTaskParser =
  ( \level todoKeyword priority (title, tags) timeProp1 timeProp2 timeProp3 properties description ->
      let propsList = catMaybes [timeProp1, timeProp2, timeProp3]
          mCreated = (snd <$> find (\p -> fst p == orgCreatedProperty) properties)
          createdParser = charParser '[' *> dateTimeParserReimplemented <* charParser ']'
          mCreatedProp = case fmap (runParser createdParser) mCreated of
            Just (_, _, ParserSuccess t) -> Just t
            _ -> Nothing
       in Task
            { _level = level,
              _todoKeyword = todoKeyword,
              _priority = priority,
              _title = title,
              _tags = S.fromList tags,
              _scheduled = findProp orgScheduledField propsList,
              _deadline = findProp orgDeadlineField propsList,
              _createdProp = mCreatedProp,
              _closed = findProp orgClosedField propsList,
              _properties = properties,
              _description = description
            }
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
      let mCreated = (snd <$> find (\p -> fst p == orgCreatedProperty) properties)
          createdParser = charParser '[' *> dateTimeParserReimplemented <* charParser ']'
          mCreatedProp = case fmap (runParser createdParser) mCreated of
            Just (_, _, ParserSuccess t) -> Just t
            _ -> Nothing
          combinedDesc = RichText (_unRichText description ++ _unRichText description2)
       in Task
            { _level = level,
              _todoKeyword = todoKeyword,
              _priority = priority,
              _title = title,
              _tags = S.fromList tags,
              _scheduled = Nothing,
              _deadline = Nothing,
              _createdProp = mCreatedProp,
              _closed = Nothing,
              _properties = properties,
              _description = combinedDesc
            }
  )
    <$> (skipBlanksParser *> taskLevelParser <* charParser ' ')
    <*> (skipBlanksExceptNewLinesParser *> todoKeyWordParser)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser priorityParser)
    <*> (skipBlanksExceptNewLinesParser *> titleAndTagsParser)
    <*> brokenDescriptionParser
    <*> (skipBlanksParser *> propertiesParser)
    <*> descriptionParser

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
