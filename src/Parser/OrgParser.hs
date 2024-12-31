{-# LANGUAGE DeriveFunctor #-}
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
import Data.List (find)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time
import GHC.Base hiding (foldr)
import Parser.Parser
import Parser.StandardParsers
import TextUtils

---------------------------------- TREE ---------------------------------------

maybeLastAndRest :: [a] -> ([a], Maybe a)
maybeLastAndRest [] = ([], Nothing)
maybeLastAndRest [x] = ([], Just x)
maybeLastAndRest (x : xs) = (x : rest, last)
 where
  (rest, last) = maybeLastAndRest xs

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither Nothing e = Left e
maybeToEither (Just a) _ = Right a

-- Forest is represented as a "root" empty node and children of it are root
-- nodes of trees in that forest
data Forest a = Forest [Forest a] | Node a [Forest a] deriving (Show, Functor)

leaf :: a -> Forest a
leaf a = Node a []

-- Appends new tree to a node
-- Level 0 means under root node
appendToAtLevel :: Forest a -> Forest a -> Int -> Either String (Forest a)
appendToAtLevel tree node level
  | level == 0 = case tree of
      Forest nodes -> Right $ Forest (nodes ++ [node])
      Node a nodes -> Right $ Node a (nodes ++ [node])
  | level > 0 = case tree of
      Forest nodes -> do
        let (rest, maybeLast) = maybeLastAndRest nodes
        last <- maybeToEither maybeLast "Can only add nodes to level 0 in empty forest"
        appendedLast <- appendToAtLevel last node (level - 1)
        Right $ Forest (rest ++ [appendedLast])
      Node a nodes -> do
        let (rest, maybeLast) = maybeLastAndRest nodes
        last <- maybeToEither maybeLast "Can only add nodes to level 0 in empty node"
        appendedLast <- appendToAtLevel last node (level - 1)
        Right $ Node a (rest ++ [appendedLast])
  | otherwise = Left "Cannot insert at negative level"

makeForest :: (Show a) => [a] -> (a -> Int) -> Either String (Forest a)
makeForest list getLevel = foldl append (Right $ Forest []) list
 where
  append (Left e) _ = Left e
  append (Right acc) value = appendToAtLevel acc (leaf value) (getLevel value)

-------------------------------- ORG MODE -------------------------------------

data TaskFile = TaskFile
  { name :: Maybe T.Text
  , content :: Forest Task
  }
  deriving (Show)

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

toOrgDateTime :: T.Text -> Maybe UTCTime
toOrgDateTime str = parseTimeM True defaultTimeLocale "%Y-%m-%d %a %H:%M" (T.unpack str)

toOrgDate :: T.Text -> Maybe Day
toOrgDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d %a" (T.unpack str)

type OrgTime = Either Day UTCTime

parseDateOrDateTime :: T.Text -> Maybe OrgTime
parseDateOrDateTime input =
  case (toOrgDateTime input, toOrgDate input) of
    (Just dateTime, _) -> Just $ Right dateTime
    (Nothing, Just date) -> Just $ Left date
    _ -> Nothing

dateTimeParser :: Char -> Parser (Maybe OrgTime)
dateTimeParser delimiterRight = fmap parseDateOrDateTime (splitParser (T.span (/= delimiterRight)))

-- TODO: return error instead of nothing
timePropertyParser :: T.Text -> (Char, Char) -> Parser (Maybe OrgTime)
timePropertyParser field (delimiterLeft, delimiterRight) =
  stringParser field
    *> charParser ':'
    *> skipBlanksParser
    *> charParser delimiterLeft
    *> dateTimeParser delimiterRight
    <* charParser delimiterRight

-- TODO: return error instead of nothing, so the last pure Nothing won't be
-- necessary
scheduledOrDeadLineParser :: Parser (Maybe (T.Text, OrgTime))
scheduledOrDeadLineParser =
  makeP "SCHEDULED" angleDelim
    <|> makeP "DEADLINE" angleDelim
    <|> makeP "CLOSED" bracketDelim
    <|> pure Nothing
 where
  angleDelim = ('<', '>')
  bracketDelim = ('[', ']')
  makeP field delim = fmap (fmap (field,)) (timePropertyParser field delim)

propertyParser :: Parser (T.Text, T.Text)
propertyParser =
  charParser ':'
    *> ( (\a _ b -> (a, b))
          <$> wordParser
          <*> charParser ':'
          <*> tillTheEndOfStringParser
          <* charParser '\n'
       )

propertiesParser :: Parser [(T.Text, T.Text)]
propertiesParser =
  stringParser ":PROPERTIES:"
    *> skipBlanksParser
    *> many propertyParser
    <* skipBlanksParser
    <* stringParser ":END:"

descriptionParser :: Parser T.Text
descriptionParser = takeUntilDelimParser "\n*"

data Task = Task
  { level :: Int
  , todoKeyword :: T.Text
  , priority :: Maybe Int
  , title :: T.Text
  , tags :: [T.Text]
  , scheduled :: Maybe OrgTime
  , deadline :: Maybe OrgTime
  , closed :: Maybe OrgTime
  , created :: Maybe OrgTime
  , properties :: [(T.Text, T.Text)]
  , description :: T.Text
  }
  deriving (Show)

findProp :: T.Text -> [(T.Text, a)] -> Maybe a
findProp name l = snd <$> find (\(n, _) -> n == name) l

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
          (findProp "SCHEDULED" propsList)
          (findProp "DEADLINE" propsList)
          (findProp "CLOSED" propsList)
          (findProp "CREATED" properties >>= parseDateOrDateTime)
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
          (findProp "SCHEDULED" propsList)
          (findProp "DEADLINE" propsList)
          (findProp "CLOSED" propsList)
          (findProp "CREATED" properties >>= parseDateOrDateTime)
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
          (findProp "SCHEDULED" propsList)
          (findProp "DEADLINE" propsList)
          (findProp "CLOSED" propsList)
          Nothing
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

allTasksParser :: Parser (Forest Task)
allTasksParser =
  many anyTaskparser
    >>= ( \case
            Left err -> failingParser $ "Forest Construction Failed: " ++ err
            Right forest -> succeedingParser forest
        )
      . (`makeForest` (\t -> level t - 1))

orgFileParser :: Parser TaskFile
orgFileParser = fmap (uncurry TaskFile) parser
 where
  fileTitleParser = maybeParser $ stringParser "#+TITLE: " *> tillTheEndOfStringParser <* skipBlanksParser
  parser = (,) <$> fileTitleParser <*> allTasksParser
