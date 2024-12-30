{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use ==" #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit, isLetter, isLower, isSpace)
import Data.Maybe (catMaybes)
import Data.Functor (void)
import qualified Data.Text as T
import Data.Time
import GHC.Base
import Data.List (find)

readFileExample :: FilePath -> IO T.Text
readFileExample path = T.pack . B.unpack <$> B.readFile path

splitBy :: Char -> T.Text -> [T.Text]
splitBy _ "" = []
splitBy delimiterChar inputString = T.foldr f [T.pack ""] inputString
 where
  f :: Char -> [T.Text] -> [T.Text]
  f _ [] = []
  f currentChar allStrings@(partialString : handledStrings)
    | currentChar == delimiterChar = "" : allStrings
    | otherwise = T.cons currentChar partialString : handledStrings

isWhitespaceExceptNewline :: Char -> Bool
isWhitespaceExceptNewline c = isSpace c && c /= '\n' && c /= '\r'

splitByFirstDelimiter :: T.Text -> T.Text -> (T.Text, T.Text)
splitByFirstDelimiter _ "" = ("", "")
splitByFirstDelimiter delim input
  | delim `T.isPrefixOf` input = ("", input)
  | otherwise =
      let (prefix, rest) = splitByFirstDelimiter delim (T.tail input)
       in (T.cons (T.head input) prefix, rest)

split :: Char -> T.Text -> [T.Text]
split _ "" = []
split delim str =
  let (prefix, suffix) = T.break (== delim) str
   in prefix : case T.uncons suffix of
        Nothing -> []
        Just (_, rest) -> case split delim rest of
          [] -> [":"]
          x : xs -> T.cons delim x : xs

------------------------------- PARSER ---------------------------------------

data Location = Location {line :: Int, column :: Int} deriving (Show)
zeroLocation :: Location
zeroLocation = Location 0 0

shifLocationByChar :: Location -> Char -> Location
shifLocationByChar (Location l _) '\n' = Location (l + 1) 0
shifLocationByChar (Location l c) _ = Location l (c + 1)

shiftLocationByString :: T.Text -> Location -> Location
shiftLocationByString input loc = T.foldl shifLocationByChar loc input

type ParserInput = (Location -> Location, T.Text)
type ParserError = String
data ParserResult a = ParserSuccess a | ParserFailure ParserError deriving (Functor, Show)

instance Applicative ParserResult where
  pure = ParserSuccess
  (ParserSuccess f) <*> (ParserSuccess a) = ParserSuccess (f a)
  (ParserFailure e) <*> _ = ParserFailure e
  _ <*> (ParserFailure e) = ParserFailure e

newtype Parser a = Parser (ParserInput -> (ParserInput, ParserResult a)) deriving (Functor)

failingParser :: ParserError -> Parser a
failingParser err = Parser $ \i -> (i, ParserFailure err)

runParser :: Parser a -> T.Text -> (Location, T.Text, ParserResult a)
runParser (Parser run) i =
  let ((loc, leftOver), res) = run (id, i)
   in (loc zeroLocation, leftOver, res)

instance Applicative Parser where
  pure x = Parser $ \input -> (input, ParserSuccess x)
  Parser f <*> Parser fa = Parser $ \i ->
    let (i', r) = f i
        (i'', r') = fa i'
     in (i'', r <*> r')

instance Alternative Parser where
  empty = failingParser "Empty parser always fails"
  (Parser p1) <|> (Parser p2) = Parser $ \i ->
    let (i1, r1) = p1 i
        (i2, r2) = p2 i
     in case (r1, r2) of
          (ParserSuccess r, _) -> (i1, ParserSuccess r)
          (ParserFailure _, ParserSuccess r) -> (i2, ParserSuccess r)
          (ParserFailure e, ParserFailure _) -> (i1, ParserFailure e) -- TODO: in this case it is not clear which input to provide, but actually doesn't matter because it already failed

instance Monad Parser where
  p >>= f = flatten notFlattened
   where
    notFlattened = fmap f p
    flatten :: Parser (Parser b) -> Parser b
    flatten (Parser p1) = Parser $ \i -> case p1 i of
      (i', ParserSuccess (Parser p2)) -> p2 i'
      (i', ParserFailure e) -> (i', ParserFailure e)

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

-------------------------------- ORG MODE -------------------------------------

-- data Tree a = Node [Tree a] | Leaf a deriving (Show)

-- data TaskFile = TaskFile
--   { path :: String
--   , name :: String
--   , content :: Tree Task
--   }

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
splitToTitleAndTags input = (actualTitle, actualTags)
 where
  parts = split ':' input
  (titleParts, tagParts) = break isTag parts
  title = T.concat titleParts
  tags = filter (not . T.null) $ fmap stripLeadingColumn tagParts

  -- (actualTitle, actualTags) = case T.uncons $ reverse tagParts of
  --   Nothing -> (title, [])
  --   Just (x, _) -> if x == ":" then (title, fmap T.pack tags) else (input, [])
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

-- stripLeadingColumn [] = []
-- stripLeadingColumn (x : xs)
--   | x == ':' = xs
--   | otherwise = x : xs

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
  , created :: Maybe OrgTime -- NOTE: should not forget when decerealizing that it is a property
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

allTasksParser :: Parser Task
allTasksParser =
  properTaskParser
    <|> brokenDescriptionTaskParser
    <|> brokenPropertiesTaskParser

main :: IO ()
main = do
  content <- readFileExample "./resources/Sample.org"
  let (_, _, tasks) = runParser (many allTasksParser) content
  print (fmap head tasks)
