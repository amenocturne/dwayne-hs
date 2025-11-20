{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.OrgMode where

import Control.Lens (makeLenses)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
  )
import Data.List (sortBy)
import qualified Data.Set as S
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Time
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Vector as V
import GHC.Generics (Generic)
import Model.Injection

-- Model

data TextNode
  = PlainText T.Text
  | OrgLink
      { _linkUrl :: T.Text,
        _linkTitle :: Maybe T.Text
      }
  deriving (Show, Eq)

newtype RichText = RichText {_unRichText :: [TextNode]}
  deriving (Show, Eq)

data TimeUnit = Hour | Day | Week | Month | Year
  deriving (Show, Eq, Bounded, Enum)

data RepeatType
  = NextDate -- Example: "+1m" shifts by exactly one month after completion
  | NextFutureDate -- Example: "++1m" shift by at least one month (but may be several) till reaches the first future date
  | PlusCompletionDate -- Example: ".+1m" shifts by 1 month since completion day
  deriving (Show, Eq, Bounded, Enum)

data DelayType
  = AllOccurrences -- Example: "-1d" notifies 1d before scheduled
  | FirstOccurrence -- Example: "--1d"
  deriving (Show, Eq, Bounded, Enum)

data RepeatInterval = RepeatInterval
  { repeatType :: RepeatType,
    repeatValue :: Int,
    repeatTimeUnit :: TimeUnit
  }
  deriving (Show, Eq)

data DelayInterval = DelayInterval
  { delayType :: DelayType,
    dealyValue :: Int,
    delayTimeUnit :: TimeUnit
  }
  deriving (Show, Eq)

data OrgTime = OrgTime
  { time :: Either Day LocalTime,
    repeater :: Maybe RepeatInterval,
    delay :: Maybe DelayInterval
  }
  deriving (Show, Eq)

data Task = Task
  { _level :: Int,
    _todoKeyword :: T.Text,
    _priority :: Maybe Int,
    _title :: RichText,
    _tags :: S.Set T.Text,
    _scheduled :: Maybe OrgTime,
    _deadline :: Maybe OrgTime,
    _createdProp :: Maybe OrgTime,
    _closed :: Maybe OrgTime,
    _properties :: [(T.Text, T.Text)],
    _description :: RichText
  }
  deriving (Show, Eq)

data TaskFile a = TaskFile
  { _name :: Maybe T.Text,
    _content :: V.Vector a
  }
  deriving (Show, Eq)

data Delimiter = AngleDelim | BracketDelim

makeLenses ''TextNode
makeLenses ''RichText
makeLenses ''TaskFile

---------------------- RICH TEXT HELPERS ----------------------------

richTextToPlain :: RichText -> T.Text
richTextToPlain (RichText nodes) = T.concat $ Prelude.map nodeToText nodes
  where
    nodeToText (PlainText t) = t
    nodeToText (OrgLink url (Just title)) = title
    nodeToText (OrgLink url Nothing) = url

plainToRichText :: T.Text -> RichText
plainToRichText t = RichText [PlainText t]

extractUrls :: RichText -> [T.Text]
extractUrls (RichText nodes) = [url | OrgLink url _ <- nodes]

instance IsString RichText where
  fromString = plainToRichText . T.pack

---------------------- CONSTANTS ------------------------------------

instance Injection DelayType T.Text where
  to AllOccurrences = "-"
  to FirstOccurrence = "--"

instance Injection T.Text (Maybe DelayType) where
  to "-" = Just AllOccurrences
  to "--" = Just FirstOccurrence
  to _ = Nothing

instance Injection Delimiter (Char, Char) where
  to AngleDelim = ('<', '>')
  to BracketDelim = ('[', ']')

data TimeField = TimeField
  { timeFieldName :: T.Text,
    timeFieldDelimiter :: Delimiter
  }

makeLenses ''Task

allTimeUnits :: [TimeUnit]
allTimeUnits = [minBound .. maxBound]

instance Injection TimeUnit Char where
  to Hour = 'h'
  to Day = 'd'
  to Week = 'w'
  to Month = 'm'
  to Year = 'y'

instance Injection Char (Maybe TimeUnit) where
  to 'h' = Just Hour
  to 'd' = Just Day
  to 'w' = Just Week
  to 'm' = Just Month
  to 'y' = Just Year
  to _ = Nothing

instance Injection RepeatType T.Text where
  to NextDate = "+"
  to NextFutureDate = "++"
  to PlusCompletionDate = ".+"

instance Injection T.Text (Maybe RepeatType) where
  to "+" = Just NextDate
  to "++" = Just NextFutureDate
  to ".+" = Just PlusCompletionDate
  to _ = Nothing

-- Sorting specifies the order in which this types are parsed, so we should
-- firstly try -- and only then -
allDelayTypes :: [DelayType]
allDelayTypes = sortInverseLength [minBound .. maxBound]

-- Sorting specifies the order in which this types are parsed, so we should
-- firstly try ++ and only then +
allRepeatTypes :: [RepeatType]
allRepeatTypes = sortInverseLength [minBound .. maxBound]

sortInverseLength :: (Injection a T.Text) => [a] -> [a]
sortInverseLength = sortBy compareLength
  where
    compareLength :: (Injection a T.Text) => a -> a -> Ordering
    compareLength a b = compare (T.length (to b)) (T.length (to a))

-- Time fields

orgScheduledField :: TimeField
orgScheduledField = TimeField "SCHEDULED" AngleDelim

orgDeadlineField :: TimeField
orgDeadlineField = TimeField "DEADLINE" AngleDelim

orgClosedField :: TimeField
orgClosedField = TimeField "CLOSED" BracketDelim

orgCreatedProperty :: T.Text
orgCreatedProperty = "CREATED"

orgTimeFields :: [TimeField]
orgTimeFields = [orgScheduledField, orgDeadlineField, orgClosedField]

-- Properties

orgPropertiesBegin :: T.Text
orgPropertiesBegin = ":PROPERTIES:"

orgPropertiesEnd :: T.Text
orgPropertiesEnd = ":END:"

-- Time Formats

orgDayFormat :: String
orgDayFormat = "%Y-%m-%d %a"

orgDayTimeFormat :: String
orgDayTimeFormat = "%Y-%m-%d %a %H:%M"

orgTimeFormat :: String
orgTimeFormat = "%H:%M"

-- | Individual TODO keyword constants
orgInboxKeyword :: T.Text
orgInboxKeyword = "INBOX"

orgRelevantKeyword :: T.Text
orgRelevantKeyword = "RELEVANT"

orgSomedayKeyword :: T.Text
orgSomedayKeyword = "SOMEDAY"

orgNotesKeyword :: T.Text
orgNotesKeyword = "NOTES"

orgListKeyword :: T.Text
orgListKeyword = "LIST"

orgWaitingKeyword :: T.Text
orgWaitingKeyword = "WAITING"

orgProjectKeyword :: T.Text
orgProjectKeyword = "PROJECT"

orgTodoKeyword :: T.Text
orgTodoKeyword = "TODO"

orgDoneKeyword :: T.Text
orgDoneKeyword = "DONE"

orgTrashKeyword :: T.Text
orgTrashKeyword = "TRASH"

orgTodoKeyWords :: [T.Text]
orgTodoKeyWords =
  [ orgInboxKeyword,
    orgRelevantKeyword,
    orgSomedayKeyword,
    orgNotesKeyword,
    orgListKeyword,
    orgWaitingKeyword,
    orgProjectKeyword,
    orgTodoKeyword,
    orgDoneKeyword,
    orgTrashKeyword,
    "" -- for tasks without todo keyword
  ]

---------------------- JSON SERIALIZATION ----------------------------

instance ToJSON TextNode where
  toJSON (PlainText t) = object ["type" .= ("plain" :: T.Text), "text" .= t]
  toJSON (OrgLink url mTitle) =
    object
      [ "type" .= ("link" :: T.Text),
        "url" .= url,
        "title" .= mTitle
      ]

instance FromJSON TextNode where
  parseJSON = withObject "TextNode" $ \v -> do
    nodeType <- v .: "type"
    case nodeType :: T.Text of
      "plain" -> PlainText <$> v .: "text"
      "link" -> OrgLink <$> v .: "url" <*> v .:? "title"
      _ -> fail "Unknown TextNode type"

instance ToJSON RichText where
  toJSON (RichText nodes) = toJSON nodes

instance FromJSON RichText where
  parseJSON v = RichText <$> parseJSON v

instance ToJSON TimeUnit where
  toJSON Hour = String "hour"
  toJSON Day = String "day"
  toJSON Week = String "week"
  toJSON Month = String "month"
  toJSON Year = String "year"

instance FromJSON TimeUnit where
  parseJSON = withText "TimeUnit" $ \case
    "hour" -> pure Hour
    "day" -> pure Day
    "week" -> pure Week
    "month" -> pure Month
    "year" -> pure Year
    _ -> fail "Unknown TimeUnit"

instance ToJSON RepeatType where
  toJSON NextDate = String "nextDate"
  toJSON NextFutureDate = String "nextFutureDate"
  toJSON PlusCompletionDate = String "plusCompletionDate"

instance FromJSON RepeatType where
  parseJSON = withText "RepeatType" $ \case
    "nextDate" -> pure NextDate
    "nextFutureDate" -> pure NextFutureDate
    "plusCompletionDate" -> pure PlusCompletionDate
    _ -> fail "Unknown RepeatType"

instance ToJSON DelayType where
  toJSON AllOccurrences = String "allOccurrences"
  toJSON FirstOccurrence = String "firstOccurrence"

instance FromJSON DelayType where
  parseJSON = withText "DelayType" $ \case
    "allOccurrences" -> pure AllOccurrences
    "firstOccurrence" -> pure FirstOccurrence
    _ -> fail "Unknown DelayType"

instance ToJSON RepeatInterval where
  toJSON (RepeatInterval rType rValue rUnit) =
    object
      [ "type" .= rType,
        "value" .= rValue,
        "unit" .= rUnit
      ]

instance FromJSON RepeatInterval where
  parseJSON = withObject "RepeatInterval" $ \v ->
    RepeatInterval
      <$> v .: "type"
      <*> v .: "value"
      <*> v .: "unit"

instance ToJSON DelayInterval where
  toJSON (DelayInterval dType dValue dUnit) =
    object
      [ "type" .= dType,
        "value" .= dValue,
        "unit" .= dUnit
      ]

instance FromJSON DelayInterval where
  parseJSON = withObject "DelayInterval" $ \v ->
    DelayInterval
      <$> v .: "type"
      <*> v .: "value"
      <*> v .: "unit"

-- | Serialize OrgTime as an object with date/time components
-- Format: { "date": "2025-11-20", "time": "14:30" | null, "repeater": {...} | null, "delay": {...} | null }
instance ToJSON OrgTime where
  toJSON (OrgTime timeVal mRepeater mDelay) =
    case timeVal of
      Left day ->
        object
          [ "date" .= formatTime defaultTimeLocale "%Y-%m-%d" day,
            "time" .= Null,
            "repeater" .= mRepeater,
            "delay" .= mDelay
          ]
      Right localTime ->
        let day = localDay localTime
            tod = localTimeOfDay localTime
         in object
              [ "date" .= formatTime defaultTimeLocale "%Y-%m-%d" day,
                "time" .= formatTime defaultTimeLocale "%H:%M" tod,
                "repeater" .= mRepeater,
                "delay" .= mDelay
              ]

instance FromJSON OrgTime where
  parseJSON = withObject "OrgTime" $ \v -> do
    dateStr <- v .: "date"
    mTimeStr <- v .:? "time"
    mRepeater <- v .:? "repeater"
    mDelay <- v .:? "delay"

    day <- case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr of
      Just d -> pure d
      Nothing -> fail "Invalid date format"

    timeVal <- case mTimeStr of
      Nothing -> pure $ Left day
      Just timeStr -> do
        tod <- case parseTimeM True defaultTimeLocale "%H:%M" (T.unpack timeStr) of
          Just t -> pure t
          Nothing -> fail "Invalid time format"
        pure $ Right (LocalTime day tod)

    pure $ OrgTime timeVal mRepeater mDelay

-- | Serialize Task with field names matching TypeScript interface
instance ToJSON Task where
  toJSON task =
    object
      [ "level" .= _level task,
        "todoKeyword" .= _todoKeyword task,
        "priority" .= _priority task,
        "title" .= _title task,
        "tags" .= S.toList (_tags task),
        "scheduled" .= _scheduled task,
        "deadline" .= _deadline task,
        "createdProp" .= _createdProp task,
        "closed" .= _closed task,
        "properties" .= _properties task,
        "description" .= _description task
      ]

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v ->
    Task
      <$> v .: "level"
      <*> v .: "todoKeyword"
      <*> v .:? "priority"
      <*> v .: "title"
      <*> (S.fromList <$> v .: "tags")
      <*> v .:? "scheduled"
      <*> v .:? "deadline"
      <*> v .:? "createdProp"
      <*> v .:? "closed"
      <*> v .: "properties"
      <*> v .: "description"

instance ToJSON a => ToJSON (TaskFile a) where
  toJSON (TaskFile mName content) =
    object
      [ "name" .= mName,
        "content" .= V.toList content
      ]

instance FromJSON a => FromJSON (TaskFile a) where
  parseJSON = withObject "TaskFile" $ \v ->
    TaskFile
      <$> v .:? "name"
      <*> (V.fromList <$> v .: "content")
