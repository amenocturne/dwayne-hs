{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.OrgMode where

import qualified Data.Text as T
import Data.Time
import Model.Injection

-- Model

data TimeUnit = Hour | Day | Week | Month | Year
  deriving (Show, Eq, Bounded, Enum)

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

data RepeatType
  = NextDate -- Example: "+1m" shifts by exactly one month after completion
  | NextFutureDate -- Example: "++1m" shift by at least one month (but may be several) till reaches the first future date
  | PlusCompletionDate -- Example: ".+1m" shifts by 1 month since completion day
  deriving (Show, Eq, Bounded, Enum)

allRepeatTypes :: [RepeatType]
allRepeatTypes = [minBound .. maxBound]

instance Injection RepeatType T.Text where
  to NextDate = "+"
  to NextFutureDate = "++"
  to PlusCompletionDate = "+."

instance Injection T.Text (Maybe RepeatType) where
  to "+" = Just NextDate
  to "++" = Just NextFutureDate
  to "+." = Just PlusCompletionDate
  to _ = Nothing

data DelayType
  = AllOccurrences -- Example: "-1d" notifies 1d before scheduled
  | FirstOccurrence -- Example: "--1d"
  deriving (Show, Eq, Bounded, Enum)

allDelayTypes :: [DelayType]
allDelayTypes = [minBound .. maxBound]

instance Injection DelayType T.Text where
  to AllOccurrences = "-"
  to FirstOccurrence = "--"

instance Injection T.Text (Maybe DelayType) where
  to "-" = Just AllOccurrences
  to "--" = Just FirstOccurrence
  to _ = Nothing

data RepeatInterval = RepeatInterval
  { repeatType :: RepeatType
  , repeatValue :: Int
  , repeatTimeUnit :: TimeUnit
  }
  deriving (Show)

data DelayInterval = DelayInterval
  { delayType :: DelayType
  , dealyValue :: Int
  , delayTimeUnit :: TimeUnit
  }
  deriving (Show)

data OrgTime = OrgTime
  { time :: Either Day LocalTime
  , repeater :: Maybe RepeatInterval
  , delay :: Maybe DelayInterval
  }
  deriving (Show)

data Task = Task
  { level :: Int
  , todoKeyword :: T.Text
  , priority :: Maybe Int
  , title :: T.Text
  , tags :: [T.Text]
  , scheduled :: Maybe OrgTime
  , deadline :: Maybe OrgTime
  , closed :: Maybe OrgTime
  , properties :: [(T.Text, T.Text)]
  , description :: T.Text
  }
  deriving (Show)

data TaskFile = TaskFile
  { name :: Maybe T.Text
  , content :: [Task] -- NOTE: maybe should change it to forest, but for now list is just fine
  }
  deriving (Show)

---------------------- CONSTANTS ------------------------------------

data Delimiter = AngleDelim | BracketDelim -- TODO: create injections for that as well

delims :: Delimiter -> (Char, Char)
delims AngleDelim = ('<', '>')
delims BracketDelim = ('[', ']')

data TimeField = TimeField
  { timeFieldName :: T.Text
  , timeFieldDelimiter :: Delimiter
  }

-- Time fields

orgScheduledField :: TimeField
orgScheduledField = TimeField "SCHEDULED" AngleDelim

orgDeadlineField :: TimeField
orgDeadlineField = TimeField "DEADLINE" AngleDelim

orgClosedField :: TimeField
orgClosedField = TimeField "CLOSED" BracketDelim

orgTimeFields :: [TimeField]
orgTimeFields = [orgScheduledField, orgDeadlineField]

-- Properties

orgPropertiesBegin :: T.Text
orgPropertiesBegin = ":PROPERTIES:"

orgPropertiesEnd :: T.Text
orgPropertiesEnd = ":END:"

-- Time Formats

orgDayFormat :: String
orgDayFormat = "%Y-%m-%d %a"

orgDayFormatCount :: Int -- TODO: should probably create data type for that
orgDayFormatCount = 4 + 1 + 2 + 1 + 2 + 1 + 3

orgDayTimeFormat :: String
orgDayTimeFormat = "%Y-%m-%d %a %H:%M"

orgTimeFormat :: String
orgTimeFormat = "%H:%M"

orgDayTimeFormatCount :: Int -- TODO: should probably create data type for that
orgDayTimeFormatCount = 4 + 1 + 2 + 1 + 2 + 1 + 3 + 1 + 2 + 1 + 2
