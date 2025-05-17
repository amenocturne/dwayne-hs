{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.OrgMode where

import Control.Lens
import qualified Data.Text as T
import Data.Time
import Model.Injection
import Data.Vector as V

-- Model

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
  {_level :: Int
  ,_todoKeyword :: T.Text
  ,_priority :: Maybe Int
  ,_title :: T.Text
  ,_tags :: [T.Text]
  ,_scheduled :: Maybe OrgTime
  ,_deadline :: Maybe OrgTime
  ,_closed :: Maybe OrgTime
  ,_properties :: [(T.Text, T.Text)]
  ,_description :: T.Text
  }
  deriving (Show)

data TaskFile a = TaskFile
  { _name :: Maybe T.Text
  , _content :: V.Vector a
  }
  deriving (Show)

data Delimiter = AngleDelim | BracketDelim

makeLenses ''TaskFile

---------------------- CONSTANTS ------------------------------------

allDelayTypes :: [DelayType]
allDelayTypes = [minBound .. maxBound]

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
  { timeFieldName :: T.Text
  , timeFieldDelimiter :: Delimiter
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

allRepeatTypes :: [RepeatType]
allRepeatTypes = [minBound .. maxBound]

instance Injection RepeatType T.Text where
  to NextDate = "+"
  to NextFutureDate = "++"
  to PlusCompletionDate = ".+"

instance Injection T.Text (Maybe RepeatType) where
  to "+" = Just NextDate
  to "++" = Just NextFutureDate
  to ".+" = Just PlusCompletionDate
  to _ = Nothing

-- Time fields

orgScheduledField :: TimeField
orgScheduledField = TimeField "SCHEDULED" AngleDelim

orgDeadlineField :: TimeField
orgDeadlineField = TimeField "DEADLINE" AngleDelim

orgClosedField :: TimeField
orgClosedField = TimeField "CLOSED" BracketDelim

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
