{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Model.OrgMode where

import Control.Lens (makeLenses)
import qualified Data.Text as T
import Data.Time
import Data.Vector as V
import Model.Injection
import Data.List (sortBy)

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
  deriving (Show, Eq)

data DelayInterval = DelayInterval
  { delayType :: DelayType
  , dealyValue :: Int
  , delayTimeUnit :: TimeUnit
  }
  deriving (Show, Eq)

data OrgTime = OrgTime
  { time :: Either Day LocalTime
  , repeater :: Maybe RepeatInterval
  , delay :: Maybe DelayInterval
  }
  deriving (Show, Eq)

data Task = Task
  { _level :: Int
  , _todoKeyword :: T.Text
  , _priority :: Maybe Int
  , _title :: T.Text
  , _tags :: [T.Text]
  , _scheduled :: Maybe OrgTime
  , _deadline :: Maybe OrgTime
  , _closed :: Maybe OrgTime
  , _properties :: [(T.Text, T.Text)]
  , _description :: T.Text
  }
  deriving (Show, Eq)

data TaskFile a = TaskFile
  { _name :: Maybe T.Text
  , _content :: V.Vector a
  }
  deriving (Show, Eq)

data Delimiter = AngleDelim | BracketDelim

makeLenses ''TaskFile

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

sortInverseLength:: (Injection a T.Text) => [a] -> [a]
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

orgCreatedProperty:: T.Text
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

orgTodoKeyWords :: [T.Text]
orgTodoKeyWords =
  [ "INBOX"
  , "RELEVANT"
  , "SOMEDAY"
  , "NOTES"
  , "LIST"
  , "WAITING"
  , "PROJECTS"
  , "TODO"
  , "DONE"
  , "TRASH"
  , "" -- for tasks without todo keyword
  ]
