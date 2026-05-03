{-# LANGUAGE OverloadedStrings #-}

-- | Event-sourced model for tasks.
--
-- An 'Event' is a partial update to a task, identified by (file_path, task_index).
-- Each field is wrapped in 'Maybe' where 'Nothing' means "this event did not
-- touch this field". A 'Just' carrying 'Nullable.nullValue' means
-- "this event cleared the field".
--
-- Genesis events have all fields populated (Just _). Subsequent delta events
-- have only the changed fields.
module Events.Types
  ( Event (..),
    emptyEvent,
    genesisEvent,
    isoFormat,
    parseIso,
  )
where

import Core.Nullable (Nullable (..))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value,
    object,
    toJSON,
    withObject,
    (.:?),
    (.=),
  )
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Model.OrgMode (OrgTime, RichText, Task (..))

-- | A partial task update. Field encoding:
--
-- @Nothing@      — event did not touch this field (SQL NULL)
-- @Just x@       — event set the field to x (or, if x is the type's
--                  Nullable.nullValue sentinel, cleared the field)
data Event = Event
  { evFilePath :: FilePath,
    evTaskIndex :: Int,
    evOccurredAt :: UTCTime,
    evLevel :: Maybe Int,
    evTodoKeyword :: Maybe T.Text,
    evPriority :: Maybe Int,
    evTitle :: Maybe RichText,
    evTags :: Maybe [T.Text],
    evScheduled :: Maybe OrgTime,
    evDeadline :: Maybe OrgTime,
    evCreated :: Maybe OrgTime,
    evClosed :: Maybe OrgTime,
    evProperties :: Maybe [(T.Text, T.Text)],
    evDescription :: Maybe RichText
  }
  deriving (Show, Eq)

-- | A no-op event template carrying just the row identity.
emptyEvent :: FilePath -> Int -> UTCTime -> Event
emptyEvent fp idx t =
  Event
    { evFilePath = fp,
      evTaskIndex = idx,
      evOccurredAt = t,
      evLevel = Nothing,
      evTodoKeyword = Nothing,
      evPriority = Nothing,
      evTitle = Nothing,
      evTags = Nothing,
      evScheduled = Nothing,
      evDeadline = Nothing,
      evCreated = Nothing,
      evClosed = Nothing,
      evProperties = Nothing,
      evDescription = Nothing
    }

-- | Build a genesis event from a complete Task. Every field becomes 'Just',
-- using each type's Nullable sentinel for fields the Task left unset.
genesisEvent :: FilePath -> Int -> UTCTime -> Task -> Event
genesisEvent fp idx t task =
  Event
    { evFilePath = fp,
      evTaskIndex = idx,
      evOccurredAt = t,
      evLevel = Just (_level task),
      evTodoKeyword = Just (_todoKeyword task),
      evPriority = Just (maybe (nullValue :: Int) id (_priority task)),
      evTitle = Just (_title task),
      evTags = Just (S.toList (_tags task)),
      evScheduled = Just (orgOrNull (_scheduled task)),
      evDeadline = Just (orgOrNull (_deadline task)),
      evCreated = Just (orgOrNull (_createdProp task)),
      evClosed = Just (orgOrNull (_closed task)),
      evProperties = Just (_properties task),
      evDescription = Just (_description task)
    }
  where
    orgOrNull :: Maybe OrgTime -> OrgTime
    orgOrNull (Just o) = o
    orgOrNull Nothing = nullValue

-- | Millisecond-precision ISO8601. Subsecond precision matters because two
-- mutations within the same wall-clock second collide on the
-- (file_path, task_index, occurred_at) primary key and the second is silently
-- dropped by INSERT OR IGNORE. Mobile uses the same format (".SSS'Z'").
isoFormat :: UTCTime -> T.Text
isoFormat t = T.pack (noFrac ++ "." ++ ms ++ "Z")
  where
    full = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" t
    (noFrac, ms) = case break (== '.') full of
      (datePart, '.' : rest) -> (datePart, take 3 (rest ++ "000"))
      _ -> (full, "000")


parseIso :: T.Text -> Maybe UTCTime
parseIso t = firstJust attempts
  where
    s = T.unpack t
    attempts =
      [ parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" s,
        parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" s,
        parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" s,
        parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" s,
        parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s
      ]
    firstJust :: [Maybe a] -> Maybe a
    firstJust [] = Nothing
    firstJust (Just x : _) = Just x
    firstJust (Nothing : xs) = firstJust xs

-- | Serialize properties as JSON array of [key, value] pairs to match the
-- existing wire convention used in DB.TaskRow.
propsToJSON :: [(T.Text, T.Text)] -> Value
propsToJSON = toJSON . map (\(k, v) -> [k, v])

propsFromJSON :: Maybe [[T.Text]] -> Maybe [(T.Text, T.Text)]
propsFromJSON = fmap (map toPair)
  where
    toPair [k, v] = (k, v)
    toPair _ = ("", "")

instance ToJSON Event where
  toJSON e =
    object
      [ "file" .= evFilePath e,
        "taskIndex" .= evTaskIndex e,
        "occurredAt" .= isoFormat (evOccurredAt e),
        "level" .= evLevel e,
        "todoKeyword" .= evTodoKeyword e,
        "priority" .= evPriority e,
        "title" .= evTitle e,
        "tags" .= evTags e,
        "scheduled" .= evScheduled e,
        "deadline" .= evDeadline e,
        "created" .= evCreated e,
        "closed" .= evClosed e,
        "properties" .= fmap propsToJSON (evProperties e),
        "description" .= evDescription e
      ]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \v -> do
    fp <- reqField v "file"
    idx <- reqField v "taskIndex"
    occ <- reqField v "occurredAt"
    occT <- case parseIso occ of
      Just u -> pure u
      Nothing -> fail ("Event.occurredAt: cannot parse " <> T.unpack occ)
    Event fp idx occT
      <$> v .:? "level"
      <*> v .:? "todoKeyword"
      <*> v .:? "priority"
      <*> v .:? "title"
      <*> v .:? "tags"
      <*> v .:? "scheduled"
      <*> v .:? "deadline"
      <*> v .:? "created"
      <*> v .:? "closed"
      <*> (propsFromJSON <$> v .:? "properties")
      <*> v .:? "description"
    where
      reqField o k = do
        m <- o .:? k
        case m of
          Just x -> pure x
          Nothing -> fail ("Event: missing field " <> show k)
