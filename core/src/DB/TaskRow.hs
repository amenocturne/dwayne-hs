{-# LANGUAGE OverloadedStrings #-}

-- | Serialization helpers for task field values that flow through SQLite
-- storage. The legacy 'DBTask' newtype (with 'ToRow'/'FromRow' instances
-- against the dropped @tasks@ table) was removed in Phase 3; the events
-- log carries field values via its own JSON-shaped columns and consumes
-- the helpers in 'Events.Types'. The shared serializers stay here so
-- Phase-3 tests and any future row-based readers continue to round-trip
-- the same on-disk encoding.
module DB.TaskRow
  ( serializeTags,
    serializeProperties,
    serializeOrgTime,
    deserializeTags,
    deserializeProperties,
    deserializeOrgTime,
  )
where

import Data.Aeson (eitherDecode, encode, toJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, LocalTime, defaultTimeLocale, formatTime, parseTimeM)
import Model.OrgMode (OrgTime (..))

-- Serialization helpers

serializeTags :: S.Set T.Text -> T.Text
serializeTags = decodeUtf8Lazy . encode . toJSON . S.toList

serializeProperties :: [(T.Text, T.Text)] -> T.Text
serializeProperties = decodeUtf8Lazy . encode . toJSON

serializeOrgTime :: OrgTime -> T.Text
serializeOrgTime (OrgTime timeVal _ _) = case timeVal of
  Left day -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" day)
  Right lt -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lt)

-- Deserialization helpers

deserializeTags :: T.Text -> S.Set T.Text
deserializeTags t =
  case eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right tags -> S.fromList tags
    Left _ -> S.empty

deserializeProperties :: T.Text -> [(T.Text, T.Text)]
deserializeProperties t =
  case eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right pairs -> map tuplify pairs
    Left _ -> []
  where
    tuplify :: [T.Text] -> (T.Text, T.Text)
    tuplify [k, v] = (k, v)
    tuplify _ = ("", "")

deserializeOrgTime :: T.Text -> Maybe OrgTime
deserializeOrgTime t =
  case parseDateTime t of
    Just lt -> Just (OrgTime (Right lt) Nothing Nothing)
    Nothing -> case parseDate t of
      Just day -> Just (OrgTime (Left day) Nothing Nothing)
      Nothing -> Nothing
  where
    parseDateTime :: T.Text -> Maybe LocalTime
    parseDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" . T.unpack

    parseDate :: T.Text -> Maybe Day
    parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

-- Internal helper

decodeUtf8Lazy :: BL.ByteString -> T.Text
decodeUtf8Lazy = TE.decodeUtf8 . BL.toStrict
