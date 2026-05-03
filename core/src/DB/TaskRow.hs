{-# LANGUAGE OverloadedStrings #-}

module DB.TaskRow
  ( DBTask (..),
    serializeTags,
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
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToRow (ToRow (..))
import Database.SQLite.Simple.Types ((:.) (..))
import Model.OrgMode (OrgTime (..), RichText (..), Task (..), plainToRichText)

newtype DBTask = DBTask {unDBTask :: Task}

instance ToRow DBTask where
  toRow (DBTask task) =
    toRow
      ( ( _level task,
          _todoKeyword task,
          _priority task,
          serializeRichText (_title task),
          serializeTags (_tags task)
        )
          :. ( fmap serializeOrgTime (_scheduled task),
               fmap serializeOrgTime (_deadline task),
               fmap serializeOrgTime (_createdProp task),
               fmap serializeOrgTime (_closed task),
               serializeProperties (_properties task),
               serializeRichText (_description task)
             )
      )

instance FromRow DBTask where
  fromRow = do
    lvl <- field
    kw <- field
    pri <- field
    ttl <- field
    tgs <- field
    sched <- field
    dl <- field
    crt <- field
    cls <- field
    props <- field
    desc <- field
    pure $
      DBTask
        Task
          { _level = lvl,
            _todoKeyword = kw,
            _priority = pri,
            _title = deserializeRichText ttl,
            _tags = deserializeTags tgs,
            _scheduled = sched >>= deserializeOrgTime,
            _deadline = dl >>= deserializeOrgTime,
            _createdProp = crt >>= deserializeOrgTime,
            _closed = cls >>= deserializeOrgTime,
            _properties = deserializeProperties props,
            _description = deserializeRichText desc
          }

-- Serialization helpers

serializeRichText :: RichText -> T.Text
serializeRichText = decodeUtf8Lazy . encode . toJSON

deserializeRichText :: T.Text -> RichText
deserializeRichText t =
  case eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right rt -> rt
    Left _ -> plainToRichText t

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
