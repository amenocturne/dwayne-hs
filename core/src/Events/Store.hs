{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | SQLite read/write for the events table.
--
-- Schema is defined in DB.Schema (v3 migration). Each row is one Event.
-- INSERT OR IGNORE is used so the same (file_path, task_index, occurred_at)
-- triple cannot be inserted twice — this gives us idempotent push/pull.
module Events.Store
  ( insertEvent,
    insertEvents,
    selectAllEvents,
    selectEventsSince,
    selectEventsForFiles,
    selectEventsForFilesSince,
    selectMaxIndexForFile,
    nextTaskIndex,
    eventsByOccurredAtRange,
  )
where

import Control.Monad (forM_)
import Core.Nullable (Nullable (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Database.SQLite.Simple
import Events.Types (Event (..))
import qualified Events.Types
import Model.OrgMode (OrgTime, RichText)

-- ---------------------------------------------------------------------------
-- Serialization helpers (mirror DB.TaskRow conventions)
-- ---------------------------------------------------------------------------

encodeRichText :: RichText -> T.Text
encodeRichText = TE.decodeUtf8 . BL.toStrict . Aeson.encode

decodeRichText :: T.Text -> RichText
decodeRichText t =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right rt -> rt
    Left _ -> nullValue

encodeTags :: [T.Text] -> T.Text
encodeTags = TE.decodeUtf8 . BL.toStrict . Aeson.encode

decodeTags :: T.Text -> [T.Text]
decodeTags t =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right xs -> xs
    Left _ -> []

encodeProps :: [(T.Text, T.Text)] -> T.Text
encodeProps = TE.decodeUtf8 . BL.toStrict . Aeson.encode . map (\(k, v) -> [k, v])

decodeProps :: T.Text -> [(T.Text, T.Text)]
decodeProps t =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right pairs -> map toTuple pairs
    Left _ -> []
  where
    toTuple [k, v] = (k, v)
    toTuple _ = ("", "")

encodeOrgTime :: OrgTime -> T.Text
encodeOrgTime =
  TE.decodeUtf8 . BL.toStrict . Aeson.encode

decodeOrgTime :: T.Text -> OrgTime
decodeOrgTime t =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right o -> o
    Left _ -> (nullValue :: OrgTime)

encodeUTC :: UTCTime -> T.Text
encodeUTC = Events.Types.isoFormat

decodeUTC :: T.Text -> UTCTime
decodeUTC t = case Events.Types.parseIso t of
  Just u -> u
  Nothing -> error ("decodeUTC: cannot parse " <> T.unpack t)

-- ---------------------------------------------------------------------------
-- Row marshalling
-- ---------------------------------------------------------------------------

-- | Convert an Event to the row tuple. Each Maybe field becomes a Maybe T.Text
-- (or Maybe Int) for SQL.
type EventRow =
  ( T.Text, -- file_path
    Int, -- task_index
    T.Text -- occurred_at
  )
    :. ( Maybe Int, -- level
         Maybe T.Text, -- todo_keyword
         Maybe Int, -- priority
         Maybe T.Text, -- title (json richtext)
         Maybe T.Text, -- tags (json array)
         Maybe T.Text, -- scheduled (json orgtime)
         Maybe T.Text, -- deadline
         Maybe T.Text -- created
       )
    :. ( Maybe T.Text, -- closed
         Maybe T.Text, -- properties (json)
         Maybe T.Text -- description (json richtext)
       )

eventToRow :: Event -> EventRow
eventToRow e =
  ( T.pack (evFilePath e),
    evTaskIndex e,
    encodeUTC (evOccurredAt e)
  )
    :. ( evLevel e,
         evTodoKeyword e,
         evPriority e,
         fmap encodeRichText (evTitle e),
         fmap encodeTags (evTags e),
         fmap encodeOrgTime (evScheduled e),
         fmap encodeOrgTime (evDeadline e),
         fmap encodeOrgTime (evCreated e)
       )
    :. ( fmap encodeOrgTime (evClosed e),
         fmap encodeProps (evProperties e),
         fmap encodeRichText (evDescription e)
       )

rowToEvent :: EventRow -> Event
rowToEvent ((fp, idx, occ) :. (lvl, kw, pri, ttl, tgs, sch, dl, crt) :. (cls, props, desc)) =
  Event
    { evFilePath = T.unpack fp,
      evTaskIndex = idx,
      evOccurredAt = decodeUTC occ,
      evLevel = lvl,
      evTodoKeyword = kw,
      evPriority = pri,
      evTitle = fmap decodeRichText ttl,
      evTags = fmap decodeTags tgs,
      evScheduled = fmap decodeOrgTime sch,
      evDeadline = fmap decodeOrgTime dl,
      evCreated = fmap decodeOrgTime crt,
      evClosed = fmap decodeOrgTime cls,
      evProperties = fmap decodeProps props,
      evDescription = fmap decodeRichText desc
    }

eventColumns :: Query
eventColumns =
  "file_path, task_index, occurred_at, \
  \level, todo_keyword, priority, title, tags, \
  \scheduled, deadline, created, \
  \closed, properties, description"

placeholders :: Query
placeholders =
  "?, ?, ?, \
  \?, ?, ?, ?, ?, \
  \?, ?, ?, \
  \?, ?, ?"

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

insertEvent :: Connection -> Event -> IO ()
insertEvent conn e =
  execute
    conn
    ("INSERT OR IGNORE INTO events (" <> eventColumns <> ") VALUES (" <> placeholders <> ")")
    (eventToRow e)

insertEvents :: Connection -> [Event] -> IO Int
insertEvents conn events =
  withTransaction conn $ do
    forM_ events (insertEvent conn)
    pure (length events)

selectAllEvents :: Connection -> IO [Event]
selectAllEvents conn = do
  rows <- query_ conn ("SELECT " <> eventColumns <> " FROM events ORDER BY occurred_at") :: IO [EventRow]
  pure (map rowToEvent rows)

-- | Select events strictly after the given timestamp.
selectEventsSince :: Connection -> UTCTime -> IO [Event]
selectEventsSince conn since = do
  rows <-
    query
      conn
      ("SELECT " <> eventColumns <> " FROM events WHERE occurred_at > ? ORDER BY occurred_at")
      (Only (encodeUTC since)) ::
      IO [EventRow]
  pure (map rowToEvent rows)

-- | Restrict to a set of file paths. If the set is empty, returns nothing.
selectEventsForFiles :: Connection -> S.Set FilePath -> IO [Event]
selectEventsForFiles conn fps
  | S.null fps = pure []
  | otherwise = do
      let placeholdersList = T.intercalate "," (map (const "?") (S.toList fps))
          q =
            Query $
              "SELECT "
                <> renderQuery eventColumns
                <> " FROM events WHERE file_path IN ("
                <> placeholdersList
                <> ") ORDER BY occurred_at"
          params = map T.pack (S.toList fps)
      rows <- query conn q params :: IO [EventRow]
      pure (map rowToEvent rows)

-- | Restrict to file paths AND occurred_at > since.
selectEventsForFilesSince ::
  Connection -> S.Set FilePath -> UTCTime -> IO [Event]
selectEventsForFilesSince conn fps since
  | S.null fps = pure []
  | otherwise = do
      let placeholdersList = T.intercalate "," (map (const "?") (S.toList fps))
          q =
            Query $
              "SELECT "
                <> renderQuery eventColumns
                <> " FROM events WHERE occurred_at > ? AND file_path IN ("
                <> placeholdersList
                <> ") ORDER BY occurred_at"
          params = encodeUTC since : map T.pack (S.toList fps)
      rows <- query conn q params :: IO [EventRow]
      pure (map rowToEvent rows)

-- | Highest task_index recorded for a file in the events table, or -1 if none.
selectMaxIndexForFile :: Connection -> FilePath -> IO Int
selectMaxIndexForFile conn fp = do
  rows <-
    query
      conn
      "SELECT COALESCE(MAX(task_index), -1) FROM events WHERE file_path = ?"
      (Only (T.pack fp)) ::
      IO [Only Int]
  case rows of
    (Only n : _) -> pure n
    [] -> pure (-1)

-- | Next free task_index for a file (max + 1, or 0).
nextTaskIndex :: Connection -> FilePath -> IO Int
nextTaskIndex conn fp = do
  n <- selectMaxIndexForFile conn fp
  pure (n + 1)

-- | Convenience: select events whose occurred_at falls in [lo, hi].
eventsByOccurredAtRange :: Connection -> UTCTime -> UTCTime -> IO [Event]
eventsByOccurredAtRange conn lo hi = do
  rows <-
    query
      conn
      ("SELECT " <> eventColumns <> " FROM events WHERE occurred_at >= ? AND occurred_at <= ? ORDER BY occurred_at")
      (encodeUTC lo, encodeUTC hi) ::
      IO [EventRow]
  pure (map rowToEvent rows)

-- | Render a Query back to Text so we can splice it into a string-built query.
renderQuery :: Query -> T.Text
renderQuery (Query t) = t
