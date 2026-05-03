{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server-side handlers for the event-sourced sync API.
--
-- GET  /api/events?since=<iso8601>  → { events, serverNow }
-- POST /api/events { events }       → { accepted, serverNow }
--
-- The server is just a transport for the events log. Business semantics
-- (last-event-wins per field) live in the projection, not here.
module Api.EventHandlers
  ( EventsResponse (..),
    PostEventsRequest (..),
    PostEventsResponse (..),
    getEventsHandler,
    postEventsHandler,
    nowUTC,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Lens (set, view)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import DB.Connection (withDatabase)
import Events.Projection (fileStateFromEvents)
import Events.Store (insertEvents, selectAllEvents, selectEventsSince)
import Events.Types (Event, isoFormat, parseIso)
import Model.OrgMode (Task)
import Servant (Handler)
import Tui.Types (AppContext, config, fileStateLens)
import qualified Tui.Types as TT

data EventsResponse = EventsResponse
  { erEvents :: [Event],
    erServerNow :: T.Text
  }
  deriving (Eq, Show)

instance ToJSON EventsResponse where
  toJSON (EventsResponse evs now) =
    object ["events" .= evs, "serverNow" .= now]

instance FromJSON EventsResponse where
  parseJSON = withObject "EventsResponse" $ \v ->
    EventsResponse <$> v .: "events" <*> v .: "serverNow"

newtype PostEventsRequest = PostEventsRequest
  { perEvents :: [Event]
  }
  deriving (Eq, Show)

instance ToJSON PostEventsRequest where
  toJSON (PostEventsRequest es) = object ["events" .= es]

instance FromJSON PostEventsRequest where
  parseJSON = withObject "PostEventsRequest" $ \v ->
    PostEventsRequest <$> v .: "events"

data PostEventsResponse = PostEventsResponse
  { perAccepted :: Int,
    perServerNow :: T.Text
  }
  deriving (Eq, Show)

instance ToJSON PostEventsResponse where
  toJSON (PostEventsResponse n now) =
    object ["accepted" .= n, "serverNow" .= now]

instance FromJSON PostEventsResponse where
  parseJSON = withObject "PostEventsResponse" $ \v ->
    PostEventsResponse <$> v .: "accepted" <*> v .: "serverNow"

nowUTC :: IO UTCTime
nowUTC = getCurrentTime

getEventsHandler ::
  MVar.MVar (AppContext Task) ->
  Maybe T.Text ->
  Handler EventsResponse
getEventsHandler cacheVar mSince = do
  ctx <- liftIO $ MVar.readMVar cacheVar
  let dbFile = TT._database (view config ctx)
  liftIO $ withDatabase dbFile $ \conn -> do
    events <- case mSince >>= parseIsoNonEmpty of
      Just t -> selectEventsSince conn t
      Nothing -> selectAllEvents conn
    now <- nowUTC
    pure $ EventsResponse events (isoFormat now)
  where
    parseIsoNonEmpty t
      | T.null t = Nothing
      | otherwise = parseIso t

postEventsHandler ::
  MVar.MVar (AppContext Task) ->
  PostEventsRequest ->
  Handler PostEventsResponse
postEventsHandler cacheVar req = do
  liftIO $ MVar.modifyMVar cacheVar $ \ctx -> do
    let dbFile = TT._database (view config ctx)
    (n, allEvents) <- withDatabase dbFile $ \conn -> do
      n <- insertEvents conn (perEvents req)
      events <- selectAllEvents conn
      pure (n, events)
    let newCtx = set fileStateLens (fileStateFromEvents allEvents) ctx
    now <- nowUTC
    pure (newCtx, PostEventsResponse n (isoFormat now))
