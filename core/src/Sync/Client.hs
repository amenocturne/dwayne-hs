{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Event-sourced sync daemon. Polls the remote server for new events and
-- pushes locally-emitted events back. There is no per-row conflict resolution:
-- the events log is append-only, and each (file_path, task_index, occurred_at)
-- is unique. Two clients that emit different deltas at the same moment simply
-- both make it into the log, and the projection takes "latest-Just-wins" per
-- field.
--
-- Local sync_state keys:
--   * last_pulled_at — server's clock at the last successful pull
--   * last_pushed_at — max occurred_at of the last successful push batch
--   * owned_files    — JSON array of file paths this client is allowed to push
module Sync.Client
  ( runSyncDaemon,
    runSyncOnce,
    SyncOptions (..),
    defaultSyncOptions,
  )
where

import Api.EventHandlers (EventsResponse (..), PostEventsRequest (..), PostEventsResponse (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import DB.Connection (initDatabase, withDatabase)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    execute,
    query,
    withTransaction,
  )
import Events.Store (insertEvents, selectEventsForFilesSince)
import Events.Types (Event (..), isoFormat, parseIso)
import Network.HTTP.Client
  ( Manager,
    RequestBody (RequestBodyLBS),
    Response,
    applyBasicAuth,
    httpLbs,
    method,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
    setQueryString,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Sync.Config (SyncCredentials (..))
import qualified System.IO as IO

data SyncOptions = SyncOptions
  { -- | How far back, in hours, to ask the server to replay events on each
    -- pull. Buys robustness against clock skew at the cost of redundant rows
    -- (which 'INSERT OR IGNORE' suppresses).
    soPullWindowHours :: Int,
    -- | Default: every file in the config is owned. Override via
    -- sync_state.owned_files for finer-grained control.
    soDefaultOwnedFiles :: [FilePath]
  }

defaultSyncOptions :: SyncOptions
defaultSyncOptions =
  SyncOptions
    { soPullWindowHours = 24,
      soDefaultOwnedFiles = []
    }

runSyncDaemon :: FilePath -> SyncCredentials -> Int -> SyncOptions -> IO ()
runSyncDaemon dbPath creds intervalSec opts = do
  initDatabase dbPath
  manager <- newTlsManager
  putStrLn $
    "dwayne sync (events): polling "
      ++ T.unpack (scServerUrl creds)
      ++ " every "
      ++ show intervalSec
      ++ "s (pull window: "
      ++ show (soPullWindowHours opts)
      ++ "h)"
  loop manager
  where
    loop manager = do
      result <- try @SomeException (oneCycle manager dbPath creds opts)
      case result of
        Right () -> threadDelay (intervalSec * 1_000_000)
        Left e -> do
          IO.hPutStrLn IO.stderr $ "sync: cycle failed: " ++ show e
          threadDelay (errorBackoffSec intervalSec * 1_000_000)
      loop manager

runSyncOnce :: FilePath -> SyncCredentials -> SyncOptions -> IO ()
runSyncOnce dbPath creds opts = do
  initDatabase dbPath
  manager <- newTlsManager
  putStrLn $
    "dwayne sync (events): force sync with "
      ++ T.unpack (scServerUrl creds)
      ++ " (pull window: "
      ++ show (soPullWindowHours opts)
      ++ "h)"
  oneCycle manager dbPath creds opts

errorBackoffSec :: Int -> Int
errorBackoffSec interval = max interval 60

oneCycle :: Manager -> FilePath -> SyncCredentials -> SyncOptions -> IO ()
oneCycle manager dbPath creds opts = do
  pullCycle manager dbPath creds opts
  pushCycle manager dbPath creds opts

-- ---------------------------------------------------------------------------
-- PULL
-- ---------------------------------------------------------------------------

pullCycle :: Manager -> FilePath -> SyncCredentials -> SyncOptions -> IO ()
pullCycle manager dbPath creds opts = do
  mLastPulled <- withDatabase dbPath getLastPulledAt
  let backwindow = secondsToNominalDiffTime (fromIntegral (soPullWindowHours opts) * (-3600))
      sinceParam = case mLastPulled of
        Just t -> Just (isoFormat (addUTCTime backwindow t))
        Nothing -> Nothing
  let url = T.unpack (scServerUrl creds) <> "/api/events"
  baseReq <- parseRequest url
  let req0 =
        applyBasicAuth
          (TE.encodeUtf8 (scUsername creds))
          (TE.encodeUtf8 (scPassword creds))
          baseReq
      req = case sinceParam of
        Just since ->
          setQueryString [("since", Just (TE.encodeUtf8 since))] req0
        Nothing -> req0
  resp <- httpLbs req manager
  ensure2xx resp
  case Aeson.eitherDecode (responseBody resp) :: Either String EventsResponse of
    Left err -> fail $ "pull: failed to decode: " ++ err
    Right er -> do
      withDatabase dbPath $ \conn -> do
        _ <- insertEvents conn (erEvents er)
        setLastPulledAt conn (erServerNow er)
      let n = length (erEvents er)
      when (n > 0) $
        putStrLn $
          "sync pull: applied " ++ show n ++ " events"

-- ---------------------------------------------------------------------------
-- PUSH
-- ---------------------------------------------------------------------------

pushCycle :: Manager -> FilePath -> SyncCredentials -> SyncOptions -> IO ()
pushCycle manager dbPath creds opts = do
  ownedFiles <- withDatabase dbPath $ \conn -> do
    fromState <- getOwnedFiles conn
    pure $ case fromState of
      Just xs | not (null xs) -> S.fromList xs
      _ -> S.fromList (soDefaultOwnedFiles opts)
  if S.null ownedFiles
    then pure ()
    else do
      lastPushed <- withDatabase dbPath getLastPushedAt
      let pushSince = case lastPushed of
            Just t -> t
            Nothing -> epoch
      events <- withDatabase dbPath $ \conn ->
        selectEventsForFilesSince conn ownedFiles pushSince
      if null events
        then pure ()
        else do
          let payload = PostEventsRequest events
              url = T.unpack (scServerUrl creds) <> "/api/events"
          baseReq <- parseRequest url
          let authedReq =
                applyBasicAuth
                  (TE.encodeUtf8 (scUsername creds))
                  (TE.encodeUtf8 (scPassword creds))
                  baseReq
              req =
                authedReq
                  { method = "POST",
                    requestBody = RequestBodyLBS (Aeson.encode payload),
                    requestHeaders =
                      ("Content-Type", "application/json")
                        : requestHeaders authedReq
                  }
          resp <- httpLbs req manager
          ensure2xx resp
          case Aeson.eitherDecode (responseBody resp) :: Either String PostEventsResponse of
            Left err -> fail $ "push: failed to decode: " ++ err
            Right per -> do
              let maxOcc = maximumOccurredAt events
              withDatabase dbPath $ \conn -> setLastPushedAt conn maxOcc
              putStrLn $
                "sync push: server accepted " ++ show (perAccepted per) ++ " events"

-- ---------------------------------------------------------------------------
-- sync_state helpers
-- ---------------------------------------------------------------------------

getLastPulledAt :: Connection -> IO (Maybe UTCTime)
getLastPulledAt conn = readUTCKey conn "last_pulled_at"

setLastPulledAt :: Connection -> T.Text -> IO ()
setLastPulledAt conn t = writeKey conn "last_pulled_at" t

getLastPushedAt :: Connection -> IO (Maybe UTCTime)
getLastPushedAt conn = readUTCKey conn "last_pushed_at"

setLastPushedAt :: Connection -> UTCTime -> IO ()
setLastPushedAt conn t = writeKey conn "last_pushed_at" (isoFormat t)

getOwnedFiles :: Connection -> IO (Maybe [FilePath])
getOwnedFiles conn = do
  rows <-
    query
      conn
      "SELECT value FROM sync_state WHERE key = ?"
      (Only ("owned_files" :: T.Text)) ::
      IO [Only T.Text]
  case rows of
    [] -> pure Nothing
    (Only v : _) -> pure $ Aeson.decode (BL.fromStrict (TE.encodeUtf8 v))

readUTCKey :: Connection -> T.Text -> IO (Maybe UTCTime)
readUTCKey conn k = do
  rows <-
    query
      conn
      "SELECT value FROM sync_state WHERE key = ?"
      (Only k) ::
      IO [Only T.Text]
  case rows of
    [] -> pure Nothing
    (Only v : _) -> pure (parseIso v)

writeKey :: Connection -> T.Text -> T.Text -> IO ()
writeKey conn k v =
  execute
    conn
    "INSERT INTO sync_state (key, value) VALUES (?, ?) \
    \ON CONFLICT(key) DO UPDATE SET value = excluded.value"
    (k, v)

-- ---------------------------------------------------------------------------
-- HTTP helpers
-- ---------------------------------------------------------------------------

ensure2xx :: Response BL.ByteString -> IO ()
ensure2xx resp = do
  let code = statusCode (responseStatus resp)
  if code >= 200 && code < 300
    then pure ()
    else
      fail $
        "sync: HTTP "
          ++ show code
          ++ ": "
          ++ BS.unpack (BL.toStrict (responseBody resp))

epoch :: UTCTime
epoch = read "1970-01-01 00:00:00 UTC"

maximumOccurredAt :: [Event] -> UTCTime
maximumOccurredAt [] = epoch
maximumOccurredAt es = maximum (map evOccurredAt es)
