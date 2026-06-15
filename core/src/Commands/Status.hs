{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commands.Status
  ( StatusArgsError (..),
    StatusTarget (..),
    healthUrl,
    statusUrl,
    parseStatusArgs,
    runStatusCommand,
  )
where

import qualified Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( HttpException (..),
    HttpExceptionContent (..),
    Manager,
    Request,
    applyBasicAuth,
    httpLbs,
    method,
    parseRequest,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Sync.Config (SyncCredentials (..), loadCredentials)
import Text.Read (readMaybe)

newtype StatusArgsError = StatusArgsError String
  deriving (Eq, Show)

data StatusTarget
  = LocalStatus Int
  | RemoteStatus
  | UrlStatus String
  deriving (Eq, Show)

statusUrl :: Int -> String
statusUrl port = "http://localhost:" ++ show port ++ "/api/health"

healthUrl :: String -> String
healthUrl baseUrl =
  stripTrailingSlash baseUrl ++ "/api/health"

parseStatusArgs :: [String] -> Either StatusArgsError StatusTarget
parseStatusArgs [] = Right (LocalStatus 8080)
parseStatusArgs ["--port", rawPort] = LocalStatus <$> parsePort rawPort
parseStatusArgs ["-p", rawPort] = LocalStatus <$> parsePort rawPort
parseStatusArgs ["--remote"] = Right RemoteStatus
parseStatusArgs ["--url", rawUrl] = Right (UrlStatus (healthUrl rawUrl))
parseStatusArgs _ =
  Left $
    StatusArgsError
      "Usage: dwayne status [--port PORT | --remote | --url URL]"

parsePort :: String -> Either StatusArgsError Int
parsePort rawPort =
  case readMaybe rawPort of
    Just port | port > 0 -> Right port
    _ -> Left $ StatusArgsError "Port must be a positive integer"

runStatusCommand :: [String] -> IO ()
runStatusCommand args =
  case parseStatusArgs args of
    Left (StatusArgsError msg) -> do
      hPutStrLn stderr msg
      exitFailure
    Right target -> do
      manager <- newTlsManager
      req <- statusRequest target
      result <- tryHttp (httpLbs req manager)
      case result of
        Right resp -> do
          let code = statusCode (responseStatus resp)
          if code >= 200 && code < 300
            then BSL.putStrLn (responseBody resp)
            else
              case target of
                RemoteStatus | code == 404 -> runRemoteEventsFallback manager
                _ -> do
                  hPutStrLn stderr $
                    "Status request failed: HTTP "
                      ++ show code
                      ++ ": "
                      ++ BSL.unpack (responseBody resp)
                  exitFailure
        Left (HttpExceptionRequest _ (ConnectionFailure _)) -> do
          putStrLn $ offlineMessage target
          exitFailure
        Left err -> do
          hPutStrLn stderr $ "Status request failed: " ++ show err
          exitFailure

statusRequest :: StatusTarget -> IO Request
statusRequest target =
  case target of
    LocalStatus port -> parseRequest (statusUrl port)
    UrlStatus url -> parseRequest url
    RemoteStatus -> do
      creds <- loadCredentials
      authedRequest creds (healthUrl (T.unpack (scServerUrl creds)))

runRemoteEventsFallback :: Manager -> IO ()
runRemoteEventsFallback manager = do
  creds <- loadCredentials
  req <- authedRequest creds (stripTrailingSlash (T.unpack (scServerUrl creds)) ++ "/api/events")
  let headReq = req {method = "HEAD"}
  result <- tryHttp (httpLbs headReq manager)
  case result of
    Right resp -> do
      let code = statusCode (responseStatus resp)
      if code >= 200 && code < 300
        then putStrLn "{\"status\":\"ok\",\"mode\":\"sync\",\"transport\":\"events\"}"
        else do
          hPutStrLn stderr $ "Status request failed: HTTP " ++ show code
          exitFailure
    Left err -> do
      hPutStrLn stderr $ "Status request failed: " ++ show err
      exitFailure

authedRequest :: SyncCredentials -> String -> IO Request
authedRequest creds url = do
  req <- parseRequest url
  pure $
    applyBasicAuth
      (TE.encodeUtf8 (scUsername creds))
      (TE.encodeUtf8 (scPassword creds))
      req

offlineMessage :: StatusTarget -> String
offlineMessage (LocalStatus port) =
  "Server not running on localhost:" ++ show port
offlineMessage RemoteStatus =
  "Remote status endpoint is unreachable"
offlineMessage (UrlStatus url) =
  "Status endpoint is unreachable: " ++ url

stripTrailingSlash :: String -> String
stripTrailingSlash = reverse . dropWhile (== '/') . reverse

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp = Control.Exception.try
