{-# LANGUAGE ScopedTypeVariables #-}

module Commands.Status
  ( StatusArgsError (..),
    statusUrl,
    parseStatusPort,
    runStatusCommand,
  )
where

import qualified Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Client
  ( HttpException (..),
    HttpExceptionContent (..),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
  )
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

newtype StatusArgsError = StatusArgsError String
  deriving (Eq, Show)

statusUrl :: Int -> String
statusUrl port = "http://localhost:" ++ show port ++ "/api/health"

parseStatusPort :: [String] -> Either StatusArgsError Int
parseStatusPort [] = Right 8080
parseStatusPort ["--port", rawPort] = parsePort rawPort
parseStatusPort ["-p", rawPort] = parsePort rawPort
parseStatusPort _ =
  Left $
    StatusArgsError
      "Usage: dwayne status [--port PORT]"

parsePort :: String -> Either StatusArgsError Int
parsePort rawPort =
  case readMaybe rawPort of
    Just port | port > 0 -> Right port
    _ -> Left $ StatusArgsError "Port must be a positive integer"

runStatusCommand :: [String] -> IO ()
runStatusCommand args =
  case parseStatusPort args of
    Left (StatusArgsError msg) -> do
      hPutStrLn stderr msg
      exitFailure
    Right port -> do
      manager <- newManager defaultManagerSettings
      req <- parseRequest (statusUrl port)
      result <- tryHttp (httpLbs req manager)
      case result of
        Right resp -> BSL.putStrLn (responseBody resp)
        Left (HttpExceptionRequest _ (ConnectionFailure _)) -> do
          putStrLn $ "Server not running on localhost:" ++ show port
          exitFailure
        Left err -> do
          hPutStrLn stderr $ "Status request failed: " ++ show err
          exitFailure

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp = Control.Exception.try
