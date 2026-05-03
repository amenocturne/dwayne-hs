{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Sync client configuration: parses ~/.config/dwayne/credentials.yml.
module Sync.Config
  ( SyncCredentials (..),
    loadCredentials,
    credentialsPath,
  )
where

import Data.Aeson (FromJSON (..), withObject, (.:))
import qualified Data.Text as T
import qualified Data.Yaml.Aeson as Y
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>))

data SyncCredentials = SyncCredentials
  { scServerUrl :: T.Text,
    scUsername :: T.Text,
    scPassword :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON SyncCredentials where
  parseJSON = withObject "SyncCredentials" $ \v ->
    SyncCredentials
      <$> v .: "serverUrl"
      <*> v .: "username"
      <*> v .: "password"

-- | Resolve the credentials file path. Honours $DWAYNE_CREDENTIALS, then
-- $XDG_CONFIG_HOME/dwayne/credentials.yml, falling back to ~/.config/dwayne/credentials.yml.
credentialsPath :: IO FilePath
credentialsPath = do
  mFromEnv <- lookupEnv "DWAYNE_CREDENTIALS"
  case mFromEnv of
    Just p -> pure p
    Nothing -> do
      mXdg <- lookupEnv "XDG_CONFIG_HOME"
      home <- getHomeDirectory
      let suffix = "dwayne" </> "credentials.yml"
      pure $ case mXdg of
        Just xdg -> xdg </> suffix
        Nothing -> home </> ".config" </> suffix

loadCredentials :: IO SyncCredentials
loadCredentials = do
  path <- credentialsPath
  exists <- doesFileExist path
  if not exists
    then
      die $
        "Sync requires credentials at "
          ++ path
          ++ ".\n"
          ++ "Format (YAML):\n"
          ++ "  serverUrl: https://dwayne.example.com\n"
          ++ "  username: dwayne\n"
          ++ "  password: <secret>"
    else do
      parsed <- Y.decodeFileEither path
      case parsed of
        Left err -> die $ "Failed to parse credentials at " ++ path ++ ": " ++ show err
        Right c -> pure c
