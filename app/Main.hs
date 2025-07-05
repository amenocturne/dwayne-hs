{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.Yaml (decodeFileEither)
import Data.Yaml.Aeson (ParseException)
import Model.OrgMode (Task)
import Parser.OrgParser (anyTaskparser, orgFileParser)
import Render.OrgRender ()
import Searcher.OrgSearcher ()
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Tui.Keybindings (normalModeBindings)
import Tui.Tui
import Tui.Types
import Writer.OrgWriter ()

getConfigPath :: IO FilePath
getConfigPath = do
  mConfigFile <- lookupEnv "DWAYNE_CONFIG"
  mXdg <- lookupEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  let dwayneConfig = "dwayne" </> "config.yml"
  return $ case (mConfigFile, mXdg) of
    (Just configFile, _) -> configFile
    (_, Just xdg) -> xdg </> dwayneConfig
    _ -> home </> ".config" </> dwayneConfig

main :: IO ()
main = do
  configFilePath <- getConfigPath
  parsedContent <- decodeFileEither configFilePath :: IO (Either ParseException (AppConfig Task))
  case parsedContent of
    Left err -> error $ show err
    Right conf -> tui sysConf conf
     where
      sysConf =
        SystemConfig
          { _taskParser = anyTaskparser
          , _fileParser = orgFileParser
          , _keybindings = normalModeBindings
          }
