{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Tui.Tui where

import Brick hiding (Location)
import Control.Lens
import Data.Functor
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import Graphics.Vty.Attributes (blue, brightBlue, brightCyan, brightGreen, brightMagenta, brightRed, brightWhite, brightYellow, cyan, green, magenta, red, rgbColor, white, yellow)
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Model.OrgMode
import Render.Render

import System.FilePath ((</>))
import Tui.ColorScheme
import Tui.Events
import Tui.Render
import Tui.Types

import Writer.Writer

import Brick.BChan
import Control.Monad (guard)
import Data.List
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Yaml (ParseException)
import Data.Yaml.Aeson (decodeFileEither)
import Model.LinearHistory (initLinearHistory)
import Parser.Parser
import Searcher.Searcher
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import TextUtils

class Tui a where
  tui :: SystemConfig a -> IO ()

instance (Searcher a, RenderTask a Name, Writer a, Show a, Eq a) => Tui a where
  tui sysConf = do
    configFilePath <- getConfigPath
    parsedConfig <- decodeFileEither configFilePath :: IO (Either ParseException (AppConfig a))
    conf <- case parsedConfig of
      Left err -> Prelude.error $ show err
      Right conf -> return conf
    parsedFiles <- mapM (\f -> fmap (f,) (readTasks (view fileParser sysConf) f)) (view files conf)
    eventChan <- newBChan 10 -- maybe should use different event channel size
    let fState = M.fromList (fmap (\(a, (_, c)) -> (a, c)) parsedFiles)
    let parsingErrors = mapMaybe (\(f, (l, e)) -> fmap (f,l,) (errorToMaybe e)) parsedFiles
    let pointers = getAllPointers fState

    -- NOTE: Safe bet that there will be less than 200 tasks on the screen as we don't
    -- know the size of the viewport in the beginning
    let maxEndIndex = 200
    let viewSpec = ViewSpec{_vsFilters = view defaultFilters sysConf, _vsSorter = view defaultSorter sysConf, _vsVersion = 0}
    let state =
          AppState
            { _eventChannel = eventChan
            , _errorDialog = Nothing
            , _keyState = NoInput
            , _appMode = NormalMode
            , _cmdState = Nothing
            , _compactView =
                initLinearHistory
                  CompactView
                    { _compactViewTaskStartIndex = 0
                    , _compactViewTasksEndIndex = min (V.length pointers - 1) maxEndIndex
                    , _cursor = 0 <$ listToMaybe (V.toList pointers)
                    , _cachedView = computeCurrentView fState pointers viewSpec
                    , _viewSpec = viewSpec
                    }
            , _fileState = initLinearHistory fState
            , _originalFileState = fState
            , _selection = Set.empty
            , _selectionAnchor = Nothing
            }
    let ctx =
          AppContext
            { _appState = state
            , _config = conf
            , _system = sysConf
            }
    let app =
          App
            { appDraw = drawUI
            , appChooseCursor = showFirstCursor
            , appHandleEvent = handleEvent
            , appStartEvent = return ()
            , appAttrMap = return $ theAppAttrMap $ getColorScheme (view colorScheme conf)
            }
    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    case parsingErrors of
      [] -> return ()
      errs ->
        writeBChan (view (appState . eventChannel) ctx) $
          Error $
            intercalate "\n" $
              fmap (\(f, l, e) -> "Error while parsing `" ++ f ++ "`: " ++ e ++ " at " ++ show (line l) ++ ":" ++ show (column l)) errs

    void $ customMain initialVty buildVty (Just eventChan) app ctx
   where
    readTasks :: Parser a -> FilePath -> IO (Location, ParserResult a)
    readTasks p f = do
      c <- readFileExample f
      let (l, _, tasks) = runParser p c
      return (l, tasks)
