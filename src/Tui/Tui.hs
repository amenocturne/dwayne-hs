{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Tui.Tui where

import Brick hiding (Location)
import Control.Lens
import Data.Functor
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Model.OrgMode
import Render.Render

import Tui.Events
import Tui.Render
import Tui.Types

import Writer.Writer

import Brick.BChan
import Data.List
import qualified Data.Vector as V
import Model.LinearHistory (initLinearHistory)
import Parser.Parser
import Searcher.Searcher
import TextUtils

getAllPointers :: FileState a -> V.Vector TaskPointer
getAllPointers fs = V.concatMap fun (V.fromList $ M.toList fs) -- TODO: optimize all this convertions
 where
  fun (f, result) =
    maybe
      V.empty
      (\taskFile -> (\(i, _) -> TaskPointer f i) <$> V.zip (V.fromList [0 ..]) (_content taskFile))
      (resultToMaybe result)

class Tui a where
  tui :: AppConfig a -> IO ()

instance (Searcher a, RenderTask a Name, Writer a, Show a) => Tui a where
  tui conf = do
    parsedFiles <- mapM (\f -> fmap (f,) (readTasks (view fileParser conf) f)) (view files conf)
    eventChan <- newBChan 10 -- TODO: maybe use different event channel size
    let fState = M.fromList (fmap (\(a, (_, c)) -> (a, c)) parsedFiles)
    let parsingErrors = mapMaybe (\(f, (l, e)) -> fmap (f,l,) (errorToMaybe e)) parsedFiles
    let pointers = getAllPointers fState
    let state =
          AppState
            { _eventChannel = eventChan
            , _errorDialog = Nothing
            , _keyState = NoInput
            , _appMode = NormalMode
            , _searchState = Nothing
            , _compactView =
                CompactView
                  { _compactViewTaskStartIndex = 0
                  , _compactViewTasksEndIndex = min (V.length pointers - 1) 200 -- NOTE: safe bet that there will be less than 200 tasks on the screen as we don't know the size of the viewport in the beginning
                  , _cursor = 0 <$ listToMaybe (V.toList pointers)
                  , _currentView = pointers
                  }
            , _fileState = initLinearHistory fState
            }
    let ctx =
          AppContext
            { _appState = state
            , _config = conf
            }
    let app =
          App
            { appDraw = drawUI -- List in type signature because each element is a layer and thus you can put widgets on top of one another
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handleEvent
            , appStartEvent = return ()
            , appAttrMap = const theAppAttrMap
            }
    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    _ <- writeBChan (view (appState . eventChannel) ctx) SaveAllFiles
    case parsingErrors of
      [] -> return ()
      errs ->
        writeBChan (view (appState . eventChannel) ctx) $
          Error $
            intercalate "\n" $
              fmap (\(f, l, e) -> "Error while parsing `" ++ f ++ "`: " ++ e ++ " at " ++ show (line l) ++ ":" ++ show (column l)) errs

    void $ customMain initialVty buildVty (Just eventChan) app ctx
   where
    -- NOTE: useful code below to save file
    -- ParserSuccess (TaskFile name tasks) -> do
    -- let wrote = write (TaskFile name tasks)
    -- void $ writeFileExample "./resources/parsed.org" wrote
    -- ParserFailure e -> simpleMain (ui (show e))
    -- return ()

    readTasks :: Parser a -> FilePath -> IO (Location, ParserResult a)
    readTasks p f = do
      c <- readFileExample f
      let (l, _, tasks) = runParser p c
      return (l, tasks)
