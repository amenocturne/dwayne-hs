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

import Tui.ColorScheme
import Tui.Events
import Tui.Render
import Tui.Types

import Writer.Writer

import Brick.BChan
import Data.List
import qualified Data.Text as T
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

defaultColorScheme :: ColorScheme
defaultColorScheme =
  ColorScheme
    { _todoKeywordColors =
        M.fromList
          [ ("INBOX", lavender)
          , ("RELEVANT", mauve)
          , ("SOMEDAY", maroon)
          , ("NOTES", green)
          , ("LIST", blue)
          , ("WAITING", mauve)
          , ("PROJECTS", green)
          , ("TODO", yellow)
          , ("DONE", surface2)
          , ("TRASH", surface2)
          , ("", textColor)
          ]
    , _priorityColors = [red, yellow, blue]
    , _tagColor = textColor
    , _timeFieldColor = textColor
    , _levelColors = [yellow, red, green, blue, mauve, teal]
    , _propertyColor = textColor
    , _descriptionColor = textColor
    , _defaultColor = textColor
    , _highlightBgColor = highlight
    }
 where
  -- Catppuccin Mocha base
  textColor = rgbColor 205 214 244 -- #CDD6F4
  surface1 = rgbColor 73 77 100 -- #494D64
  surface2 = rgbColor 88 91 112 -- #585B70
  highlight = rgbColor 20 20 100

  -- Accent colors
  lavender = rgbColor 180 190 254 -- #B4BEFE
  red = rgbColor 243 139 168 -- #F38BA8
  maroon = rgbColor 235 160 172 -- #EBA0AC
  mauve = rgbColor 203 166 247 -- #CBA6F7
  flamingo = rgbColor 221 161 161 -- #DDA1A1
  pink = rgbColor 245 194 231 -- #F5C2E7
  blue = rgbColor 137 180 250 -- #89B4FA
  teal = rgbColor 148 226 213 -- #94E2D5
  green = rgbColor 166 227 161 -- #A6E3A1
  yellow = rgbColor 249 226 175 -- #F9E0AF

-- Helper function to add default color scheme to existing config
withDefaultColors :: AppConfig a -> AppConfig a
withDefaultColors config = config{_colorScheme = defaultColorScheme}

class Tui a where
  tui :: AppConfig a -> IO ()

instance (Searcher a, RenderTask a Name, Writer a, Show a, Eq a) => Tui a where
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
            , _cmdState = Nothing
            , _compactView =
                initLinearHistory
                  CompactView
                    { _compactViewTaskStartIndex = 0
                    , _compactViewTasksEndIndex = min (V.length pointers - 1) 200 -- NOTE: safe bet that there will be less than 200 tasks on the screen as we don't know the size of the viewport in the beginning
                    , _cursor = 0 <$ listToMaybe (V.toList pointers)
                    , _currentView = pointers
                    }
            , _fileState = initLinearHistory fState
            , _originalFileState = fState
            }
    let ctx =
          AppContext
            { _appState = state
            , _config = conf
            }
    let app =
          App
            { appDraw = drawUI -- List in type signature because each element is a layer and thus you can put widgets on top of one another
            , appChooseCursor = showFirstCursor
            , appHandleEvent = handleEvent
            , appStartEvent = return ()
            , appAttrMap = theAppAttrMap . view (config . colorScheme)
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
