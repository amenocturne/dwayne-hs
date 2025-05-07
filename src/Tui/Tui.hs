{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Tui.Tui where

import Control.Monad (forM_)

import Brick
import Control.Lens
import Data.Functor
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Text.IO as T
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Model.OrgMode
import Render.Render
import System.Exit (exitFailure)

import Tui.Events
import Tui.Keybindings
import Tui.Render
import Tui.Types

import Writer.Writer

import Brick.BChan
import Brick.Keybindings as K
import Parser.Parser
import TextUtils

-- TODO: I should go through the code, collect all the errors and create widget to properly display them
-- TODO: make a shortcut to open in a default browser first found link in a task (useful for music/articles)
-- TODO: make a shortcut to download music from youtube/youtube music links
-- TODO: make a shortcut to save note contents directly to obsidian vault and open obsidian with this file to continue editing
-- TODO: Should handle external edits to files when the app is opened and update its state correctly

------------------------ Initialization ----------------------------------------

getAllPointers :: FileState a -> [TaskPointer]
getAllPointers fs = concatMap fun (M.toList fs)
 where
  fun (f, result) =
    maybe
      []
      (\taskFile -> (\(i, _) -> TaskPointer f i) <$> zip [0 ..] (_content taskFile))
      (resultToMaybe result)

getKeyDispatcher :: (Writer a) => IO (KeyDispatcher KeyEvent (GlobalAppStateF a))
getKeyDispatcher = do
  let kc = K.newKeyConfig keyEventsMapping defaultBindings [] -- Maybe I should add config files in the future
  case K.keyDispatcher kc keyEventHandler of
    Right d -> return d
    Left collisions -> do
      putStrLn "Error: some key events have the same keys bound to them."
      forM_ collisions $ \(b, hs) -> do
        T.putStrLn $ "Handlers with the '" <> K.ppBinding b <> "' binding:"
        forM_ hs $ \h -> do
          let trigger = case K.kehEventTrigger $ K.khHandler h of
                K.ByKey k -> "triggered by the key '" <> K.ppBinding k <> "'"
                K.ByEvent e -> "triggered by the event '" <> fromJust (K.keyEventName keyEventsMapping e) <> "'"
              desc = K.handlerDescription $ K.kehHandler $ K.khHandler h

          T.putStrLn $ "  " <> desc <> " (" <> trigger <> ")"
      exitFailure

app :: (RenderTask a Name, Writer a) => App (AppContext a) AppEvent Name
app =
  App
    { appDraw = drawUI -- List in type signature because each element is a layer and thus you can put widgets on top of one another
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const theAppAttrMap
    }

class Tui a where
  tui :: AppConfig a -> IO ()

instance (RenderTask a Name, Writer a, Show a) => Tui a where
  tui conf = do
    dispatcher <- getKeyDispatcher
    parsedFiles <- mapM (\f -> fmap (f,) (readTasks (view fileParser conf) f)) (view files conf)
    eventChan <- newBChan 10 -- TODO: maybe use different event channel size
    let fState = M.fromList parsedFiles
    let pointers = getAllPointers fState
    let state =
          AppState
            { _fileState = fState
            , _currentView = pointers
            , _currentTask = 0 <$ listToMaybe pointers
            , _eventChannel = eventChan
            , _errorDialog = Nothing
            }
    let ctx =
          AppContext
            { _appState = state
            , _config = conf
            , _keyEventDispatcher = dispatcher
            }
    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just eventChan) app ctx
   where
    -- void $ defaultMain app ctx

    -- case parsedFiles of
    -- ParserSuccess files -> void $ defaultMain app (AppContext [] (M. parsedFiles) 0 CompactMode config keyDispatcher)
    -- NOTE: useful code below to save file
    -- ParserSuccess (TaskFile name tasks) -> do
    -- let wrote = write (TaskFile name tasks)
    -- void $ writeFileExample "./resources/parsed.org" wrote
    -- ParserFailure e -> simpleMain (ui (show e))
    -- return ()

    readTasks :: Parser a -> FilePath -> IO (ParserResult a)
    readTasks p f = do
      c <- readFileExample f
      let (_, _, tasks) = runParser p c
      return tasks
