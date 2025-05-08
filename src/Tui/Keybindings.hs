{-# LANGUAGE OverloadedStrings #-}

module Tui.Keybindings where

import Control.Monad (forM_, when)

import Brick
import Brick.Keybindings as K
import qualified Brick.Types as BT
import Control.Lens
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Graphics.Vty.Input.Events
import System.Exit (exitFailure)
import Tui.Types
import Writer.Writer

import Brick.BChan
import Parser.Parser
import TextUtils

-- TODO: make a shortcut to open in a default browser first found link in a task (useful for music/articles)
-- TODO: make a shortcut to download music from youtube/youtube music links
-- TODO: make a shortcut to save note contents directly to obsidian vault and open obsidian with this file to continue editing
-- TODO: make a shortcut to copy task to clipboard

allKeyEvents :: [KeyEvent]
allKeyEvents = [minBound .. maxBound]

keyEventsMapping :: K.KeyEvents KeyEvent
keyEventsMapping =
  K.keyEvents $
    fmap (\k -> (T.pack $ show k, k)) allKeyEvents

toBinding :: KeyBinding a -> (KeyEvent, [K.Binding])
toBinding (KeyBinding ev b _ _) = (ev, b)

-- Helper functions

adjustCursor :: (Int -> Int) -> GlobalAppState a
adjustCursor f = do
  state <- get
  let cv = view (appState . currentView) state
  let modifyCursor c = clamp 0 (length cv - 1) (f c)
  modify $ over (currentCursor . _Just) modifyCursor
  adjustViewport

adjustViewport :: GlobalAppState a
adjustViewport = do
  ctx <- get
  mvp <- lookupViewport Viewport1
  let maybeCursor = view (appState . currentTask) ctx
  case (mvp, maybeCursor) of
    (Just vp, Just cursor) -> do
      let marginVal = view (config . scrollingMargin) ctx
      let currentTop = vp ^. BT.vpTop
          visibleHeight = snd (vp ^. BT.vpSize)
          newTop
            | cursor >= currentTop + visibleHeight - marginVal =
                cursor - (visibleHeight - marginVal - 1)
            | cursor <= currentTop + marginVal =
                max 0 (cursor - marginVal)
            | otherwise = currentTop
      setTop (viewportScroll Viewport1) newTop
    _ -> return ()

-- TODO: refactor
editSelectedTaskInEditor :: (Writer a) => GlobalAppState a
editSelectedTaskInEditor = do
  ctx <- get
  let ct = preview (appState . currentTaskLens) ctx
  let maybePtr = preview (appState . currentTaskPtr) ctx
  case (ct, maybePtr) of
    (Just task, Just ptr) -> suspendAndResume $ do
      editedContent <- editWithEditor (write task)
      when (null editedContent) $ return ()
      case editedContent of
        Nothing -> return ctx
        Just editedStr -> do
          let (l, _, result) = runParser (view (config . taskParser) ctx) (T.pack editedStr)
          case result of
            ParserSuccess t -> do
              return $ set (appState . fileState . taskBy ptr) t ctx
            ParserFailure e -> do
              _ <- writeBChan (view (appState . eventChannel) ctx) $ Error (e ++ " at " ++ show (line l) ++ ":" ++ show (column l))
              return ctx
    _ -> return ()

proceedInErrorDialog :: (Writer a) => GlobalAppState a
proceedInErrorDialog = modify (over (appState . errorDialog) (const Nothing))

----------------------- Bindings ----------------------------

normalModeBindings :: (Writer a) => [KeyBinding a]
normalModeBindings =
  [ KeyBinding MoveUp [K.bind 'k'] "Move up" $ adjustCursor (\i -> i - 1)
  , KeyBinding MoveDown [K.bind 'j'] "Move down" $ adjustCursor (+ 1)
  , KeyBinding JumpEnd [K.bind 'G'] "Jump to the end" $ adjustCursor (const maxBound)
  , KeyBinding Quit [K.bind 'q'] "Quit" halt
  , KeyBinding EditInEditor [K.bind KEnter] "Edit in editor" editSelectedTaskInEditor
  ]

errorDialogBindings :: (Writer a) => [KeyBinding a]
errorDialogBindings =
  [ KeyBinding ErrorDialogQuit [K.bind KEsc] "Quit error dialog" proceedInErrorDialog
  , KeyBinding ErrorDialogAccept [K.bind KEnter] "Accept selected option" proceedInErrorDialog
  ]

------------------------- Dispatcher ----------------------------

makeEventHandler :: (Writer a) => [KeyBinding a] -> [K.KeyEventHandler KeyEvent (GlobalAppStateF a)]
makeEventHandler =
  fmap
    ( \b ->
        K.onEvent
          (view keyEvent b)
          (view keyDecription b)
          (view keyAction b)
    )

makeKeyDispatcher :: (Writer a) => [KeyBinding a] -> (AppContext a -> Bool) -> Either [(Binding, [KeyHandler KeyEvent (GlobalAppStateF a)])] (KeyEventDispatcher a)
makeKeyDispatcher bindings precondition = fmap (`KeyEventDispatcher` precondition) maybeDispatcher
 where
  maybeDispatcher = K.keyDispatcher kc (makeEventHandler bindings)
  kc = K.newKeyConfig keyEventsMapping (fmap toBinding bindings) [] -- Maybe I should add config files in the future

-- TODO: refactor
displayCollisionError :: [(Binding, [KeyHandler KeyEvent (GlobalAppStateF a)])] -> IO b
displayCollisionError collisions =
  do
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
