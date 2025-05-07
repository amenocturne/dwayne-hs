{-# LANGUAGE OverloadedStrings #-}

module Tui.Keybindings where

import Control.Monad (when)

import Brick
import qualified Brick.Types as BT
import Control.Lens
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Tui.Types
import Writer.Writer

import Brick.BChan
import Brick.Keybindings as K
import Parser.Parser
import TextUtils

allKeyEvents :: [KeyEvent]
allKeyEvents = [minBound .. maxBound]

keyEventsMapping :: K.KeyEvents KeyEvent
keyEventsMapping =
  K.keyEvents $
    fmap (\k -> (T.pack $ show k, k)) allKeyEvents

bindKey :: KeyEvent -> [K.Binding]
bindKey MoveUp = [K.bind 'k']
bindKey MoveDown = [K.bind 'j']
bindKey JumpEnd = [K.bind 'G']
bindKey Quit = [K.bind 'q']
bindKey EditInEditor = [K.bind KEnter]

defaultBindings :: [(KeyEvent, [K.Binding])]
defaultBindings = fmap (\k -> (k, bindKey k)) allKeyEvents

handleKeyEvent :: (Writer a) => KeyEvent -> K.KeyEventHandler KeyEvent (GlobalAppStateF a)
handleKeyEvent ke = K.onEvent ke description action
 where
  (description, action) = case ke of
    MoveUp -> ("Move up", adjustCursor (\i -> i - 1))
    MoveDown -> ("Move down", adjustCursor (+ 1))
    JumpEnd -> ("Jump to the end", adjustCursor (const maxBound))
    Quit -> ("Quit", halt)
    EditInEditor -> ("Edit in editor", editSelectedTaskInEditor)
  adjustCursor :: (Int -> Int) -> GlobalAppState a
  adjustCursor f = do
    state <- get
    let cv = view (appState . currentView) state
    let modifyCursor c = clamp 0 (length cv - 1) (f c)
    modify $ over (currentCursor . _Just) modifyCursor
    adjustViewport
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

keyEventHandler :: (Writer a) => [K.KeyEventHandler KeyEvent (GlobalAppStateF a)]
keyEventHandler = fmap handleKeyEvent allKeyEvents
