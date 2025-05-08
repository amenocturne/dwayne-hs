{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Tui.Keybindings where

import Control.Monad (when)

import Brick
import Brick.Keybindings as K
import qualified Brick.Types as BT
import Control.Lens
import Data.Maybe (isJust)
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Tui.Types
import Writer.Writer

import Brick.BChan
import Data.List.NonEmpty (NonEmpty (..), fromList)
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

errorDialogKeyContext :: AppContext a -> Bool
errorDialogKeyContext = isJust . view (appState . errorDialog)

normalKeyContext :: AppContext a -> Bool
normalKeyContext ctx = not $ errorDialogKeyContext ctx

class ToBind a where
  toKey :: a -> NonEmpty KeyPress

instance ToBind Char where
  toKey c = pure $ KeyPress (KChar c) []

instance ToBind Key where
  toKey c = pure $ KeyPress c []

instance ToBind (NonEmpty Char) where
  toKey s = s >>= toKey

toKeySeq :: String -> NonEmpty KeyPress
toKeySeq s = toKey $ fromList s

normalModeBindings :: (Writer a) => [KeyBinding a]
normalModeBindings =
  [ -- Error dialog
    KeyBinding ErrorDialogQuit (toKey KEsc) "Quit error dialog" proceedInErrorDialog errorDialogKeyContext
  , KeyBinding ErrorDialogAccept (toKey KEnter) "Accept selected option" proceedInErrorDialog errorDialogKeyContext
  , -- Normal mode
    KeyBinding MoveUp (toKey 'k') "Move up" (adjustCursor (\i -> i - 1)) normalKeyContext
  , KeyBinding MoveDown (toKey 'j') "Move down" (adjustCursor (+ 1)) normalKeyContext
  , KeyBinding JumpEnd (toKey 'G') "Jump to the end" (adjustCursor (const maxBound)) normalKeyContext
  , KeyBinding JumpEnd (toKeySeq "gg") "Jump to the end" (adjustCursor (const 0)) normalKeyContext
  , KeyBinding Quit (toKey 'q') "Quit" halt normalKeyContext
  , KeyBinding EditInEditor (toKey KEnter) "Edit in editor" editSelectedTaskInEditor normalKeyContext
  ]
