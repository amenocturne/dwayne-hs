{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Tui.Keybindings where

import Control.Monad (when)

import Brick
import qualified Brick.Types as BT
import Control.Lens
import Data.Maybe (isJust)
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Tui.Types
import Writer.Writer

import Brick.BChan
import Data.Char (isUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import qualified Data.Set as S
import Model.OrgMode (Task, todoKeyword)
import Parser.Parser
import TextUtils
import Writer.OrgWriter ()

-- TODO: make a shortcut to open in a default browser first found link in a task (useful for music/articles)
-- TODO: make a shortcut to download music from youtube/youtube music links
-- TODO: make a shortcut to save note contents directly to obsidian vault and open obsidian with this file to continue editing
-- TODO: make a shortcut to copy task to clipboard

-- Helper functions

adjustCursor :: (Int -> Int) -> GlobalAppState a
adjustCursor f = do
  state <- get
  let cv = view currentViewLens state
  let modifyCursor c = clamp 0 (length cv - 1) (f c)
  modify $ over (currentCursorLens . _Just) modifyCursor
  adjustViewport

adjustViewport :: GlobalAppState a
adjustViewport = do
  ctx <- get
  mvp <- lookupViewport Viewport1
  let maybeCursor = view currentCursorLens ctx
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
  let ct = preview currentTaskLens ctx
  let maybePtr = preview currentTaskPtr ctx
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
              return $ set (fileStateLens . taskBy ptr) t ctx
            ParserFailure e -> do
              _ <- writeBChan (view (appState . eventChannel) ctx) $ Error (e ++ " at " ++ show (line l) ++ ":" ++ show (column l))
              return ctx
    _ -> return ()

proceedInErrorDialog :: (Writer a) => GlobalAppState a
proceedInErrorDialog = modify (over (appState . errorDialog) (const Nothing))

saveForUndo :: AppContext a -> AppContext a
saveForUndo s = (setUndo . setRedo) s
 where
  setUndo = set undoStackLens (view tasksStateLens s : view undoStackLens s)
  setRedo = set redoStackLens []

undo :: AppContext a -> AppContext a
undo s = f s
 where
  undoSt = view undoStackLens s
  redoSt = view redoStackLens s
  curSt = view tasksStateLens s
  f = case undoSt of
    [] -> id
    x : xs -> set undoStackLens xs . set tasksStateLens x . set redoStackLens (curSt : redoSt)

redo :: AppContext a -> AppContext a
redo s = f s
 where
  undoSt = view undoStackLens s
  redoSt = view redoStackLens s
  curSt = view tasksStateLens s
  f = case redoSt of
    [] -> id
    x : xs -> set redoStackLens xs . set tasksStateLens x . set undoStackLens (curSt : undoSt)

-- Needs to be specific type
changeTodoKeyword :: T.Text -> AppContext Task -> AppContext Task
changeTodoKeyword keyword = over (currentTaskLens . todoKeyword) (const keyword)

----------------------- Bindings ----------------------------

errorDialogKeyContext :: AppContext a -> Bool
errorDialogKeyContext = isJust . view (appState . errorDialog)

normalKeyContext :: AppContext a -> Bool
normalKeyContext ctx = not $ errorDialogKeyContext ctx

class WithMod a where
  withMod :: a -> Modifier -> NonEmpty KeyPress

instance WithMod Char where
  withMod c m = pure $ KeyPress (KChar $ toLower c) withShift
   where
    withShift = if isUpper c then S.fromList [MShift, m] else S.singleton m

class ToBind a where
  toKey :: a -> NonEmpty KeyPress

instance ToBind Char where
  toKey c = pure $ KeyPress (KChar $ toLower c) withShift
   where
    withShift = if isUpper c then S.singleton MShift else S.empty

instance ToBind Key where
  toKey c = pure $ KeyPress c S.empty

instance ToBind String where
  toKey s = fromList s >>= toKey

instance ToBind T.Text where
  toKey s = toKey $ T.unpack s

toKeySeq :: String -> NonEmpty KeyPress
toKeySeq s = fromList s >>= toKey

changeTodoKeywordBinding :: T.Text -> String -> KeyBinding Task
changeTodoKeywordBinding keyword bind = KeyBinding (ChangeTodoKeyword keyword) (toKey bind) (T.concat ["Change todo keyword to ", keyword]) (modify saveForUndo >> modify (changeTodoKeyword keyword)) normalKeyContext

class ToNormalModeBinding k where
  normalBinding :: KeyEvent -> k -> T.Text -> GlobalAppState a -> KeyBinding a

instance ToNormalModeBinding Char where
  normalBinding event bind desc action = KeyBinding event (toKey bind) desc action normalKeyContext

instance ToNormalModeBinding Key where
  normalBinding event bind desc action = KeyBinding event (toKey bind) desc action normalKeyContext

instance ToNormalModeBinding (NonEmpty KeyPress) where
  normalBinding event bind desc action = KeyBinding event bind desc action normalKeyContext

normalModeBindings :: [KeyBinding Task]
normalModeBindings =
  [ --------------------------------- Error Dialog -----------------------------
    KeyBinding ErrorDialogQuit (toKey KEsc) "Quit error dialog" proceedInErrorDialog errorDialogKeyContext
  , KeyBinding ErrorDialogAccept (toKey KEnter) "Accept selected option" proceedInErrorDialog errorDialogKeyContext
  , --------------------------------- Normal Mode -----------------------------
    -- Movement
    normalBinding MoveUp 'k' "Move up" $ adjustCursor (\i -> i - 1)
  , normalBinding MoveDown 'j' "Move down" $ adjustCursor (+ 1)
  , normalBinding JumpEnd 'G' "Jump to the end" $ adjustCursor (const maxBound)
  , normalBinding JumpBeginning (toKeySeq "gg") "Jump to the end" $ adjustCursor (const 0)
  , -- KeyBuffer
    normalBinding CleanKeyState KEsc "Clean key state" $ modify $ set (appState . keyState) NoInput
  , -- History
    normalBinding Undo 'u' "Undo" $ modify undo
  , normalBinding Redo (withMod 'r' MCtrl) "Redo" $ modify redo
  -- Todo Keywords
  , changeTodoKeywordBinding "INBOX" "ti"
  , changeTodoKeywordBinding "RELEVANT" "tr"
  , changeTodoKeywordBinding "SOMEDAY" "ts"
  , changeTodoKeywordBinding "NOTES" "tn"
  , changeTodoKeywordBinding "LIST" "tl"
  , changeTodoKeywordBinding "WAITING" "tw"
  , changeTodoKeywordBinding "PROJECTS" "tp"
  , changeTodoKeywordBinding "TODO" "tt"
  , changeTodoKeywordBinding "DONE" "td"
  , changeTodoKeywordBinding "TRASH" "tx"
  , -- Other
    normalBinding Quit (toKey 'q') "Quit" halt
  , normalBinding EditInEditor (toKey KEnter) "Edit in editor" $ modify saveForUndo >> editSelectedTaskInEditor
  ]
