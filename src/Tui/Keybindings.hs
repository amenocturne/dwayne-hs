{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
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
import Control.Monad.IO.Class (liftIO)
import Data.Char (isUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Model.LinearHistory as L
import Model.OrgMode (Task, todoKeyword)
import Parser.Parser
import Searcher.OrgSearcher ()
import Searcher.Searcher
import TextUtils
import Tui.Tui
import Writer.OrgWriter ()

-- TODO: make a shortcut to open in a default browser first found link in a task (useful for music/articles)
-- TODO: make a shortcut to download music from youtube/youtube music links
-- TODO: make a shortcut to save note contents directly to obsidian vault and open obsidian with this file to continue editing
-- TODO: make a shortcut to copy task to clipboard
-- TODO: make selection mode for bulk actions

-- Helper functions

adjustCursor :: (Int -> Int) -> GlobalAppState a
adjustCursor f = do
  ctx <- get
  let cv = view currentViewLens ctx
  let modifyCursor c = clamp 0 (length cv - 1) (f c)
  let currentCursor = view cursorLens ctx
  let newCursor = fmap modifyCursor currentCursor
  modify $ set cursorLens newCursor
  adjustViewport

adjustViewport :: GlobalAppState a
adjustViewport = do
  ctx <- get
  let curCursor = view cursorLens ctx
  let marginVal = view (config . scrollingMargin) ctx
  maybeExtent <- lookupExtent CompactViewWidget
  case (curCursor, maybeExtent) of
    (Just cur, Just extent) -> do
      let compView = view compactViewLens ctx
      let compStart = view compactViewTaskStartIndex compView
      let compEnd = view compactViewTasksEndIndex compView
      let curView = view currentViewLens ctx
      let height = snd (BT.extentSize extent)
      let clampPos i = min (max i 0) (V.length curView - 1)

      let fromDesiredTop desiredTop
            | desiredTop <= 0 = set compactViewTaskStartIndex 0 . set compactViewTasksEndIndex (clampPos height)
            | otherwise = set compactViewTaskStartIndex desiredTop . set compactViewTasksEndIndex (clampPos $ desiredTop + height - 1)

      let fromDesiredBottom desiredBottom
            | desiredBottom >= V.length curView - 1 = set compactViewTaskStartIndex (clampPos $ V.length curView - height) . set compactViewTasksEndIndex (clampPos $ V.length curView - 1)
            | otherwise = set compactViewTaskStartIndex (clampPos $ desiredBottom - height) . set compactViewTasksEndIndex desiredBottom

      let newCompactView
            | cur < compStart + marginVal =
                fromDesiredTop (cur - marginVal) compView
            | cur > compEnd - marginVal =
                fromDesiredBottom (cur + marginVal) compView
            | otherwise = compView
      modify $ over compactViewLens (const newCompactView)
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

saveForUndo :: (Eq a) => GlobalAppState a -> GlobalAppState a
saveForUndo f = do
  ctx <- get
  let oldHist = view (appState . fileState) ctx
  f
  newCtx <- get
  let newState = view fileStateLens newCtx
  when (newState /= view L.currentState oldHist) $ modify $ over (appState . fileState) (const $ L.append newState oldHist)

undo :: AppContext a -> AppContext a
undo = over (appState . fileState) L.undo

redo :: AppContext a -> AppContext a
redo = over (appState . fileState) L.redo

saveForJump :: GlobalAppState a -> GlobalAppState a
saveForJump f = do
  ctx <- get
  let oldHist = view (appState . compactView) ctx
  f
  newCtx <- get
  let newState = view compactViewLens newCtx
  when (newState /= view L.currentState oldHist) $ modify $ over (appState . compactView) (const $ L.append newState oldHist)

jumpBack :: AppContext a -> AppContext a
jumpBack = over (appState . compactView) L.undo

jumpForward :: AppContext a -> AppContext a
jumpForward = over (appState . compactView) L.redo

-- Needs to be specific type
changeTodoKeyword :: T.Text -> AppContext Task -> AppContext Task
changeTodoKeyword keyword = over (currentTaskLens . todoKeyword) (const keyword)

switchMode :: AppMode a -> AppContext a -> AppContext a
switchMode mode = over (appState . appMode) (const mode)

abortSearch :: AppContext a -> AppContext a
abortSearch = set (appState . searchState) Nothing

removeLastIfExists :: T.Text -> T.Text
removeLastIfExists t
  | T.null t = t -- Return unchanged if empty
  | otherwise = T.dropEnd 1 t

searchDeleteChar :: AppContext a -> AppContext a
searchDeleteChar ctx = f ctx
 where
  input = preview (appState . searchState . _Just . searchInput) ctx
  f = case fmap T.null input of
    Just False -> over (appState . searchState . _Just . searchInput) removeLastIfExists
    _ -> switchMode NormalMode . set (appState . searchState) Nothing

applySearch :: (Searcher a) => AppContext a -> AppContext a
applySearch ctx =
  ( set currentViewLens newView
      . set cursorLens newCurTask
      . cleanCompactView
      . abortSearch
  )
    ctx
 where
  compView = view compactViewLens ctx
  start = view compactViewTaskStartIndex compView
  end = view compactViewTasksEndIndex compView
  cleanCompactView = set (compactViewLens . compactViewTaskStartIndex) 0 . set (compactViewLens . compactViewTasksEndIndex) (min (V.length newView - 1) (end - start + 1))
  oldView = view currentViewLens ctx
  fs = view fileStateLens ctx
  maybeQuery = preview (appState . searchState . _Just . searchInput) ctx
  ptrToMatch q ptr = maybe False (matches q) (preview (taskBy ptr) fs)
  (newCurTask, newView) = case fmap (\q -> V.filter (ptrToMatch q) oldView) maybeQuery of
    Just nv -> if V.length nv > 0 then (Just 0, nv) else (Nothing, nv)
    Nothing -> (view cursorLens ctx, oldView)

applyFilterToAllTasks :: (a -> Bool) -> GlobalAppState a
applyFilterToAllTasks predicate = do
  ctx <- get
  let fs = view fileStateLens ctx
  let ptrToMatch ptr = maybe False predicate (preview (taskBy ptr) fs)
  let newView = V.filter ptrToMatch (getAllPointers fs)
  let newCurTask = if V.length newView > 0 then Just 0 else Nothing
  modify $ over currentViewLens (const newView)
  modify $ over cursorLens (const newCurTask)
  adjustViewport

todoKeywordFilter :: T.Text -> Task -> Bool
todoKeywordFilter keyword task = view todoKeyword task == keyword

----------------------- Bindings ----------------------------

errorDialogKeyContext :: AppContext a -> Bool
errorDialogKeyContext = isJust . view (appState . errorDialog)

modeKeyContext :: AppMode a -> AppContext a -> Bool
modeKeyContext mode ctx = view (appState . appMode) ctx == mode && not (errorDialogKeyContext ctx)

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
changeTodoKeywordBinding keyword bind =
  KeyBinding
    (ChangeTodoKeyword keyword)
    (toKey bind)
    (T.concat ["Change todo keyword to ", keyword])
    (saveForUndo $ modify (changeTodoKeyword keyword))
    (modeKeyContext NormalMode)

changeViewKeywordBinding :: T.Text -> String -> KeyBinding Task
changeViewKeywordBinding keyword bind =
  normalBinding
    (View keyword)
    (toKeySeq bind)
    (T.concat ["Show ", keyword, " tasks"])
    $ saveForJump (applyFilterToAllTasks (todoKeywordFilter keyword))

class ToBinding k where
  toBinding :: AppMode a -> KeyEvent -> k -> T.Text -> GlobalAppState a -> KeyBinding a

instance ToBinding Char where
  toBinding mode event bind desc action = KeyBinding event (toKey bind) desc action (modeKeyContext mode)

instance ToBinding Key where
  toBinding mode event bind desc action = KeyBinding event (toKey bind) desc action (modeKeyContext mode)

instance ToBinding (NonEmpty KeyPress) where
  toBinding mode event bind desc action = KeyBinding event bind desc action (modeKeyContext mode)

normalBinding :: (ToBinding k) => KeyEvent -> k -> T.Text -> GlobalAppState a -> KeyBinding a
normalBinding = toBinding NormalMode

searchBinding :: (ToBinding k) => KeyEvent -> k -> T.Text -> GlobalAppState a -> KeyBinding a
searchBinding = toBinding SearchMode

normalModeBindings :: [KeyBinding Task]
normalModeBindings =
  [ --------------------------------- Error Dialog -----------------------------
    KeyBinding ErrorDialogQuit (toKey KEsc) "Quit error dialog" proceedInErrorDialog errorDialogKeyContext
  , KeyBinding ErrorDialogAccept (toKey KEnter) "Accept selected option" proceedInErrorDialog errorDialogKeyContext
  , --------------------------------- Search Mode -----------------------------
    searchBinding AbortSearch (toKey KEsc) "Abort search" $ modify $ abortSearch . switchMode NormalMode
  , searchBinding SearchDeleteChar (toKey KBS) "Delete char" $ modify searchDeleteChar
  , searchBinding ApplySearch (toKey KEnter) "Apply search results" $ saveForJump $ modify (applySearch . switchMode NormalMode)
  , --------------------------------- Normal Mode -----------------------------
    -- Mode switching
    normalBinding SwitchToSearchMode '/' "Switch to search mode" $ modify $ switchMode SearchMode
  , -- Movement
    normalBinding MoveUp 'k' "Move up" $ adjustCursor (\i -> i - 1)
  , normalBinding MoveDown 'j' "Move down" $ adjustCursor (+ 1)
  , normalBinding JumpEnd 'G' "Jump to the end" $ saveForJump $ adjustCursor (const maxBound)
  , normalBinding JumpBeginning (toKeySeq "gg") "Jump to the end" $ saveForJump $ adjustCursor (const 0)
  , normalBinding JumpBackward (withMod 'o' MCtrl) "Jump backward" $ modify jumpBack
  , normalBinding JumpForward '\t' "Jump forward" $ modify jumpForward -- NOTE: Terminals translate Ctrl-i into TAB
  , -- KeyBuffer
    normalBinding CleanKeyState KEsc "Clean key state" $ modify $ set (appState . keyState) NoInput
  , -- History
    normalBinding Undo 'u' "Undo" $ modify undo
  , normalBinding Redo (withMod 'r' MCtrl) "Redo" $ modify redo
  , -- Todo Keywords
    changeTodoKeywordBinding "INBOX" "ti"
  , changeTodoKeywordBinding "RELEVANT" "tr"
  , changeTodoKeywordBinding "SOMEDAY" "ts"
  , changeTodoKeywordBinding "NOTES" "tn"
  , changeTodoKeywordBinding "LIST" "tl"
  , changeTodoKeywordBinding "WAITING" "tw"
  , changeTodoKeywordBinding "PROJECTS" "tp"
  , changeTodoKeywordBinding "TODO" "tt"
  , changeTodoKeywordBinding "DONE" "td"
  , changeTodoKeywordBinding "TRASH" "tx"
  , -- Views
    normalBinding (View "all") (toKeySeq " aa") "Show all tasks" $ saveForJump $ applyFilterToAllTasks (const True)
  , changeViewKeywordBinding "INBOX" " ai"
  , changeViewKeywordBinding "RELEVANT" " ar"
  , changeViewKeywordBinding "SOMEDAY" " as"
  , changeViewKeywordBinding "NOTES" " an"
  , changeViewKeywordBinding "LIST" " al"
  , changeViewKeywordBinding "WAITING" " aw"
  , changeViewKeywordBinding "PROJECTS" " ap"
  , changeViewKeywordBinding "TODO" " at"
  , changeViewKeywordBinding "DONE" " ad"
  , changeViewKeywordBinding "TRASH" " ax"
  , -- Other
    normalBinding Quit (toKey 'q') "Quit" halt
  , normalBinding EditInEditor (toKey KEnter) "Edit in editor" $ saveForUndo editSelectedTaskInEditor
  ]
