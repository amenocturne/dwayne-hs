{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Tui.Keybindings where

import Control.Monad (when)
import System.Process (callCommand)

import Brick
import qualified Brick.Types as BT
import Control.Applicative ((<|>))
import Control.Lens
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Text.Regex
import Tui.Types
import Writer.Writer

import Brick.BChan
import Control.Monad.IO.Class (liftIO)
import Data.Char (isUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import qualified Data.Set as S
import Data.Time (getZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import qualified Model.LinearHistory as L
import Model.OrgMode (
  Task (..),
  TaskFile,
  content,
  orgCreatedProperty,
  orgDayTimeFormat,
  tags,
  todoKeyword,
 )
import Parser.Parser
import Searcher.OrgSearcher ()
import Searcher.Searcher
import TextUtils
import Tui.Tui
import Writer.OrgWriter ()

-- TODO: make a shortcut to download music from youtube/youtube music links
-- TODO: make a shortcut to save note contents directly to obsidian vault and open obsidian with this file to continue editing
-- TODO: make a shortcut to copy task to clipboard
-- TODO: make selection mode for bulk actions
-- TODO: Make sorting actions

-- Helper functions

extractFirstUrl :: T.Text -> Maybe T.Text
extractFirstUrl text = case (orgMatches, urlMatches) of
  (Just (match : _), _) -> Just $ T.pack match
  (_, Just (match : _)) -> Just $ T.pack match
  _ -> Nothing
 where
  urlPattern = mkRegex "(https?://[^ \t\n]+)"
  urlMatches = matchRegex urlPattern $ T.unpack text
  orgPattern = mkRegex "\\[\\[(https?://[^ \t\n]+)\\]"
  orgMatches = matchRegex orgPattern $ T.unpack text

openUrlInBrowser :: T.Text -> IO ()
openUrlInBrowser url = callCommand $ "open " ++ T.unpack url

openTaskUrl :: GlobalAppState Task
openTaskUrl = do
  ctx <- get
  let ct = preview currentTaskLens ctx
  case ct of
    Just task -> do
      let titleUrl = extractFirstUrl (_title task)
          descUrl = extractFirstUrl (_description task)
          firstUrl = titleUrl <|> descUrl
      case firstUrl of
        Just url -> do
          liftIO $ openUrlInBrowser url
        Nothing -> return ()
    Nothing -> return ()

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

addNewTask :: GlobalAppState Task
addNewTask = do
  ctx <- get
  let fp = view (config . inboxFile) ctx
  case preview (fileLens fp) ctx of
    Nothing -> return ()
    Just tf -> do
      now <- liftIO getZonedTime
      let createdStr = T.pack $ "[" ++ formatTime defaultTimeLocale orgDayTimeFormat now ++ "]"
          dummyTask =
            Task
              { _level = 1
              , _todoKeyword = "INBOX"
              , _priority = Nothing
              , _title = "{{Title}}"
              , _tags = S.empty
              , _scheduled = Nothing
              , _deadline = Nothing
              , _closed = Nothing
              , _properties = [(orgCreatedProperty, createdStr)]
              , _description = ""
              }
          initialContent = write dummyTask

      suspendAndResume $ do
        editedMaybe <- editWithEditor initialContent
        case editedMaybe of
          Nothing -> return ctx
          Just editedStr ->
            let (l, _, result) =
                  runParser (view (config . taskParser) ctx) editedStr
             in case result of
                  ParserFailure err -> do
                    -- show parse error
                    _ <-
                      writeBChan (view (appState . eventChannel) ctx) $
                        Error (err ++ " at " ++ show (line l) ++ ":" ++ show (column l))
                    return ctx
                  ParserSuccess newTask
                    | newTask == dummyTask -> do
                        writeBChan (view (appState . eventChannel) ctx) $
                          Error "No changes detected; task not added."
                        return ctx
                    | otherwise -> do
                        -- append parsed task
                        let oldTasks = tf ^. content
                            idx = V.length oldTasks
                            updatedTf = tf & content .~ V.snoc oldTasks newTask
                            ptr = TaskPointer fp idx
                            ctx1 = set (fileLens fp) updatedTf ctx
                            ctx2 = over currentViewLens (`V.snoc` ptr) ctx1
                            ctx3 = set cursorLens (Just idx) ctx2
                        return ctx3
      adjustViewport

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
          let (l, _, result) = runParser (view (config . taskParser) ctx) editedStr
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

-- Needs to be specific type
addTag :: T.Text -> AppContext Task -> AppContext Task
addTag tag = over (currentTaskLens . tags) (S.insert tag)

-- Needs to be specific type
deleteTag :: T.Text -> AppContext Task -> AppContext Task
deleteTag tag = over (currentTaskLens . tags) (S.delete tag)

abortCmd :: AppContext a -> AppContext a
abortCmd = switchMode NormalMode . set (appState . cmdState) Nothing

removeLastIfExists :: T.Text -> T.Text
removeLastIfExists t
  | T.null t = t -- Return unchanged if empty
  | otherwise = T.dropEnd 1 t

cmdDeleteChar :: AppContext a -> AppContext a
cmdDeleteChar ctx =
  case view (appState . cmdState) ctx of
    Just (Typing cmdType input)
      | not (T.null input) ->
          over (appState . cmdState) (\_ -> Just (Typing cmdType (removeLastIfExists input))) ctx
    _ ->
      (switchMode NormalMode . set (appState . cmdState) Nothing) ctx

applySearch :: (Searcher a) => T.Text -> AppContext a -> AppContext a
applySearch query ctx =
  ( set currentViewLens newView
      . set cursorLens newCurTask
      . cleanCompactView
  )
    ctx
 where
  compView = view compactViewLens ctx
  start = view compactViewTaskStartIndex compView
  end = view compactViewTasksEndIndex compView
  cleanCompactView = set (compactViewLens . compactViewTaskStartIndex) 0 . set (compactViewLens . compactViewTasksEndIndex) (min (V.length newView - 1) (end - start + 1))
  oldView = view currentViewLens ctx
  fs = view fileStateLens ctx
  ptrToMatch q ptr = maybe False (matches q) (preview (taskBy ptr) fs)
  (newCurTask, newView) =
    let nv = V.filter (ptrToMatch query) oldView
     in if V.length nv > 0 then (Just 0, nv) else (Nothing, nv)

executeCommand :: (Searcher a, Writer a, Show a) => GlobalAppState a
executeCommand = do
  ctx <- get
  case view (appState . cmdState) ctx of
    Just (Typing cmdType cmd) ->
      case cmdType of
        Command -> do
          case T.strip cmd of
            "w" -> do
              saveAll
              let filesSavedCount = M.size $ view fileStateLens ctx
              let msg = T.pack $ show filesSavedCount <> if filesSavedCount == 1 then " file written" else " files written"
              modify $ set (appState . cmdState) (Just $ ShowingMessage msg)
            "w!" -> do
              forceWriteAll
              let filesSavedCount = M.size $ view fileStateLens ctx
              let msg = T.pack $ show filesSavedCount <> if filesSavedCount == 1 then " file written (forced)" else " files written (forced)"
              modify $ set (appState . cmdState) (Just $ ShowingMessage msg)
            "q" -> quit
            "q!" -> forceQuit
            "wq" -> do
              forceWriteAll
              let filesSavedCount = M.size $ view fileStateLens ctx
              let msg = T.pack $ show filesSavedCount <> if filesSavedCount == 1 then " file written (forced)" else " files written (forced)"
              modify $ set (appState . cmdState) (Just $ ShowingMessage msg)
              quit
            unknown -> do
              let msg = "E492: Not an editor command: " <> unknown
              modify $ set (appState . cmdState) (Just $ ShowingMessage msg)
        Search -> do
          saveForJump $ modify (applySearch (T.strip cmd))
          modify $ switchMode NormalMode . set (appState . cmdState) Nothing
    _ -> return ()

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

saveAll :: GlobalAppState a
saveAll = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) SaveAllFiles

forceWriteAll :: GlobalAppState a
forceWriteAll = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) ForceWriteAll

-- NOTE: this is a hack not to call `halt` immidiately so that it processes all
-- other events in the channel first, for e.g. saving files
quit :: GlobalAppState a
quit = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) QuitApp

forceQuit :: GlobalAppState a
forceQuit = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) ForceQuit

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

-- FIX: when there are 0 tasks with that keyword it falls into error
changeViewKeywordBinding :: T.Text -> String -> KeyBinding Task
changeViewKeywordBinding keyword bind =
  normalBinding
    (View keyword)
    (toKeySeq bind)
    (T.concat ["Show ", keyword, " tasks"])
    $ saveForJump (applyFilterToAllTasks (todoKeywordFilter keyword))

addTagBinding :: T.Text -> String -> KeyBinding Task
addTagBinding tag bind =
  KeyBinding
    (AddTag tag)
    (toKey bind)
    (T.concat ["Add tag `", tag, "` to the task"])
    (saveForUndo $ modify (addTag tag))
    (modeKeyContext NormalMode)

deleteTagBinding :: T.Text -> String -> KeyBinding Task
deleteTagBinding tag bind =
  KeyBinding
    (DeleteTag tag)
    (toKey bind)
    (T.concat ["Delete tag `", tag, "` from the task"])
    (saveForUndo $ modify (deleteTag tag))
    (modeKeyContext NormalMode)

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

cmdBinding :: (ToBinding k) => KeyEvent -> k -> T.Text -> GlobalAppState a -> KeyBinding a
cmdBinding = toBinding CmdMode

normalModeBindings :: [KeyBinding Task]
normalModeBindings =
  [ --------------------------------- Error Dialog -----------------------------
    KeyBinding ErrorDialogQuit (toKey KEsc) "Quit error dialog" proceedInErrorDialog errorDialogKeyContext
  , KeyBinding ErrorDialogAccept (toKey KEnter) "Accept selected option" proceedInErrorDialog errorDialogKeyContext
  , --------------------------------- Cmd Mode --------------------------------
    cmdBinding AbortCmd (toKey KEsc) "Abort command" $ modify abortCmd
  , cmdBinding CmdDeleteChar (toKey KBS) "Delete char" $ modify cmdDeleteChar
  , cmdBinding ApplyCmd (toKey KEnter) "Execute command" executeCommand
  , --------------------------------- Normal Mode -----------------------------
    -- Mode switching
    normalBinding SwitchToSearchMode '/' "Switch to search mode" $ modify $ \ctx -> (switchMode CmdMode . set (appState . cmdState) (Just $ Typing Search T.empty)) ctx
  , normalBinding SwitchToCmdMode ':' "Switch to command mode" $ modify $ \ctx -> (switchMode CmdMode . set (appState . cmdState) (Just $ Typing Command T.empty)) ctx
  , -- Movement
    normalBinding MoveUp 'k' "Move up" $ adjustCursor (\i -> i - 1)
  , normalBinding MoveUp KUp "Move up" $ adjustCursor (\i -> i - 1)
  , normalBinding MoveDown 'j' "Move down" $ adjustCursor (+ 1)
  , normalBinding MoveDown KDown "Move down" $ adjustCursor (+ 1)
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
  , -- Tags addition
    addTagBinding "music" "a,m"
  , deleteTagBinding "music" "d,m"
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
  , -- Macros
    normalBinding (Macro "Music") (toKeySeq "mm") "Macros for music entries" $
      saveForUndo $
        modify $
          addTag "music" . addTag "download" . changeTodoKeyword "LIST"
  , -- Other
    normalBinding AddTask (toKeySeq "at") "Add new task" $ saveForUndo addNewTask
  , normalBinding EditInEditor (toKey KEnter) "Edit in editor" $ saveForUndo editSelectedTaskInEditor
  , normalBinding OpenUrl (toKeySeq "gx") "Open URL in task" openTaskUrl
  ]
