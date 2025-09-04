{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Tui.Keybindings where

import Control.Monad (when)
import System.Process (callCommand)

import Brick
import qualified Brick.Types as BT
import Control.Applicative ((<|>))
import Control.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Graphics.Vty.Input.Events
import Text.Regex.Posix ((=~))
import Tui.Types
import Writer.Writer

import Brick.BChan
import Control.Monad.IO.Class (liftIO)
import Data.Char (isUpper, toLower)
import Data.Foldable (forM_)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Set as Set
import Data.Time (LocalTime (LocalTime), getZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (midnight)
import qualified Data.Vector as V
import qualified Model.LinearHistory as L
import Model.OrgMode (
  OrgTime (time),
  Task (..),
  TaskFile,
  content,
  level,
  orgCreatedProperty,
  orgDayTimeFormat,
  priority,
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
-- TODO: Should add an AI agent that would go through the processed tasks and tag them in the background, for it to be more consistent and accurate I can show a list of tags that are currently present and instruct to introduce new tags only if there are no corresponding ones. Also I can use searching functions to show him examples of tags that he chose for a particular task to double check his choices
-- TODO: Support working with projects

enterSelectionMode :: GlobalAppState Task
enterSelectionMode = do
  ctx <- get
  case view cursorLens ctx of
    Just cursor -> do
      modify $ switchMode SelectionMode
      modify $ set selectionLens (Set.singleton cursor)
      modify $ set selectionAnchorLens (Just cursor)
    Nothing -> return ()

toggleRangeSelection :: GlobalAppState Task
toggleRangeSelection = do
  ctx <- get
  case view cursorLens ctx of
    Just cursor -> do
      case view selectionAnchorLens ctx of
        Just anchor -> do
          let range = Set.fromList [min anchor cursor .. max anchor cursor]
              currentSelection = view selectionLens ctx
              newSelection = Set.union currentSelection range
          modify $ set selectionLens newSelection
          modify $ set selectionAnchorLens (Just cursor)
        Nothing -> do
          let currentSelection = view selectionLens ctx
              newSelection = Set.insert cursor currentSelection
          modify $ set selectionLens newSelection
          modify $ set selectionAnchorLens (Just cursor)
    Nothing -> return ()

toggleCurrentSelection :: GlobalAppState Task
toggleCurrentSelection = do
  ctx <- get
  case view cursorLens ctx of
    Just cursor -> do
      let currentSelection = view selectionLens ctx
      let newSelection =
            if Set.member cursor currentSelection
              then Set.delete cursor currentSelection
              else Set.insert cursor currentSelection
      modify $ set selectionLens newSelection
    Nothing -> return ()

exitSelectionMode :: GlobalAppState Task
exitSelectionMode = do
  modify $ switchMode NormalMode
  modify $ set selectionLens Set.empty
  modify $ set selectionAnchorLens Nothing

applyToSelection :: (Task -> Task) -> GlobalAppState Task
applyToSelection action = do
  ctx <- get
  let selectedIndices = Set.toList $ view selectionLens ctx
      cv = view currentViewLens ctx
      selectedPtrs = mapMaybe (cv V.!?) selectedIndices
  mapM_ (\ptr -> modify $ over (fileStateLens . taskBy ptr) action) selectedPtrs

smartApplyTodoKeyword :: T.Text -> GlobalAppState Task
smartApplyTodoKeyword keyword = do
  ctx <- get
  let selection = view selectionLens ctx
  if Set.null selection
    then modify (changeTodoKeyword keyword)
    else applyToSelection (set todoKeyword keyword)

smartApplyTagAction :: (S.Set T.Text -> S.Set T.Text) -> GlobalAppState Task
smartApplyTagAction tagAction = do
  ctx <- get
  let selection = view selectionLens ctx
  if Set.null selection
    then modify (over (currentTaskLens . tags) tagAction)
    else applyToSelection (over tags tagAction)

selectionAwareMove :: (Int -> Int) -> GlobalAppState Task
selectionAwareMove moveFunc = do
  ctx <- get
  case view (appState . appMode) ctx of
    SelectionMode -> do
      adjustCursor moveFunc
      newCtx <- get
      case (view selectionAnchorLens newCtx, view cursorLens newCtx) of
        (Just anchor, Just newCursor) -> do
          let range = Set.fromList [min anchor newCursor .. max anchor newCursor]
          modify $ set selectionLens range
        _ -> return ()
    _ -> adjustCursor moveFunc

extractFirstUrl :: T.Text -> Maybe T.Text
extractFirstUrl text =
  let bs = TE.encodeUtf8 text
      orgPattern, urlPattern :: B.ByteString
      orgPattern = "\\[\\[(https?://[^ \t\n\\]]+)\\]\\]"
      urlPattern = "(https?://[^ \t\n]+)"
      orgMatches :: [[B.ByteString]] = bs =~ orgPattern
      urlMatches :: [[B.ByteString]] = bs =~ urlPattern
   in case orgMatches of
        ((_ : url : _) : _) -> Just $ TE.decodeUtf8 url
        _ -> case urlMatches of
          ((url : _) : _) -> Just $ TE.decodeUtf8 url
          _ -> Nothing

applySorter :: (Task -> Task -> Ordering) -> GlobalAppState Task
applySorter sorter = do
  modify $ set viewSorterLens sorter
  modify $ set cursorLens (Just 0)
  modify $ set (compactViewLens . viewportStart) 0

veryOldTime :: LocalTime
veryOldTime = read "1970-01-01 00:00:00"

sortByPriorityAsc :: Task -> Task -> Ordering
sortByPriorityAsc = comparing getPriority
 where
  getPriority t = fromMaybe maxBound (_priority t)

sortByPriorityDesc :: Task -> Task -> Ordering
sortByPriorityDesc t1 t2 = comparing getPriority t2 t1
 where
  getPriority t = fromMaybe maxBound (_priority t)

sortByCreatedAsc :: Task -> Task -> Ordering
sortByCreatedAsc = comparing getCreated
 where
  getCreated t = case fmap time (_createdProp t) of
    Nothing -> veryOldTime
    Just (Left day) -> LocalTime day midnight
    Just (Right lt) -> lt

sortByCreatedDesc :: Task -> Task -> Ordering
sortByCreatedDesc t1 t2 =
  comparing getCreated t2 t1
 where
  getCreated t = case fmap time (_createdProp t) of
    Nothing -> veryOldTime
    Just (Left day) -> LocalTime day midnight
    Just (Right lt) -> lt

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
        Just url -> liftIO $ openUrlInBrowser url
        Nothing -> return ()
    Nothing -> return ()

adjustViewport :: GlobalAppState a
adjustViewport = do
  mExt <- lookupExtent CompactViewWidget
  case mExt of
    Nothing -> return ()
    Just (Extent _ _ (w, h)) -> do
      ctx <- get
      let margin = view (config . scrollingMargin) ctx
          viewSize = V.length $ view currentViewLens ctx
          viewStartOld = view (compactViewLens . viewportStart) ctx

      case view cursorLens ctx of
        Nothing -> return ()
        Just cursor -> do
          let vStartNew
                | cursor < viewStartOld + margin = max 0 (cursor - margin)
                | cursor >= viewStartOld + h - margin = min (viewSize - h) (cursor - h + 1 + margin)
                | otherwise = viewStartOld

              vFinalStart = max 0 (min (viewSize - h) vStartNew)

          when (vFinalStart /= viewStartOld) $
            modify $
              set (compactViewLens . viewportStart) vFinalStart

adjustCursor :: (Int -> Int) -> GlobalAppState a
adjustCursor f = do
  ctx <- get
  let cv = view currentViewLens ctx
  let modifyCursor c = clamp 0 (length cv - 1) (f c)
  let currentCursor = view cursorLens ctx
  let newCursor = fmap modifyCursor currentCursor
  modify $ set cursorLens newCursor
  adjustViewport

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
              , _createdProp = Nothing
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
                  runParser (view (system . taskParser) ctx) editedStr
             in case result of
                  ParserFailure err -> do
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
                        let oldTasks = tf ^. content
                            idx = V.length oldTasks
                            updatedTf = tf & content .~ V.snoc oldTasks newTask
                            ptr = TaskPointer fp idx
                            ctx1 = set (fileLens fp) updatedTf ctx
                            ctx2 = set cursorLens (Just idx) ctx1
                        return ctx2

editSelectedTaskInEditor :: (Writer a) => GlobalAppState a
editSelectedTaskInEditor = do
  ctx <- get
  let ct = preview currentTaskLens ctx
  let maybePtr = view currentTaskPtr ctx
  case (ct, maybePtr) of
    (Just task, Just ptr) -> suspendAndResume $ do
      editedContent <- editWithEditor (write task)
      when (null editedContent) $ return ()
      case editedContent of
        Nothing -> return ctx
        Just editedStr -> do
          let (l, _, result) = runParser (view (system . taskParser) ctx) editedStr
          case result of
            ParserSuccess t -> return $ set (fileStateLens . taskBy ptr) t ctx
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

changeTodoKeyword :: T.Text -> AppContext Task -> AppContext Task
changeTodoKeyword keyword = over (currentTaskLens . todoKeyword) (const keyword)

addTag :: T.Text -> AppContext Task -> AppContext Task
addTag tag = over (currentTaskLens . tags) (S.insert tag)

upPriority :: AppContext Task -> AppContext Task
upPriority = over (currentTaskLens . priority) f
 where
  f Nothing = Just 1
  f (Just 0) = Nothing
  f (Just x) = Just (x - 1 `mod` 3)

downPriority :: AppContext Task -> AppContext Task
downPriority = over (currentTaskLens . priority) f
 where
  f Nothing = Just 1
  f (Just 2) = Nothing
  f (Just x) = Just (x + 1 `mod` 3)

deleteTag :: T.Text -> AppContext Task -> AppContext Task
deleteTag tag = over (currentTaskLens . tags) (S.delete tag)

abortCmd :: AppContext a -> AppContext a
abortCmd = switchMode NormalMode . set (appState . cmdState) Nothing

removeLastIfExists :: T.Text -> T.Text
removeLastIfExists t
  | T.null t = t
  | otherwise = T.dropEnd 1 t

cmdDeleteChar :: AppContext a -> AppContext a
cmdDeleteChar ctx =
  case view (appState . cmdState) ctx of
    Just (Typing cmdType input)
      | not (T.null input) ->
          over (appState . cmdState) (\_ -> Just (Typing cmdType (removeLastIfExists input))) ctx
    _ ->
      (switchMode NormalMode . set (appState . cmdState) Nothing) ctx

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
        Search -> saveForJump $ do
          ctx <- get
          modify $ over viewFilterLens ((matches $ T.strip cmd) :)
          modify $ set cursorLens (Just 0)
          modify $ switchMode NormalMode . set (appState . cmdState) Nothing
    _ -> return ()

applyFilterToAllTasks :: (a -> Bool) -> GlobalAppState a
applyFilterToAllTasks predicate = do
  ctx <- get
  modify $ set viewFilterLens [predicate]
  modify $ set cursorLens (Just 0)
  modify $ set (compactViewLens . viewportStart) 0

todoKeywordFilter :: T.Text -> Task -> Bool
todoKeywordFilter keyword task = view todoKeyword task == keyword

-- Project hierarchy functions

-- | Find the project that contains the given task
getProjectForTask :: TaskPointer -> FileState Task -> Maybe TaskPointer
getProjectForTask taskPtr fs =
  case preview (ix (view file taskPtr) . success . content) fs of
    Nothing -> Nothing
    Just tasks ->
      let taskIdx = view taskIndex taskPtr
          targetLevel = maybe 0 (view level) (tasks V.!? taskIdx)
          findProjectIndex idx
            | idx <= 0 = Nothing
            | otherwise =
                case tasks V.!? (idx - 1) of
                  Just task
                    | view level task < targetLevel && isProjectTask task ->
                        Just (idx - 1)
                  _ -> findProjectIndex (idx - 1)
       in fmap (TaskPointer (view file taskPtr)) (findProjectIndex taskIdx)

-- | Get all subtasks that belong to a project
getProjectSubtasks :: TaskPointer -> FileState Task -> [TaskPointer]
getProjectSubtasks projectPtr fs =
  case preview (ix (view file projectPtr) . success . content) fs of
    Nothing -> []
    Just tasks ->
      let projectIdx = view taskIndex projectPtr
          projectLevel = maybe 0 (view level) (tasks V.!? projectIdx)
          tasksAfterProject = V.drop (projectIdx + 1) tasks
          subtaskIndices = V.takeWhile (\task -> view level task > projectLevel) tasksAfterProject
       in [ TaskPointer (view file projectPtr) (projectIdx + 1 + i)
          | i <- [0 .. V.length subtaskIndices - 1]
          ]

-- | Check if a task is a project (has PROJECT keyword)
isProjectTask :: Task -> Bool
isProjectTask = todoKeywordFilter "PROJECTS"

-- | Get all project pointers from all files
getAllProjectPointers :: FileState Task -> [TaskPointer]
getAllProjectPointers fs =
  [ TaskPointer fp idx
  | (fp, result) <- M.toList fs
  , taskFile <- maybe [] pure (resultToMaybe result)
  , (idx, task) <- zip [0 ..] (V.toList $ view content taskFile)
  , isProjectTask task
  ]

-- | Open refile dialog to select a project
openRefileDialog :: GlobalAppState Task
openRefileDialog = do
  ctx <- get
  let fs = view fileStateLens ctx
      allProjects = getAllProjectPointers fs
  case allProjects of
    [] -> return () -- No projects found
    _ -> do
      let dialog = RefileDialog
            { _rdProjects = allProjects
            , _rdSearchQuery = ""
            , _rdSelectedIndex = 0
            }
      modify $ set (appState . refileDialog) (Just dialog)

-- | Jump to project view for the current task
goToProjectView :: GlobalAppState Task
goToProjectView = do
  ctx <- get
  let fs = view fileStateLens ctx
      maybeProjectPtr = do
        currentPtr <- view currentTaskPtr ctx
        getProjectForTask currentPtr fs
  case maybeProjectPtr of
    Nothing -> return ()
    Just projPtr ->
      saveForJump $ do
        let projectSubtasks = getProjectSubtasks projPtr fs
            -- Custom sorter that puts project task first, then sorts subtasks normally
            originalSorter = view viewSorterLens ctx
            projectViewSorter task1 task2 =
              case ( Just task1 == preview (taskBy projPtr) fs
                   , Just task2 == preview (taskBy projPtr) fs
                   ) of
                (True, False) -> LT -- task1 is project, comes first
                (False, True) -> GT -- task2 is project, comes first
                _ -> originalSorter task1 task2
            projectAndSubtasksFilter task =
              -- Include the project task itself
              (Just task == preview (taskBy projPtr) fs)
                ||
                -- Include all subtasks
                any
                  ( \subtaskPtr ->
                      Just task == preview (taskBy subtaskPtr) fs
                  )
                  projectSubtasks
        modify $ set viewFilterLens [projectAndSubtasksFilter]
        modify $ set viewSorterLens projectViewSorter
        modify $ set cursorLens (Just 0)
        modify $ set (compactViewLens . viewportStart) 0

saveAll :: GlobalAppState a
saveAll = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) SaveAllFiles

forceWriteAll :: GlobalAppState a
forceWriteAll = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) ForceWriteAll

quit :: GlobalAppState a
quit = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) QuitApp

forceQuit :: GlobalAppState a
forceQuit = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) ForceQuit

----------------------- Bindings ----------------------------

errorDialogKeyContext :: AppContext a -> Bool
errorDialogKeyContext = isJust . view (appState . errorDialog)

modeKeyContext :: AppMode a -> AppContext a -> Bool
modeKeyContext mode ctx = view (appState . appMode) ctx == mode && not (errorDialogKeyContext ctx)

selectionModeKeyContext :: AppContext a -> Bool
selectionModeKeyContext ctx = view (appState . appMode) ctx == SelectionMode && not (errorDialogKeyContext ctx)

anyModeKeyContext :: [AppMode a] -> AppContext a -> Bool
anyModeKeyContext modes ctx =
  let currentMode = view (appState . appMode) ctx
   in currentMode `elem` modes && not (errorDialogKeyContext ctx)

class WithMod a where
  withMod :: a -> Modifier -> NonEmpty KeyPress

instance WithMod Char where
  withMod c m = pure $ KeyPress (KChar $ toLower c) withShift
   where
    withShift = if isUpper c then S.fromList [MShift, m] else S.singleton m

instance WithMod Key where
  withMod c m = pure $ KeyPress c (S.singleton m)

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

instance ToBind (NonEmpty KeyPress) where
  toKey = id

toKeySeq :: String -> NonEmpty KeyPress
toKeySeq s = fromList s >>= toKey

changeTodoKeywordBinding :: T.Text -> String -> KeyBinding Task
changeTodoKeywordBinding keyword bind =
  KeyBinding
    (ChangeTodoKeyword keyword)
    (toKey bind)
    (T.concat ["Change todo keyword to ", keyword])
    (saveForUndo $ smartApplyTodoKeyword keyword)
    normalOrSelectionContext

changeViewKeywordBinding :: T.Text -> String -> Maybe (Task -> Task -> Ordering) -> KeyBinding Task
changeViewKeywordBinding keyword bind sorter =
  normalBinding
    (View keyword)
    (toKeySeq bind)
    (T.concat ["Show ", keyword, " tasks"])
    $ do
      saveForJump (applyFilterToAllTasks (todoKeywordFilter keyword))
      forM_ sorter applySorter

normalBinding :: (ToBind k) => KeyEvent -> k -> T.Text -> GlobalAppState a -> KeyBinding a
normalBinding event bind desc action = KeyBinding event (toKey bind) desc action (modeKeyContext NormalMode)

cmdBinding :: (ToBind k) => KeyEvent -> k -> T.Text -> GlobalAppState a -> KeyBinding a
cmdBinding event bind desc action = KeyBinding event (toKey bind) desc action (modeKeyContext CmdMode)

normalOrSelectionContext :: AppContext a -> Bool
normalOrSelectionContext = anyModeKeyContext [NormalMode, SelectionMode]

addTagKeybinding tag shortcut = KeyBinding (AddTag tag) (toKeySeq shortcut) (T.concat ["Add ", tag, " tag"]) (saveForUndo $ smartApplyTagAction (S.insert tag)) normalOrSelectionContext

deleleTagKeybinding tag shortcut = KeyBinding (DeleteTag tag) (toKeySeq shortcut) (T.concat ["Delete ", tag, " tag"]) (saveForUndo $ smartApplyTagAction (S.delete tag)) normalOrSelectionContext

orgKeyBindings :: [KeyBinding Task]
orgKeyBindings =
  [ --------------------------------- Error Dialog -----------------------------
    KeyBinding ErrorDialogQuit (toKey KEsc) "Quit error dialog" proceedInErrorDialog errorDialogKeyContext
  , KeyBinding ErrorDialogAccept (toKey KEnter) "Accept selected option" proceedInErrorDialog errorDialogKeyContext
  , --------------------------------- Cmd Mode --------------------------------
    cmdBinding AbortCmd (toKey KEsc) "Abort command" $ modify abortCmd
  , cmdBinding CmdDeleteChar (toKey KBS) "Delete char" $ modify cmdDeleteChar
  , cmdBinding ApplyCmd (toKey KEnter) "Execute command" executeCommand
  , --------------------------------- Selection Mode ---------------------------
    -- Enter/exit selection mode
    normalBinding EnterSelectionMode (withMod 'v' MShift) "Enter selection mode" enterSelectionMode
  , normalBinding ToggleCurrentSelection 'v' "Toggle current item selection" toggleCurrentSelection
  , KeyBinding ExitSelectionMode (toKey KEsc) "Exit selection mode" exitSelectionMode selectionModeKeyContext
  , KeyBinding ToggleRangeSelection (withMod 'v' MShift) "Toggle range selection" toggleRangeSelection selectionModeKeyContext
  , KeyBinding ToggleCurrentSelection (toKey 'v') "Toggle current selection" toggleCurrentSelection selectionModeKeyContext
  , ---------------------------------------- Normal Mode  ----------------------
    normalBinding SwitchToSearchMode (toKey '/') "Switch to search mode" (modify $ \ctx -> (switchMode CmdMode . set (appState . cmdState) (Just $ Typing Search T.empty)) ctx)
  , normalBinding SwitchToCmdMode (toKey ':') "Switch to command mode" (modify $ \ctx -> (switchMode CmdMode . set (appState . cmdState) (Just $ Typing Command T.empty)) ctx)
  , -- KeyBuffer
    normalBinding CleanKeyState KEsc "Clean key state" $ modify $ set (appState . keyState) NoInput
  , -- History
    normalBinding Undo (toKey 'u') "Undo" (modify undo)
  , normalBinding Redo (withMod 'r' MCtrl) "Redo" (modify redo)
  , normalBinding UpPriority (withMod KUp MShift) "Priority Up" (saveForUndo $ modify upPriority)
  , normalBinding DownPriority (withMod KDown MShift) "Priority Down" (saveForUndo $ modify downPriority)
  , --------------------------------- Normal/Selection Mode (shared) -----------
    -- Movement
    KeyBinding MoveUp (toKey 'k') "Move up" (selectionAwareMove (\i -> i - 1)) normalOrSelectionContext
  , KeyBinding MoveUp (toKey KUp) "Move up" (selectionAwareMove (\i -> i - 1)) normalOrSelectionContext
  , KeyBinding MoveDown (toKey 'j') "Move down" (selectionAwareMove (+ 1)) normalOrSelectionContext
  , KeyBinding MoveDown (toKey KDown) "Move down" (selectionAwareMove (+ 1)) normalOrSelectionContext
  , KeyBinding JumpEnd (toKey 'G') "Jump to the end" (saveForJump $ selectionAwareMove (const maxBound)) normalOrSelectionContext
  , KeyBinding JumpBeginning (toKeySeq "gg") "Jump to the beginning" (saveForJump $ selectionAwareMove (const 0)) normalOrSelectionContext
  , KeyBinding JumpBackward (withMod 'o' MCtrl) "Jump backward" (modify jumpBack) normalOrSelectionContext
  , KeyBinding JumpForward (toKey '\t') "Jump forward" (modify jumpForward) normalOrSelectionContext -- NOTE: Terminals translate Ctrl-i into TAB
  , -- Todo Keywords (smart: apply to selection if in selection mode, current task if in normal mode)
    -- TODO: create a config field for specifying TODO keywords and their shortcuts
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
  , -- Tags (smart: apply to selection if there are selected items, current task otherwise)
    -- TODO: create a config field for specifying tags and their shortcuts
    addTagKeybinding "music" "a,m"
  , deleleTagKeybinding "music" "d,m"
  , addTagKeybinding "cool" "a,c"
  , deleleTagKeybinding "cool" "d,c"
  , addTagKeybinding "software" "a,s"
  , deleleTagKeybinding "software" "d,s"
  , addTagKeybinding "book" "a,b"
  , deleleTagKeybinding "book" "d,b"
  , -- Macros
    KeyBinding
      (Macro "Music")
      (toKeySeq "mm")
      "Macros for music entries"
      ( saveForUndo $ do
          smartApplyTagAction (S.insert "music")
          smartApplyTagAction (S.insert "download")
          smartApplyTodoKeyword "LIST"
      )
      normalOrSelectionContext
  , -- Views
    normalBinding (View "all") (toKeySeq " aa") "Show all tasks" $ saveForJump $ applyFilterToAllTasks (const True)
  , changeViewKeywordBinding "INBOX" " ai" $ Just sortByCreatedDesc
  , changeViewKeywordBinding "RELEVANT" " ar" $ Just sortByPriorityAsc
  , changeViewKeywordBinding "SOMEDAY" " as" Nothing
  , changeViewKeywordBinding "NOTES" " an" Nothing
  , changeViewKeywordBinding "LIST" " al" Nothing
  , changeViewKeywordBinding "WAITING" " aw" $ Just sortByPriorityAsc
  , changeViewKeywordBinding "PROJECTS" " ap" $ Just sortByPriorityAsc
  , changeViewKeywordBinding "TODO" " at" $ Just sortByPriorityAsc
  , changeViewKeywordBinding "DONE" " ad" Nothing
  , changeViewKeywordBinding "TRASH" " ax" Nothing
  , -- Sorting
    normalBinding SortCreatedAsc (toKeySeq "sca") "Sort by created (asc)" $ saveForJump (applySorter sortByCreatedAsc)
  , normalBinding SortCreatedDesc (toKeySeq "scd") "Sort by created (desc)" $ saveForJump (applySorter sortByCreatedDesc)
  , normalBinding SortPriorityAsc (toKeySeq "sca") "Sort by priority (asc)" $ saveForJump (applySorter sortByPriorityAsc)
  , normalBinding SortPriorityDesc (toKeySeq "scd") "Sort by priority (desc)" $ saveForJump (applySorter sortByPriorityDesc)
  , -- Other
    normalBinding AddTask (toKeySeq "at") "Add new task" $ saveForUndo addNewTask
  , normalBinding EditInEditor (toKey KEnter) "Edit in editor" $ saveForUndo editSelectedTaskInEditor
  , normalBinding OpenUrl (toKeySeq "gx") "Open URL in task" openTaskUrl
  , normalBinding GoToProject (toKeySeq "gp") "Go to project view" goToProjectView
  , normalBinding Refile (toKeySeq "gr") "Refile task to project" openRefileDialog
  ]
