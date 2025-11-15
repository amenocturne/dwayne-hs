{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Tui.Keybindings where

import Control.Monad (when)
import Control.Exception (catch, IOException)
import System.Process (spawnProcess, waitForProcess)

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
import qualified Core.Operations as Ops
import Core.Types (TaskPointer (..), FileState, file)
import Model.OrgMode (Task, orgProjectKeyword, orgTrashKeyword, orgInboxKeyword, orgDoneKeyword, orgTodoKeyword, orgRelevantKeyword, orgSomedayKeyword, orgNotesKeyword, orgListKeyword, orgWaitingKeyword)
import Refile.Refile (refileTaskToProject)
import Refile.OrgRefileable () -- Import Refileable instance for Task
import qualified Validation.SystemValidation as SV
import Brick.Widgets.Dialog (dialog, dialogSelection)
import qualified Commands.Command as Cmd
import qualified Commands.Registry as Registry
import qualified Commands.Projects as CmdProjects

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
import Data.Time.Calendar (fromGregorian)
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

-- Get pointers for currently selected tasks
getSelectedTaskPointers :: AppContext Task -> [TaskPointer]
getSelectedTaskPointers ctx =
  let selection = view selectionLens ctx
      cv = view currentViewLens ctx
      selectedIndices = Set.toList selection
  in mapMaybe (cv V.!?) selectedIndices

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
      needsValidation = keyword == orgProjectKeyword
      getCurrentKeywords = if Set.null selection
        then case view currentTaskPtr ctx of
               Just ptr -> maybe [] (pure . view todoKeyword) $ preview (taskBy ptr) (view fileStateLens ctx)
               Nothing -> []
        else let selectedTasks = getSelectedTaskPointers ctx
                 fs = view fileStateLens ctx
             in mapMaybe (\ptr -> preview (taskBy ptr . todoKeyword) fs) selectedTasks
      
      oldKeywords = getCurrentKeywords
      oldHasProject = orgProjectKeyword `elem` oldKeywords
      newHasProject = keyword == orgProjectKeyword
      
  if Set.null selection
    then modify (changeTodoKeyword keyword)
    else applyToSelection (set todoKeyword keyword)
    
  when (needsValidation || oldHasProject) $ do
    newCtx <- get
    let issues = SV.validateSystem newCtx
    case issues of
      [] -> return ()
      (issue:_) -> do
        let message = T.unpack (SV.issueDescription issue)
            dlg = ValidationDialog
                  { _vdDialog = dialog (Just $ str "Validation") Nothing 60
                  , _vdMisplacedTasks = SV.affectedItems issue
                  , _vdMessage = message ++ " (Enter to accept, Esc to cancel)"
                  }
        liftIO $ writeBChan (view (appState . eventChannel) newCtx) $ ValidationDialogCreated dlg

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
veryOldTime = LocalTime (fromGregorian 1970 1 1) midnight

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

openUrlInBrowser :: T.Text -> IO (Either String ())
openUrlInBrowser url = catch tryOpen handleError
  where
    tryOpen = do
      -- Use spawnProcess to avoid shell injection - passes URL as argument directly
      ph <- spawnProcess "open" [T.unpack url]
      _ <- waitForProcess ph
      return $ Right ()

    handleError :: IOException -> IO (Either String ())
    handleError e = return $ Left $ unlines
      [ "Failed to open URL in browser"
      , "URL: " ++ T.unpack url
      , "Reason: " ++ show e
      ]

-- TODO: If task contains multiple URLs, show a popup menu to let user choose which one to open
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
          result <- liftIO $ openUrlInBrowser url
          case result of
            Left err -> liftIO $ writeBChan (view (appState . eventChannel) ctx) $ Error err
            Right () -> return ()
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
          -- Calculate the maximum allowed viewport start position
          -- If viewport is larger than content, maxViewStart is 0
          let maxViewStart = max 0 (viewSize - h)

          let vStartNew
                -- Scrolling up: keep cursor visible with margin from top
                | cursor < viewStartOld + margin = max 0 (cursor - margin)
                -- Scrolling down: keep cursor visible with margin from bottom
                | cursor >= viewStartOld + h - margin =
                    min maxViewStart (cursor - h + 1 + margin)
                -- Cursor is within visible area with margin: don't scroll
                | otherwise = viewStartOld

              -- Ensure viewport start is within valid bounds
              vFinalStart = max 0 (min maxViewStart vStartNew)

          when (vFinalStart /= viewStartOld) $
            modify $
              set (compactViewLens . viewportStart) vFinalStart

adjustCursor :: (Int -> Int) -> GlobalAppState a
adjustCursor f = do
  ctx <- get
  let cv = view currentViewLens ctx
  let currentCursor = view cursorLens ctx
  -- If view is empty, cursor should be Nothing
  let newCursor =
        if null cv
        then Nothing
        else fmap (\c -> clamp 0 (length cv - 1) (f c)) currentCursor
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
              , _todoKeyword = orgInboxKeyword
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
        editResult <- editWithEditor initialContent
        case editResult of
          Left err -> do
            writeBChan (view (appState . eventChannel) ctx) $ Error err
            return ctx
          Right Nothing -> return ctx  -- User cancelled
          Right (Just editedStr) ->
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
      editResult <- editWithEditor (write task)
      case editResult of
        Left err -> do
          writeBChan (view (appState . eventChannel) ctx) $ Error err
          return ctx
        Right Nothing -> return ctx  -- User cancelled
        Right (Just editedStr) -> do
          let (l, _, result) = runParser (view (system . taskParser) ctx) editedStr
          case result of
            ParserSuccess t -> return $ set (fileStateLens . taskBy ptr) t ctx
            ParserFailure e -> do
              _ <- writeBChan (view (appState . eventChannel) ctx) $ Error (e ++ " at " ++ show (line l) ++ ":" ++ show (column l))
              return ctx
    _ -> return ()

proceedInErrorDialog :: (Writer a) => GlobalAppState a
proceedInErrorDialog = modify (over (appState . errorDialog) (const Nothing))

acceptValidation :: GlobalAppState Task
acceptValidation = do
  ctx <- get
  case view (appState . validationDialog) ctx of
    Just dialog -> do
      -- Accept validation: move PROJECT tasks with subtasks to projects file
      let misplacedTasks = view vdMisplacedTasks dialog
          projectsFilePath = view (config . projectsFile) ctx
      
      -- Move all misplaced PROJECT tasks with their subtasks to projects file
      -- Process each PROJECT task separately to preserve their hierarchies
      mapM_ (moveProjectTaskWithSubtasks projectsFilePath) misplacedTasks
      
      modify $ set (appState . validationDialog) Nothing
    Nothing -> return ()

-- | Move a PROJECT task and all its subtasks to the projects file, preserving hierarchy
moveProjectTaskWithSubtasks :: FilePath -> TaskPointer -> GlobalAppState Task
moveProjectTaskWithSubtasks projectsFilePath taskPtr = do
  ctx <- get
  let fs = view fileStateLens ctx
  case preview (taskBy taskPtr) fs of
    Just projectTask -> do
      -- Get all subtasks for this PROJECT task
      let subtasks = Ops.getProjectSubtasks taskPtr fs
          allTasksToMove = taskPtr : subtasks
          projectLevel = view level projectTask
          
      -- Calculate level adjustments to preserve relative hierarchy
      -- All tasks will be adjusted so the PROJECT task becomes level 1 (top-level in projects file)
      let levelAdjustment = 1 - projectLevel
      
      -- Apply level adjustment to all tasks and move them
      case preview (ix projectsFilePath . success) fs of
        Just projectTaskFile -> do
          let currentTasks = view content projectTaskFile
          
          adjustedTasks <- mapM (\ptr -> do
            currentCtx <- get
            let currentFs = view fileStateLens currentCtx
            case preview (taskBy ptr) currentFs of
              Just task -> do
                let adjustedTask = over level (+ levelAdjustment) task
                when (view file ptr /= projectsFilePath) $ 
                  modify $ over (fileStateLens . taskBy ptr . todoKeyword) (const orgTrashKeyword)
                return $ Just adjustedTask
              Nothing -> return Nothing
            ) allTasksToMove
          
          let validTasks = mapMaybe id adjustedTasks
              newTasks = currentTasks V.++ V.fromList validTasks
              updatedProjectFile = set content newTasks projectTaskFile
          
          modify $ set (fileStateLens . ix projectsFilePath . success) updatedProjectFile
          
        Nothing -> return () -- Projects file not found
    Nothing -> return ()

rejectValidation :: (Writer a) => GlobalAppState a
rejectValidation = modify (over (appState . validationDialog) (const Nothing))

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
        Search -> CmdProjects.saveForJump $ do
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

-- | Open refile dialog to select a project
openRefileDialog :: GlobalAppState Task
openRefileDialog = do
  ctx <- get
  let fs = view fileStateLens ctx
      allProjects = Ops.getAllProjects fs
  case allProjects of
    [] -> return () -- No projects found
    _ -> do
      let dialog = RefileDialog
            { _rdProjects = allProjects
            , _rdSearchQuery = ""
            , _rdSelectedIndex = 0
            }
      modify $ set (appState . refileDialog) (Just dialog)

-- | Refresh the current view by recomputing the cached view without changing the ViewSpec
refreshCurrentView :: GlobalAppState a
refreshCurrentView = do
  ctx <- get
  let newCache = recomputeCurrentView ctx
  modify $ set (compactViewLens . cachedView) newCache

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

validationDialogKeyContext :: AppContext a -> Bool
validationDialogKeyContext = isJust . view (appState . validationDialog)

modeKeyContext :: AppMode a -> AppContext a -> Bool
modeKeyContext mode ctx = view (appState . appMode) ctx == mode && not (errorDialogKeyContext ctx) && not (validationDialogKeyContext ctx)

selectionModeKeyContext :: AppContext a -> Bool
selectionModeKeyContext ctx = view (appState . appMode) ctx == SelectionMode && not (errorDialogKeyContext ctx) && not (validationDialogKeyContext ctx)

anyModeKeyContext :: [AppMode a] -> AppContext a -> Bool
anyModeKeyContext modes ctx =
  let currentMode = view (appState . appMode) ctx
   in currentMode `elem` modes && not (errorDialogKeyContext ctx) && not (validationDialogKeyContext ctx)

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
      CmdProjects.saveForJump (applyFilterToAllTasks (todoKeywordFilter keyword))
      forM_ sorter applySorter

normalBinding :: (ToBind k) => KeyEvent -> k -> T.Text -> GlobalAppState a -> KeyBinding a
normalBinding event bind desc action = KeyBinding event (toKey bind) desc action (modeKeyContext NormalMode)

cmdBinding :: (ToBind k) => KeyEvent -> k -> T.Text -> GlobalAppState a -> KeyBinding a
cmdBinding event bind desc action = KeyBinding event (toKey bind) desc action (modeKeyContext CmdMode)

normalOrSelectionContext :: AppContext a -> Bool
normalOrSelectionContext = anyModeKeyContext [NormalMode, SelectionMode]

addTagKeybinding tag shortcut = KeyBinding (AddTag tag) (toKeySeq shortcut) (T.concat ["Add ", tag, " tag"]) (saveForUndo $ smartApplyTagAction (S.insert tag)) normalOrSelectionContext

deleleTagKeybinding tag shortcut = KeyBinding (DeleteTag tag) (toKeySeq shortcut) (T.concat ["Delete ", tag, " tag"]) (saveForUndo $ smartApplyTagAction (S.delete tag)) normalOrSelectionContext

-- | Convert a Command to a KeyBinding
commandToKeyBinding :: Cmd.Command Task -> KeyBinding Task
commandToKeyBinding cmd =
  KeyBinding
    (Cmd.cmdKeyEvent cmd)
    (Cmd.cmdTuiKeybinding cmd)
    (Cmd.cmdTuiDescription cmd)
    (Cmd.cmdAction cmd)
    (Cmd.cmdContext cmd)

orgKeyBindings :: [KeyBinding Task]
orgKeyBindings =
  [ --------------------------------- Error Dialog -----------------------------
    KeyBinding ErrorDialogQuit (toKey KEsc) "Quit error dialog" proceedInErrorDialog errorDialogKeyContext
  , KeyBinding ErrorDialogAccept (toKey KEnter) "Accept selected option" proceedInErrorDialog errorDialogKeyContext
  , ----------------------------- Validation Dialog ---------------------------
    KeyBinding ValidationDialogAccept (toKey KEnter) "Move PROJECT tasks to projects file" acceptValidation validationDialogKeyContext
  , KeyBinding ValidationDialogReject (toKey KEsc) "Skip moving PROJECT tasks" rejectValidation validationDialogKeyContext
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
  , KeyBinding JumpEnd (toKey 'G') "Jump to the end" (CmdProjects.saveForJump $ selectionAwareMove (const maxBound)) normalOrSelectionContext
  , KeyBinding JumpBeginning (toKeySeq "gg") "Jump to the beginning" (CmdProjects.saveForJump $ selectionAwareMove (const 0)) normalOrSelectionContext
  , KeyBinding JumpBackward (withMod 'o' MCtrl) "Jump backward" (modify jumpBack) normalOrSelectionContext
  , KeyBinding JumpForward (toKey '\t') "Jump forward" (modify jumpForward) normalOrSelectionContext -- NOTE: Terminals translate Ctrl-i into TAB
  , -- Todo Keywords (smart: apply to selection if in selection mode, current task if in normal mode)
    -- TODO: create a config field for specifying TODO keywords and their shortcuts
    changeTodoKeywordBinding orgInboxKeyword "ti"
  , changeTodoKeywordBinding orgRelevantKeyword "tr"
  , changeTodoKeywordBinding orgSomedayKeyword "ts"
  , changeTodoKeywordBinding orgNotesKeyword "tn"
  , changeTodoKeywordBinding orgListKeyword "tl"
  , changeTodoKeywordBinding orgWaitingKeyword "tw"
  , changeTodoKeywordBinding orgProjectKeyword "tp"
  , changeTodoKeywordBinding orgTodoKeyword "tt"
  , changeTodoKeywordBinding orgDoneKeyword "td"
  , changeTodoKeywordBinding orgTrashKeyword "tx"
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
    normalBinding (View "all") (toKeySeq " aa") "Show all tasks" $ CmdProjects.saveForJump $ applyFilterToAllTasks (const True)
  , changeViewKeywordBinding orgInboxKeyword " ai" $ Just sortByCreatedDesc
  , changeViewKeywordBinding orgRelevantKeyword " ar" $ Just sortByPriorityAsc
  , changeViewKeywordBinding orgSomedayKeyword " as" Nothing
  , changeViewKeywordBinding orgNotesKeyword " an" Nothing
  , changeViewKeywordBinding orgListKeyword " al" Nothing
  , changeViewKeywordBinding orgWaitingKeyword " aw" $ Just sortByPriorityAsc
  , changeViewKeywordBinding orgProjectKeyword " ap" $ Just sortByPriorityAsc
  , changeViewKeywordBinding orgTodoKeyword " at" $ Just sortByPriorityAsc
  , changeViewKeywordBinding orgDoneKeyword " ad" Nothing
  , changeViewKeywordBinding orgTrashKeyword " ax" Nothing
  , -- Sorting
    normalBinding SortCreatedAsc (toKeySeq "sca") "Sort by created (asc)" $ CmdProjects.saveForJump (applySorter sortByCreatedAsc)
  , normalBinding SortCreatedDesc (toKeySeq "scd") "Sort by created (desc)" $ CmdProjects.saveForJump (applySorter sortByCreatedDesc)
  , normalBinding SortPriorityAsc (toKeySeq "sca") "Sort by priority (asc)" $ CmdProjects.saveForJump (applySorter sortByPriorityAsc)
  , normalBinding SortPriorityDesc (toKeySeq "scd") "Sort by priority (desc)" $ CmdProjects.saveForJump (applySorter sortByPriorityDesc)
  , -- Other
    normalBinding AddTask (toKeySeq "at") "Add new task" $ saveForUndo addNewTask
  , normalBinding EditInEditor (toKey KEnter) "Edit in editor" $ saveForUndo editSelectedTaskInEditor
  , normalBinding OpenUrl (toKeySeq "gx") "Open URL in task" openTaskUrl
  , normalBinding Refile (toKeySeq "gr") "Refile task to project" openRefileDialog
  , normalBinding RefreshView (toKey 'r') "Refresh current view" refreshCurrentView
  ]
  -- Commands from registry
  ++ map commandToKeyBinding Registry.allCommands
