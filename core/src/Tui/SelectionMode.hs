{-# LANGUAGE OverloadedStrings #-}

module Tui.SelectionMode
  ( enterSelectionMode,
    exitSelectionMode,
    toggleRangeSelection,
    toggleCurrentSelection,
    getSelectedTaskPointers,
    applyToSelection,
    smartApplyTodoKeyword,
    smartApplyTagAction,
    selectionAwareMove,
  )
where

import Brick
import Brick.BChan
import Brick.Widgets.Dialog (dialog)
import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Core.Types (TaskPointer)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Model.OrgMode (Task, orgProjectKeyword, tags, todoKeyword)
import qualified Tui.Helpers as Helpers
import Tui.Types
  ( AppContext,
    AppEvent (..),
    AppMode (..),
    GlobalAppState,
    Name (..),
    ValidationDialog (..),
    appMode,
    appState,
    compactViewLens,
    config,
    currentTaskLens,
    currentTaskPtr,
    currentViewLens,
    cursorLens,
    eventChannel,
    fileStateLens,
    scrollingMargin,
    selectionAnchorLens,
    selectionLens,
    switchMode,
    taskBy,
    viewportStart,
  )
import qualified Validation.SystemValidation as SV

-- | Enter selection mode, selecting the current task under cursor
enterSelectionMode :: GlobalAppState Task
enterSelectionMode = do
  ctx <- get
  case view cursorLens ctx of
    Just cursor -> do
      modify $ switchMode SelectionMode
      modify $ set selectionLens (Set.singleton cursor)
      modify $ set selectionAnchorLens (Just cursor)
    Nothing -> return ()

-- | Toggle range selection - extends selection from anchor to current cursor
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

-- | Toggle selection of the current task under cursor
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

-- | Exit selection mode and clear all selections
exitSelectionMode :: GlobalAppState Task
exitSelectionMode = do
  modify $ switchMode NormalMode
  modify $ set selectionLens Set.empty
  modify $ set selectionAnchorLens Nothing

-- | Get pointers for currently selected tasks
getSelectedTaskPointers :: AppContext Task -> [TaskPointer]
getSelectedTaskPointers ctx =
  let selection = view selectionLens ctx
      cv = view currentViewLens ctx
      selectedIndices = Set.toList selection
   in mapMaybe (cv V.!?) selectedIndices

-- | Apply a function to all selected tasks
applyToSelection :: (Task -> Task) -> GlobalAppState Task
applyToSelection action = do
  ctx <- get
  let selectedIndices = Set.toList $ view selectionLens ctx
      cv = view currentViewLens ctx
      selectedPtrs = mapMaybe (cv V.!?) selectedIndices
  mapM_ (\ptr -> modify $ over (fileStateLens . taskBy ptr) action) selectedPtrs

-- | Smart apply TODO keyword - works on selection or current task
-- Includes validation for PROJECT keyword
smartApplyTodoKeyword :: T.Text -> GlobalAppState Task
smartApplyTodoKeyword keyword = do
  ctx <- get
  let selection = view selectionLens ctx
      needsValidation = keyword == orgProjectKeyword
      getCurrentKeywords =
        if Set.null selection
          then case view currentTaskPtr ctx of
            Just ptr -> maybe [] (pure . view todoKeyword) $ preview (taskBy ptr) (view fileStateLens ctx)
            Nothing -> []
          else
            let selectedTasks = getSelectedTaskPointers ctx
                fs = view fileStateLens ctx
             in mapMaybe (\ptr -> preview (taskBy ptr . todoKeyword) fs) selectedTasks

      oldKeywords = getCurrentKeywords
      oldHasProject = orgProjectKeyword `elem` oldKeywords

  if Set.null selection
    then modify (Helpers.changeTodoKeyword keyword)
    else applyToSelection (set todoKeyword keyword)

  when (needsValidation || oldHasProject) $ do
    newCtx <- get
    let issues = SV.validateSystem newCtx
    case issues of
      [] -> return ()
      (issue : _) -> do
        let message = T.unpack (SV.issueDescription issue)
            dlg =
              ValidationDialog
                { _vdDialog = dialog (Just $ str "Validation") Nothing 60,
                  _vdMisplacedTasks = SV.affectedItems issue,
                  _vdMessage = message ++ " (Enter to accept, Esc to cancel)"
                }
        liftIO $ writeBChan (view (appState . eventChannel) newCtx) $ ValidationDialogCreated dlg

-- | Smart apply tag action - works on selection or current task
smartApplyTagAction :: (S.Set T.Text -> S.Set T.Text) -> GlobalAppState Task
smartApplyTagAction tagAction = do
  ctx <- get
  let selection = view selectionLens ctx
  if Set.null selection
    then modify (over (currentTaskLens . tags) tagAction)
    else applyToSelection (over tags tagAction)

-- | Selection-aware cursor movement
-- In selection mode, moves cursor and extends selection range
-- In normal mode, just moves cursor
selectionAwareMove :: (Int -> Int) -> GlobalAppState Task
selectionAwareMove moveFunc = do
  ctx <- get
  case view (appState . appMode) ctx of
    SelectionMode -> do
      Helpers.adjustCursor moveFunc
      newCtx <- get
      case (view selectionAnchorLens newCtx, view cursorLens newCtx) of
        (Just anchor, Just newCursor) -> do
          let range = Set.fromList [min anchor newCursor .. max anchor newCursor]
          modify $ set selectionLens range
        _ -> return ()
    _ -> Helpers.adjustCursor moveFunc
