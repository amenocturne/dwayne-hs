{-# LANGUAGE OverloadedStrings #-}

module Commands.Projects (goToProjectsCommand, goToProjectView, saveForJump, showProjectView) where

import Brick (get, modify)
import Commands.Command (Command (..))
import Control.Lens
import qualified Core.Operations as Ops
import Core.Types (FileState, TaskPointer)
import Model.OrgMode (Task)
import Tui.Types
  ( AppContext
  , GlobalAppState
  , KeyEvent (..)
  , KeyPress (..)
  , appState
  , compactView
  , compactViewLens
  , currentTaskPtr
  , cursorLens
  , fileStateLens
  , taskBy
  , viewFilterLens
  , viewportStart
  , viewSorterLens
  )
import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import qualified Model.LinearHistory as L

-- | Go to project view command
-- If the current task has a parent project, shows that project and its subtasks
-- If the current task IS a project, shows it and its subtasks
-- Otherwise, does nothing
goToProjectsCommand :: Command Task
goToProjectsCommand =
  Command
    { cmdName = "Go to Project View"
    , cmdDescription = "Navigate to the project view for the current task, showing the project and all its subtasks"
    , cmdKeyEvent = GoToProject
    , cmdTuiKeybinding = keySeq "gp" -- 'g' then 'p'
    , cmdTuiDescription = "Go to project view"
    , cmdAction = saveForJump goToProjectView
    , cmdContext = const True -- Always available in normal mode (context will be checked by key bindings)
    }
  where
    -- Helper to create key sequences without circular dependency
    keySeq (c:cs) = KeyPress (E.KChar c) Set.empty :| map (\x -> KeyPress (E.KChar x) Set.empty) cs
    keySeq [] = error "keySeq: empty string"

-- | Save current view state before making a jump (for jump history)
saveForJump :: GlobalAppState a -> GlobalAppState a
saveForJump f = do
  ctx <- get
  let oldHist = view (appState . compactView) ctx
  f
  newCtx <- get
  let newState = view compactViewLens newCtx
  when (newState /= view L.currentState oldHist) $
    modify $ over (appState . compactView) (const $ L.append newState oldHist)

-- | Main action: Navigate to project view for current task
goToProjectView :: GlobalAppState Task
goToProjectView = do
  ctx <- get
  let fs = view fileStateLens ctx
      maybeCurrentPtr = view currentTaskPtr ctx
  case maybeCurrentPtr of
    Nothing -> return ()
    Just currentPtr -> do
      let maybeProjectPtr = Ops.findProjectForTask currentPtr fs
          currentTask = preview (taskBy currentPtr) fs
      case maybeProjectPtr of
        -- If task has a parent project, show the parent project and its contents
        Just projPtr ->
          showProjectView projPtr fs ctx
        -- If task has no parent project, check if current task is a PROJECT task
        Nothing ->
          case currentTask of
            Just task | Ops.isProjectTask task ->
              showProjectView currentPtr fs ctx
            _ -> return () -- Do nothing if task is neither PROJECT nor has parent PROJECT

-- | Helper function to show project view for a given project pointer
showProjectView :: TaskPointer -> FileState Task -> AppContext Task -> GlobalAppState Task
showProjectView projPtr fs ctx = do
  let projectSubtasks = Ops.getProjectSubtasks projPtr fs
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
