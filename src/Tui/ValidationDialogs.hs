{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tui.ValidationDialogs where

import Brick
import Control.Lens
import Control.Monad (when)
import qualified Core.Operations as Ops
import Core.Types (FileState, TaskPointer (..), file)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Model.OrgMode (Task, content, level, orgDoneKeyword, orgInboxKeyword, orgListKeyword, orgNotesKeyword, orgProjectKeyword, orgRelevantKeyword, orgSomedayKeyword, orgTodoKeyword, orgTrashKeyword, orgWaitingKeyword, todoKeyword)
import Parser.Parser (success)
import Tui.Types
import Writer.Writer

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

          adjustedTasks <-
            mapM
              ( \ptr -> do
                  currentCtx <- get
                  let currentFs = view fileStateLens currentCtx
                  case preview (taskBy ptr) currentFs of
                    Just task -> do
                      let adjustedTask = over level (+ levelAdjustment) task
                      when (view file ptr /= projectsFilePath) $
                        modify $
                          over (fileStateLens . taskBy ptr . todoKeyword) (const orgTrashKeyword)
                      return $ Just adjustedTask
                    Nothing -> return Nothing
              )
              allTasksToMove

          let validTasks = mapMaybe id adjustedTasks
              newTasks = currentTasks V.++ V.fromList validTasks
              updatedProjectFile = set content newTasks projectTaskFile

          modify $ set (fileStateLens . ix projectsFilePath . success) updatedProjectFile
        Nothing -> return () -- Projects file not found
    Nothing -> return ()

rejectValidation :: (Writer a) => GlobalAppState a
rejectValidation = modify (over (appState . validationDialog) (const Nothing))
