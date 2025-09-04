{-# LANGUAGE FlexibleInstances #-}

module Refile.OrgRefileable where

import Brick (get, modify)
import Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as V

import Model.OrgMode (Task, TaskFile, content, level, todoKeyword)
import Parser.Parser (success)
import Refile.Refileable
import Tui.Types

-- | Refileable instance for Task
instance Refileable Task where
  insertTaskUnder projectPtr task originalTaskPtr = do
    ctx <- get
    let fs = view fileStateLens ctx
        projectsFilePath = view (config . projectsFile) ctx
        originalFile = view file originalTaskPtr
    
    case preview (taskBy projectPtr) fs of
      Just project -> do
        -- Calculate new task level (project level + 1)
        let projectLevel = view level project
            adjustedTask = set level (projectLevel + 1) task
        
        -- Check if we're moving within the same file (projects file)
        if originalFile == projectsFilePath
          then do
            -- Move within the same file - remove from old position and insert at new position
            case preview (ix projectsFilePath . success) fs of
              Just projectTaskFile -> do
                let projectTasks = view content projectTaskFile
                    originalIdx = view taskIndex originalTaskPtr
                    projectIdx = view taskIndex projectPtr
                    
                    -- Remove the original task
                    tasksWithoutOriginal = V.ifilter (\i _ -> i /= originalIdx) projectTasks
                    
                    -- Adjust project index if original was before it
                    adjustedProjectIdx = if originalIdx < projectIdx then projectIdx - 1 else projectIdx
                    
                    -- Insert the adjusted task after the project
                    (beforeProject, afterProject) = V.splitAt (adjustedProjectIdx + 1) tasksWithoutOriginal
                    newTasks = beforeProject V.++ V.singleton adjustedTask V.++ afterProject
                    updatedProjectFile = set content newTasks projectTaskFile
                
                -- Update the projects file
                modify $ set (fileStateLens . ix projectsFilePath . success) updatedProjectFile
              
              Nothing -> return () -- Projects file not found
          else do
            -- Moving from different file - insert in projects file (original will be marked as TRASH)
            case preview (ix projectsFilePath . success) fs of
              Just projectTaskFile -> do
                let projectTasks = view content projectTaskFile
                    projectIdx = view taskIndex projectPtr
                    (beforeProject, afterProject) = V.splitAt (projectIdx + 1) projectTasks
                    newTasks = beforeProject V.++ V.singleton adjustedTask V.++ afterProject
                    updatedProjectFile = set content newTasks projectTaskFile
                
                -- Update the projects file
                modify $ set (fileStateLens . ix projectsFilePath . success) updatedProjectFile
              
              Nothing -> return () -- Projects file not found
      Nothing -> return () -- Project not found
  
  markTaskForRemoval taskPtr = do
    ctx <- get
    let projectsFilePath = view (config . projectsFile) ctx
        taskFile = view file taskPtr
    
    -- Only mark as TRASH if the task is NOT in the projects file
    -- (if it's in the projects file, it was already moved by insertTaskUnder)
    if taskFile /= projectsFilePath
      then modify $ over (fileStateLens . taskBy taskPtr . todoKeyword) (const (T.pack "TRASH"))
      else return () -- Task was moved within projects file, no need to mark as TRASH