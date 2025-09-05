{-# LANGUAGE FlexibleInstances #-}

module Refile.OrgRefileable where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as V

import Model.OrgMode (Task, TaskFile, content, level, todoKeyword)
import Parser.Parser (success)
import Refile.Refileable
import Tui.Types

-- | Refileable instance for Task
instance Refileable Task where
  insertTaskUnder projectPtr task originalTaskPtr fs projectsFilePath =
    let originalFile = view file originalTaskPtr
    in case preview (taskBy projectPtr) fs of
      Just project ->
        -- Calculate new task level (project level + 1)
        let projectLevel = view level project
            adjustedTask = set level (projectLevel + 1) task
        
        in -- Check if we're moving within the same file (projects file)
        if originalFile == projectsFilePath
          then
            -- Move within the same file - remove from old position and insert at new position
            case preview (ix projectsFilePath . success) fs of
              Just projectTaskFile ->
                let projectTasks = view content projectTaskFile
                    originalIdx = view taskIndex originalTaskPtr
                    projectIdx = view taskIndex projectPtr
                    
                    -- Remove the original task
                    tasksWithoutOriginal = V.ifilter (\i _ -> i /= originalIdx) projectTasks
                    
                    -- Adjust project index if original was before it
                    adjustedProjectIdx = if originalIdx < projectIdx then projectIdx - 1 else projectIdx
                    
                    -- Find where the project's subtasks end
                    projectLevel = view level project
                    findProjectEnd idx
                      | idx >= V.length tasksWithoutOriginal = idx
                      | otherwise = 
                          let task = tasksWithoutOriginal V.! idx
                              taskLevel = view level task
                          in if taskLevel > projectLevel
                             then findProjectEnd (idx + 1)  -- Still in project subtasks
                             else idx  -- Found end of project
                    
                    insertionPoint = findProjectEnd (adjustedProjectIdx + 1)
                    
                    -- Insert the adjusted task at the end of project's subtasks
                    (beforeInsertion, afterInsertion) = V.splitAt insertionPoint tasksWithoutOriginal
                    newTasks = beforeInsertion V.++ V.singleton adjustedTask V.++ afterInsertion
                    updatedProjectFile = set content newTasks projectTaskFile
                    updatedFs = set (ix projectsFilePath . success) updatedProjectFile fs
                
                in RefileResult updatedFs True
              
              Nothing -> RefileResult fs False -- Projects file not found
          else
            -- Moving from different file - insert in projects file (original will be marked as TRASH)
            case preview (ix projectsFilePath . success) fs of
              Just projectTaskFile ->
                let projectTasks = view content projectTaskFile
                    projectIdx = view taskIndex projectPtr
                    
                    -- For cross-file refiling, insert immediately after the project
                    -- (the test expects this behavior)
                    (beforeProject, afterProject) = V.splitAt (projectIdx + 1) projectTasks
                    newTasks = beforeProject V.++ V.singleton adjustedTask V.++ afterProject
                    updatedProjectFile = set content newTasks projectTaskFile
                    updatedFs = set (ix projectsFilePath . success) updatedProjectFile fs
                
                in RefileResult updatedFs False
              
              Nothing -> RefileResult fs False -- Projects file not found
      Nothing -> RefileResult fs False -- Project not found
  
  markTaskForRemoval taskPtr fs projectsFilePath =
    let taskFile = view file taskPtr
    
    in -- Only mark as TRASH if the task is NOT in the projects file
    -- (if it's in the projects file, it was already moved by insertTaskUnder)
    if taskFile /= projectsFilePath
      then over (taskBy taskPtr . todoKeyword) (const (T.pack "TRASH")) fs
      else fs -- Task was moved within projects file, no need to mark as TRASH