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
  insertTaskUnder projectPtr task _originalTaskPtr = do
    ctx <- get
    let fs = view fileStateLens ctx
        projectsFilePath = view (config . projectsFile) ctx
    
    case preview (taskBy projectPtr) fs of
      Just project -> do
        -- Calculate new task level (project level + 1)
        let projectLevel = view level project
            adjustedTask = set level (projectLevel + 1) task
        
        -- Insert the task after the project in the projects file
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
    -- Mark the original task as TRASH
    modify $ over (fileStateLens . taskBy taskPtr . todoKeyword) (const (T.pack "TRASH"))