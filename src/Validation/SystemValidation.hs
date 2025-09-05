{-# LANGUAGE FlexibleContexts #-}

module Validation.SystemValidation where

import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Model.OrgMode (Task, TaskFile, content, todoKeyword, level)
import Parser.Parser (resultToMaybe, success)
import Tui.Types (FileState, TaskPointer(..), AppContext, taskBy, config, projectsFile, fileStateLens, file, taskIndex)
data ValidationIssueId 
  = MisplacedProjectTasks
  deriving (Eq, Show, Ord, Enum, Bounded)

data ValidationFixId
  = MoveTasksToProjectsFile
  deriving (Eq, Show, Ord, Enum, Bounded)

data ValidationIssue = ValidationIssue
  { issueId :: ValidationIssueId
  , issueDescription :: T.Text
  , affectedItems :: [TaskPointer]
  , severity :: ValidationSeverity
  } deriving (Eq, Show)

data ValidationSeverity = Warning | Error
  deriving (Eq, Show, Ord)

data ValidationFix a = ValidationFix
  { fixId :: ValidationFixId
  , fixDescription :: T.Text
  , fixFunction :: FileState a -> FileState a
  }

class SystemValidator a where
  validateSystem :: AppContext a -> [ValidationIssue]
  getFixForIssue :: ValidationIssueId -> ValidationFix a
isProjectTask :: Task -> Bool
isProjectTask task = view todoKeyword task == T.pack "PROJECT"
getAllProjectTasksWithLocation :: FileState Task -> [TaskPointer]
getAllProjectTasksWithLocation fs =
  [ TaskPointer fp idx
  | (fp, result) <- M.toList fs
  , taskFile <- maybe [] pure (resultToMaybe result)
  , (idx, task) <- zip [0..] (V.toList $ view content taskFile)
  , isProjectTask task
  ]
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

-- Note: The actual moving logic is implemented in acceptValidation 
-- where we have the monadic context needed for refileTaskToProject
instance SystemValidator Task where
  validateSystem ctx = 
    let fs = view fileStateLens ctx
        projectsFilePath = view (config . projectsFile) ctx
        allProjectTasks = getAllProjectTasksWithLocation fs
        misplacedTasks = filter (\ptr -> view file ptr /= projectsFilePath) allProjectTasks
    in if null misplacedTasks
       then []
       else [ValidationIssue
             { issueId = MisplacedProjectTasks
             , issueDescription = T.pack $ 
                 "Found " ++ show (length misplacedTasks) ++ " PROJECT task" ++ 
                 (if length misplacedTasks == 1 then "" else "s") ++ 
                 " not in the projects file"
             , affectedItems = misplacedTasks
             , severity = Warning
             }]
  
  getFixForIssue MisplacedProjectTasks = ValidationFix
    { fixId = MoveTasksToProjectsFile
    , fixDescription = T.pack "Move PROJECT tasks with their subtasks to projects file"
    , fixFunction = \fs -> fs -- Fix is implemented in acceptValidation where we have monadic context
    }