{-# LANGUAGE FlexibleContexts #-}

module Validation.SystemValidation where

import Control.Lens
import qualified Core.Operations as Ops
import Core.Types (FileState, TaskPointer, file)
import qualified Data.Text as T
import Model.OrgMode (Task)
import Tui.Types (AppContext, config, fileStateLens, projectsFile)

data ValidationIssueId
  = MisplacedProjectTasks
  deriving (Eq, Show, Ord, Enum, Bounded)

data ValidationFixId
  = MoveTasksToProjectsFile
  deriving (Eq, Show, Ord, Enum, Bounded)

data ValidationIssue = ValidationIssue
  { issueId :: ValidationIssueId,
    issueDescription :: T.Text,
    affectedItems :: [TaskPointer],
    severity :: ValidationSeverity
  }
  deriving (Eq, Show)

data ValidationSeverity = Warning | Error
  deriving (Eq, Show, Ord)

data ValidationFix a = ValidationFix
  { fixId :: ValidationFixId,
    fixDescription :: T.Text,
    fixFunction :: FileState a -> FileState a
  }

class SystemValidator a where
  validateSystem :: AppContext a -> [ValidationIssue]
  getFixForIssue :: ValidationIssueId -> ValidationFix a

instance SystemValidator Task where
  validateSystem ctx =
    let fs = view fileStateLens ctx
        projectsFilePath = view (config . projectsFile) ctx
        allProjectTasks = Ops.getAllProjects fs
        misplacedTasks = filter (\ptr -> view file ptr /= projectsFilePath) allProjectTasks

        hasSubtasks ptr = not $ null $ Ops.getProjectSubtasks ptr fs

        misplacedWithSubtasks = filter hasSubtasks misplacedTasks
     in if null misplacedWithSubtasks
          then []
          else
            [ ValidationIssue
                { issueId = MisplacedProjectTasks,
                  issueDescription =
                    T.pack $
                      "Found "
                        <> show (length misplacedWithSubtasks)
                        <> " PROJECT task(s) with subtasks outside of projects file",
                  affectedItems = misplacedWithSubtasks,
                  severity = Warning
                }
            ]

  getFixForIssue MisplacedProjectTasks =
    ValidationFix
      { fixId = MoveTasksToProjectsFile,
        fixDescription = T.pack "Move PROJECT tasks with subtasks to projects file",
        fixFunction = id
      }
