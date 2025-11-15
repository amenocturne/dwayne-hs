{-# LANGUAGE FlexibleContexts #-}

module Validation.ProjectValidation where

import Control.Lens
import Core.Types (TaskPointer, FileState, file)
import qualified Core.Operations as Ops

import Model.OrgMode (Task)

data ProjectValidationResult = ProjectValidationResult
  { misplacedProjectTasks :: [TaskPointer]  -- PROJECT tasks not in projects file
  , hasErrors :: Bool                       -- True if there are misplaced PROJECT tasks
  } deriving (Show, Eq)

validateProjectTasks :: FileState Task -> String -> ProjectValidationResult
validateProjectTasks fs projectsFilePath =
  let allProjectTasks = Ops.getAllProjects fs
      misplaced = filter (\ptr -> view file ptr /= projectsFilePath) allProjectTasks
  in ProjectValidationResult
       { misplacedProjectTasks = misplaced
       , hasErrors = not (null misplaced)
       }
