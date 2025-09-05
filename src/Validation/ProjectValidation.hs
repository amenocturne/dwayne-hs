{-# LANGUAGE FlexibleContexts #-}

module Validation.ProjectValidation where

import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Model.OrgMode (Task, TaskFile, content, todoKeyword)
import Parser.Parser (resultToMaybe)
import Tui.Types (FileState, TaskPointer(..), AppContext, taskBy, config, projectsFile, fileStateLens, file)

data ProjectValidationResult = ProjectValidationResult
  { misplacedProjectTasks :: [TaskPointer]  -- PROJECT tasks not in projects file
  , hasErrors :: Bool                       -- True if there are misplaced PROJECT tasks
  } deriving (Show, Eq)

validateProjectTasks :: FileState Task -> String -> ProjectValidationResult
validateProjectTasks fs projectsFilePath =
  let allProjectTasks = getAllProjectTasksWithLocation fs
      misplaced = filter (\ptr -> view file ptr /= projectsFilePath) allProjectTasks
  in ProjectValidationResult
       { misplacedProjectTasks = misplaced
       , hasErrors = not (null misplaced)
       }

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

validateSingleProjectTask :: TaskPointer -> FileState Task -> String -> Bool
validateSingleProjectTask taskPtr fs projectsFilePath =
  case preview (taskBy taskPtr) fs of
    Just task -> 
      if isProjectTask task
        then view file taskPtr == projectsFilePath
        else True  -- Non-PROJECT tasks don't need validation
    Nothing -> True

validateCurrentContext :: AppContext Task -> ProjectValidationResult
validateCurrentContext ctx =
  let fs = view fileStateLens ctx
      projectsFilePath = view (config . projectsFile) ctx
  in validateProjectTasks fs projectsFilePath