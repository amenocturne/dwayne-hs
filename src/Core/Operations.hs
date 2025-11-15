{-# LANGUAGE OverloadedStrings #-}

{- | Core business logic operations for task management
This module contains all pure operations for querying and manipulating tasks.
It is independent of any UI framework (TUI, CLI, Web).
-}
module Core.Operations (
  -- * Query Operations
  getTask,
  getAllTasks,
  getAllProjects,
  findProjectForTask,
  getProjectSubtasks,
  isProjectTask,

  -- * Mutation Operations
  addTask,
  markTaskDone,
  editTask,
  deleteTask,
  changeTodoKeyword,
  changeTaskPriority,
  addTaskTag,
  deleteTaskTag,

  -- * Helper Functions
  resultToMaybe,
) where

import Control.Lens
import Core.Types
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Model.OrgMode (Task, TaskFile, content, level, priority, tags, todoKeyword, orgProjectKeyword, orgDoneKeyword, orgTrashKeyword)
import Parser.Parser (ParserResult (..), resultToMaybe, success)

-- | Get a specific task by its pointer
getTask :: TaskPointer -> FileState Task -> Maybe Task
getTask ptr = preview (ix (view file ptr) . success . content . ix (view taskIndex ptr))

-- | Get all task pointers from all files
getAllTasks :: FileState Task -> [TaskPointer]
getAllTasks fs =
  [ TaskPointer fp idx
  | (fp, result) <- M.toList fs
  , taskFile <- maybe [] pure (resultToMaybe result)
  , idx <- [0 .. V.length (view content taskFile) - 1]
  ]

-- | Check if a task is a project (has PROJECT keyword)
isProjectTask :: Task -> Bool
isProjectTask task = view todoKeyword task == orgProjectKeyword

-- | Get all project pointers from all files
getAllProjects :: FileState Task -> [TaskPointer]
getAllProjects fs =
  [ TaskPointer fp idx
  | (fp, result) <- M.toList fs
  , taskFile <- maybe [] pure (resultToMaybe result)
  , (idx, task) <- zip [0 ..] (V.toList $ view content taskFile)
  , isProjectTask task
  ]

-- | Find the project that contains the given task
findProjectForTask :: TaskPointer -> FileState Task -> Maybe TaskPointer
findProjectForTask taskPtr fs =
  case preview (ix (view file taskPtr) . success . content) fs of
    Nothing -> Nothing
    Just tasks ->
      let taskIdx = view taskIndex taskPtr
          targetLevel = maybe 0 (view level) (tasks V.!? taskIdx)
          findProjectIndex idx
            | idx <= 0 = Nothing
            | otherwise =
                case tasks V.!? (idx - 1) of
                  Just task
                    | view level task < targetLevel && isProjectTask task ->
                        Just (idx - 1)
                  _ -> findProjectIndex (idx - 1)
       in fmap (TaskPointer (view file taskPtr)) (findProjectIndex taskIdx)

-- | Get all subtasks that belong to a project
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

-- | Add a new task to a file
addTask :: FilePath -> Task -> FileState Task -> Either String (FileState Task, TaskPointer)
addTask fp task fs =
  case preview (ix fp . success) fs of
    Nothing -> Left $ "File not found: " ++ fp
    Just taskFile ->
      let oldTasks = view content taskFile
          newIdx = V.length oldTasks
          updatedTaskFile = taskFile & content .~ V.snoc oldTasks task
          updatedFs = fs & ix fp . success .~ updatedTaskFile
          ptr = TaskPointer fp newIdx
       in Right (updatedFs, ptr)

-- | Mark a task as DONE
markTaskDone :: TaskPointer -> FileState Task -> Either String (FileState Task)
markTaskDone ptr fs =
  case getTask ptr fs of
    Nothing -> Left $ "Task not found at: " ++ show ptr
    Just _ ->
      Right $ fs & ix (view file ptr) . success . content . ix (view taskIndex ptr) . todoKeyword .~ orgDoneKeyword

-- | Edit a task by replacing it with a new version
editTask :: TaskPointer -> Task -> FileState Task -> Either String (FileState Task)
editTask ptr newTask fs =
  case getTask ptr fs of
    Nothing -> Left $ "Task not found at: " ++ show ptr
    Just _ ->
      Right $ fs & ix (view file ptr) . success . content . ix (view taskIndex ptr) .~ newTask

-- | Delete a task by marking it as TRASH
deleteTask :: TaskPointer -> FileState Task -> Either String (FileState Task)
deleteTask ptr fs =
  case getTask ptr fs of
    Nothing -> Left $ "Task not found at: " ++ show ptr
    Just _ ->
      Right $ fs & ix (view file ptr) . success . content . ix (view taskIndex ptr) . todoKeyword .~ orgTrashKeyword

-- | Change the TODO keyword of a task
changeTodoKeyword :: T.Text -> TaskPointer -> FileState Task -> Either String (FileState Task)
changeTodoKeyword keyword ptr fs =
  case getTask ptr fs of
    Nothing -> Left $ "Task not found at: " ++ show ptr
    Just _ ->
      Right $ fs & ix (view file ptr) . success . content . ix (view taskIndex ptr) . todoKeyword .~ keyword

-- | Change the priority of a task
changeTaskPriority :: Maybe Int -> TaskPointer -> FileState Task -> Either String (FileState Task)
changeTaskPriority newPriority ptr fs =
  case getTask ptr fs of
    Nothing -> Left $ "Task not found at: " ++ show ptr
    Just _ ->
      Right $ fs & ix (view file ptr) . success . content . ix (view taskIndex ptr) . priority .~ newPriority

-- | Add a tag to a task
addTaskTag :: T.Text -> TaskPointer -> FileState Task -> Either String (FileState Task)
addTaskTag tag ptr fs =
  case getTask ptr fs of
    Nothing -> Left $ "Task not found at: " ++ show ptr
    Just _ ->
      Right $ fs & ix (view file ptr) . success . content . ix (view taskIndex ptr) . tags %~ S.insert tag

-- | Delete a tag from a task
deleteTaskTag :: T.Text -> TaskPointer -> FileState Task -> Either String (FileState Task)
deleteTaskTag tag ptr fs =
  case getTask ptr fs of
    Nothing -> Left $ "Task not found at: " ++ show ptr
    Just _ ->
      Right $ fs & ix (view file ptr) . success . content . ix (view taskIndex ptr) . tags %~ S.delete tag
