{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Refile.RefileableSpec (spec) where

import Control.Lens
-- Import the modules we're testing
import Core.Types (FileState, TaskPointer (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Model.OrgMode
import Parser.Parser (ParserResult (..), success)
import Refile.OrgRefileable ()
import Refile.Refileable
import Test.Hspec

spec :: Spec
spec = do
  describe "Refiling Operations (Pure Functions)" $ do
    describe "insertTaskUnder" $ testInsertTaskUnder
    describe "markTaskForRemoval" $ testMarkTaskForRemoval

-- Helper functions for creating test data
createTestTask :: Int -> T.Text -> T.Text -> Task
createTestTask lvl keyword taskTitle =
  Task
    { _level = lvl,
      _todoKeyword = keyword,
      _priority = Nothing,
      _title = taskTitle,
      _tags = S.empty,
      _scheduled = Nothing,
      _deadline = Nothing,
      _createdProp = Nothing,
      _closed = Nothing,
      _properties = [],
      _description = T.empty
    }

createTestTaskFile :: [Task] -> TaskFile Task
createTestTaskFile tasks =
  TaskFile
    { _name = Nothing,
      _content = V.fromList tasks
    }

createTestFileState :: [(String, TaskFile Task)] -> FileState Task
createTestFileState files =
  M.fromList [(fp, ParserSuccess tf) | (fp, tf) <- files]

-- Test cases for insertTaskUnder
testInsertTaskUnder :: Spec
testInsertTaskUnder = do
  describe "Same-file refiling (projects.org)" $ do
    it "moves task to end of file maintaining correct order" $ do
      let projectTask = createTestTask 1 "PROJECT" "Main Project"
          task1 = createTestTask 2 "TODO" "Task 1"
          taskToMove = createTestTask 2 "TODO" "Task to Move"
          task2 = createTestTask 2 "TODO" "Task 2"

          projectsFile = createTestTaskFile [projectTask, task1, taskToMove, task2]
          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]

          projectPtr = TaskPointer "/path/to/projects.org" 0
          taskPtr = TaskPointer "/path/to/projects.org" 2
          projectsFilePath = "/path/to/projects.org"

      let refileResult = insertTaskUnder projectPtr taskToMove taskPtr fileState projectsFilePath
          resultFileState = _newFileState refileResult
          wasTaskMoved = _wasTaskMoved refileResult

      wasTaskMoved `shouldBe` True
      case preview (ix "/path/to/projects.org" . success . content) resultFileState of
        Just resultTasks -> do
          V.length resultTasks `shouldBe` 4
          -- Verify task order: project, task1, task2, adjusted task (level 2)
          view title (resultTasks V.! 0) `shouldBe` "Main Project"
          view title (resultTasks V.! 1) `shouldBe` "Task 1"
          view title (resultTasks V.! 2) `shouldBe` "Task 2"
          view title (resultTasks V.! 3) `shouldBe` "Task to Move"
          view level (resultTasks V.! 3) `shouldBe` 2 -- project level + 1
        Nothing -> expectationFailure "Could not find updated tasks in projects file"

    it "moves task to beginning of file maintaining correct order" $ do
      -- Create test data: task to move at index 0, project at index 2
      let taskToMove = createTestTask 2 "TODO" "Task to Move"
          task1 = createTestTask 2 "TODO" "Task 1"
          projectTask = createTestTask 1 "PROJECT" "Main Project"
          task2 = createTestTask 2 "TODO" "Task 2"

          projectsFile = createTestTaskFile [taskToMove, task1, projectTask, task2]
          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]

          projectPtr = TaskPointer "/path/to/projects.org" 2
          taskPtr = TaskPointer "/path/to/projects.org" 0
          projectsFilePath = "/path/to/projects.org"

      -- Execute the refiling operation
      let refileResult = insertTaskUnder projectPtr taskToMove taskPtr fileState projectsFilePath
          resultFileState = _newFileState refileResult
          wasTaskMoved = _wasTaskMoved refileResult

      -- Verify the result - project index should be adjusted since original was before it
      wasTaskMoved `shouldBe` True
      case preview (ix "/path/to/projects.org" . success . content) resultFileState of
        Just resultTasks -> do
          V.length resultTasks `shouldBe` 4
          -- Verify task order: task1, project, task2, adjusted task (at end of subtasks)
          view title (resultTasks V.! 0) `shouldBe` "Task 1"
          view title (resultTasks V.! 1) `shouldBe` "Main Project"
          view title (resultTasks V.! 2) `shouldBe` "Task 2"
          view title (resultTasks V.! 3) `shouldBe` "Task to Move"
          view level (resultTasks V.! 3) `shouldBe` 2 -- project level + 1
        Nothing -> expectationFailure "Could not find updated tasks in projects file"

    it "moves task to middle maintaining complex hierarchical structure" $ do
      -- Create a more complex hierarchical structure
      let project1 = createTestTask 1 "PROJECT" "Project 1"
          subtask1_1 = createTestTask 2 "TODO" "Subtask 1.1"
          subtask1_2 = createTestTask 2 "TODO" "Subtask 1.2"
          taskToMove = createTestTask 2 "TODO" "Task to Move"
          project2 = createTestTask 1 "PROJECT" "Project 2"
          subtask2_1 = createTestTask 2 "TODO" "Subtask 2.1"

          projectsFile = createTestTaskFile [project1, subtask1_1, subtask1_2, taskToMove, project2, subtask2_1]
          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]

          project1Ptr = TaskPointer "/path/to/projects.org" 0
          taskPtr = TaskPointer "/path/to/projects.org" 3
          projectsFilePath = "/path/to/projects.org"

      -- Execute the refiling operation - move task under project1
      let refileResult = insertTaskUnder project1Ptr taskToMove taskPtr fileState projectsFilePath
          resultFileState = _newFileState refileResult

      -- Verify the result
      case preview (ix "/path/to/projects.org" . success . content) resultFileState of
        Just resultTasks -> do
          V.length resultTasks `shouldBe` 6
          -- Should be: Project1, Subtask1.1, Subtask1.2, TaskToMove, Project2, Subtask2.1
          view title (resultTasks V.! 0) `shouldBe` "Project 1"
          view title (resultTasks V.! 1) `shouldBe` "Subtask 1.1"
          view title (resultTasks V.! 2) `shouldBe` "Subtask 1.2"
          view title (resultTasks V.! 3) `shouldBe` "Task to Move"
          view title (resultTasks V.! 4) `shouldBe` "Project 2"
          view title (resultTasks V.! 5) `shouldBe` "Subtask 2.1"
          view level (resultTasks V.! 3) `shouldBe` 2 -- project level + 1
        Nothing -> expectationFailure "Could not find updated tasks in projects file"

  describe "Cross-file refiling" $ do
    it "inserts task from inbox to projects file" $ do
      let projectTask = createTestTask 1 "PROJECT" "Main Project"
          existingTask = createTestTask 2 "TODO" "Existing Task"
          projectsFile = createTestTaskFile [projectTask, existingTask]

          inboxTask = createTestTask 1 "INBOX" "Task from Inbox"
          inboxFile = createTestTaskFile [inboxTask]

          fileState =
            createTestFileState
              [ ("/path/to/projects.org", projectsFile),
                ("/path/to/inbox.org", inboxFile)
              ]

          projectPtr = TaskPointer "/path/to/projects.org" 0
          taskPtr = TaskPointer "/path/to/inbox.org" 0
          projectsFilePath = "/path/to/projects.org"

      -- Execute the refiling operation
      let refileResult = insertTaskUnder projectPtr inboxTask taskPtr fileState projectsFilePath
          resultFileState = _newFileState refileResult
          wasTaskMoved = _wasTaskMoved refileResult

      -- Verify the task was inserted in projects file with correct level
      wasTaskMoved `shouldBe` False -- Cross-file, so task was copied
      case preview (ix "/path/to/projects.org" . success . content) resultFileState of
        Just resultTasks -> do
          V.length resultTasks `shouldBe` 3
          view title (resultTasks V.! 0) `shouldBe` "Main Project"
          view title (resultTasks V.! 1) `shouldBe` "Task from Inbox"
          view title (resultTasks V.! 2) `shouldBe` "Existing Task"
          view level (resultTasks V.! 1) `shouldBe` 2 -- project level + 1
        Nothing -> expectationFailure "Could not find updated tasks in projects file"

  describe "Edge cases" $ do
    it "handles gracefully when project not found" $ do
      let inboxTask = createTestTask 1 "INBOX" "Task"
          inboxFile = createTestTaskFile [inboxTask]
          fileState = createTestFileState [("/path/to/inbox.org", inboxFile)]

          nonExistentProjectPtr = TaskPointer "/path/to/projects.org" 0 -- File doesn't exist
          taskPtr = TaskPointer "/path/to/inbox.org" 0
          projectsFilePath = "/path/to/projects.org"

      -- Execute refiling - should return original file state
      let refileResult = insertTaskUnder nonExistentProjectPtr inboxTask taskPtr fileState projectsFilePath
          resultFileState = _newFileState refileResult
          wasTaskMoved = _wasTaskMoved refileResult

      -- Should return original file state unchanged
      wasTaskMoved `shouldBe` False
      resultFileState `shouldBe` fileState

    it "handles gracefully when projects file not found in file state" $ do
      let projectTask = createTestTask 1 "PROJECT" "Project"
          projectsFile = createTestTaskFile [projectTask]
          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]

          taskToMove = createTestTask 2 "TODO" "Task"
          projectPtr = TaskPointer "/path/to/projects.org" 0
          taskPtr = TaskPointer "/path/to/projects.org" 0
          nonExistentProjectsPath = "/path/to/nonexistent.org"

      -- Execute refiling with wrong projects file path
      let refileResult = insertTaskUnder projectPtr taskToMove taskPtr fileState nonExistentProjectsPath
          resultFileState = _newFileState refileResult
          wasTaskMoved = _wasTaskMoved refileResult

      -- Should return original file state unchanged
      wasTaskMoved `shouldBe` False
      resultFileState `shouldBe` fileState

-- Test cases for markTaskForRemoval
testMarkTaskForRemoval :: Spec
testMarkTaskForRemoval = do
  describe "TRASH marking behavior" $ do
    it "marks task as TRASH when not in projects file" $ do
      let inboxTask = createTestTask 1 "INBOX" "Task in Inbox"
          inboxFile = createTestTaskFile [inboxTask]

          projectTask = createTestTask 1 "PROJECT" "Project"
          projectsFile = createTestTaskFile [projectTask]

          fileState =
            createTestFileState
              [ ("/path/to/inbox.org", inboxFile),
                ("/path/to/projects.org", projectsFile)
              ]

          taskPtr = TaskPointer "/path/to/inbox.org" 0
          projectsFilePath = "/path/to/projects.org"

      -- Execute the removal marking
      let resultFileState = markTaskForRemoval taskPtr fileState projectsFilePath

      -- Verify task was marked as TRASH
      case preview (ix "/path/to/inbox.org" . success . content . ix 0 . todoKeyword) resultFileState of
        Just keyword -> keyword `shouldBe` "TRASH"
        Nothing -> expectationFailure "Could not find updated task in inbox file"

    it "does NOT mark task as TRASH when in projects file" $ do
      let projectTask = createTestTask 1 "PROJECT" "Project"
          taskInProjects = createTestTask 2 "TODO" "Task in Projects"
          projectsFile = createTestTaskFile [projectTask, taskInProjects]

          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]
          taskPtr = TaskPointer "/path/to/projects.org" 1
          projectsFilePath = "/path/to/projects.org"

      -- Execute the removal marking
      let resultFileState = markTaskForRemoval taskPtr fileState projectsFilePath

      -- Verify task keyword was NOT changed to TRASH
      case preview (ix "/path/to/projects.org" . success . content . ix 1 . todoKeyword) resultFileState of
        Just keyword -> keyword `shouldBe` "TODO" -- Should remain unchanged
        Nothing -> expectationFailure "Could not find task in projects file"

    it "handles gracefully when task not found" $ do
      let projectTask = createTestTask 1 "PROJECT" "Project"
          projectsFile = createTestTaskFile [projectTask]
          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]

          nonExistentTaskPtr = TaskPointer "/path/to/inbox.org" 0 -- File doesn't exist
          projectsFilePath = "/path/to/projects.org"

      -- Execute removal marking - should return original file state
      let resultFileState = markTaskForRemoval nonExistentTaskPtr fileState projectsFilePath

      -- Should return original file state unchanged
      resultFileState `shouldBe` fileState
