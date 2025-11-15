{-# LANGUAGE OverloadedStrings #-}

module Core.OperationsSpec (spec) where

import Control.Lens
import Core.Operations
import Core.Types
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Model.OrgMode
import Parser.Parser (ParserResult (..), success)
import Test.Hspec

-- Helper function to create a test task
mkTask :: T.Text -> T.Text -> Int -> Task
mkTask keyword title lvl =
  Task
    { _level = lvl
    , _todoKeyword = keyword
    , _priority = Nothing
    , _title = title
    , _tags = S.empty
    , _scheduled = Nothing
    , _deadline = Nothing
    , _createdProp = Nothing
    , _closed = Nothing
    , _properties = []
    , _description = ""
    }

-- Helper function to create a test FileState
mkFileState :: [(FilePath, [Task])] -> FileState Task
mkFileState files =
  M.fromList
    [ (fp, ParserSuccess (TaskFile Nothing (V.fromList tasks)))
    | (fp, tasks) <- files
    ]

spec :: Spec
spec = do
  describe "Query Operations" $ do
    describe "getTask" $ do
      it "returns Just task when task exists" $ do
        let task1 = mkTask "TODO" "Task 1" 1
            task2 = mkTask "DONE" "Task 2" 1
            fs = mkFileState [("file1.org", [task1, task2])]
            ptr = TaskPointer "file1.org" 0
        getTask ptr fs `shouldBe` Just task1

      it "returns Nothing when file doesn't exist" $ do
        let fs = mkFileState []
            ptr = TaskPointer "nonexistent.org" 0
        getTask ptr fs `shouldBe` Nothing

      it "returns Nothing when index is out of bounds" $ do
        let task1 = mkTask "TODO" "Task 1" 1
            fs = mkFileState [("file1.org", [task1])]
            ptr = TaskPointer "file1.org" 5
        getTask ptr fs `shouldBe` Nothing

    describe "getAllTasks" $ do
      it "returns empty list for empty FileState" $ do
        let fs = mkFileState []
        getAllTasks fs `shouldBe` []

      it "returns all tasks from single file" $ do
        let task1 = mkTask "TODO" "Task 1" 1
            task2 = mkTask "DONE" "Task 2" 1
            fs = mkFileState [("file1.org", [task1, task2])]
        length (getAllTasks fs) `shouldBe` 2

      it "returns tasks from multiple files" $ do
        let task1 = mkTask "TODO" "Task 1" 1
            task2 = mkTask "DONE" "Task 2" 1
            task3 = mkTask "TODO" "Task 3" 1
            fs = mkFileState [("file1.org", [task1, task2]), ("file2.org", [task3])]
        length (getAllTasks fs) `shouldBe` 3

    describe "getAllProjects" $ do
      it "returns empty list when no projects exist" $ do
        let task1 = mkTask "TODO" "Task 1" 1
            task2 = mkTask "DONE" "Task 2" 1
            fs = mkFileState [("file1.org", [task1, task2])]
        getAllProjects fs `shouldBe` []

      it "returns only PROJECT tasks" $ do
        let task1 = mkTask "TODO" "Task 1" 1
            project1 = mkTask "PROJECT" "Project 1" 1
            task2 = mkTask "DONE" "Task 2" 1
            project2 = mkTask "PROJECT" "Project 2" 1
            fs = mkFileState [("file1.org", [task1, project1, task2, project2])]
        length (getAllProjects fs) `shouldBe` 2

      it "returns projects from multiple files" $ do
        let project1 = mkTask "PROJECT" "Project 1" 1
            project2 = mkTask "PROJECT" "Project 2" 1
            task1 = mkTask "TODO" "Task 1" 1
            fs = mkFileState [("file1.org", [project1, task1]), ("file2.org", [project2])]
        length (getAllProjects fs) `shouldBe` 2

    describe "findProjectForTask" $ do
      it "returns Nothing when task has no parent project" $ do
        let task1 = mkTask "TODO" "Task 1" 1
            fs = mkFileState [("file1.org", [task1])]
            ptr = TaskPointer "file1.org" 0
        findProjectForTask ptr fs `shouldBe` Nothing

      it "finds immediate parent project" $ do
        let project = mkTask "PROJECT" "Project" 1
            subtask = mkTask "TODO" "Subtask" 2
            fs = mkFileState [("file1.org", [project, subtask])]
            ptr = TaskPointer "file1.org" 1
        findProjectForTask ptr fs `shouldBe` Just (TaskPointer "file1.org" 0)

      it "finds nearest parent project in hierarchy" $ do
        let project1 = mkTask "PROJECT" "Project 1" 1
            subtask1 = mkTask "TODO" "Subtask 1" 2
            subsubtask = mkTask "TODO" "Subsubtask" 3
            fs = mkFileState [("file1.org", [project1, subtask1, subsubtask])]
            ptr = TaskPointer "file1.org" 2
        -- Should find project1, not subtask1 (which is not a PROJECT)
        findProjectForTask ptr fs `shouldBe` Just (TaskPointer "file1.org" 0)

    describe "getProjectSubtasks" $ do
      it "returns empty list when project has no subtasks" $ do
        let project = mkTask "PROJECT" "Project" 1
            fs = mkFileState [("file1.org", [project])]
            ptr = TaskPointer "file1.org" 0
        getProjectSubtasks ptr fs `shouldBe` []

      it "returns immediate subtasks" $ do
        let project = mkTask "PROJECT" "Project" 1
            subtask1 = mkTask "TODO" "Subtask 1" 2
            subtask2 = mkTask "TODO" "Subtask 2" 2
            fs = mkFileState [("file1.org", [project, subtask1, subtask2])]
            ptr = TaskPointer "file1.org" 0
        length (getProjectSubtasks ptr fs) `shouldBe` 2

      it "returns nested subtasks" $ do
        let project = mkTask "PROJECT" "Project" 1
            subtask1 = mkTask "TODO" "Subtask 1" 2
            subsubtask = mkTask "TODO" "Subsubtask" 3
            subtask2 = mkTask "TODO" "Subtask 2" 2
            fs = mkFileState [("file1.org", [project, subtask1, subsubtask, subtask2])]
            ptr = TaskPointer "file1.org" 0
        length (getProjectSubtasks ptr fs) `shouldBe` 3

      it "stops at next same-level or higher-level task" $ do
        let project = mkTask "PROJECT" "Project" 1
            subtask1 = mkTask "TODO" "Subtask 1" 2
            subtask2 = mkTask "TODO" "Subtask 2" 2
            otherTask = mkTask "TODO" "Other Task" 1
            fs = mkFileState [("file1.org", [project, subtask1, subtask2, otherTask])]
            ptr = TaskPointer "file1.org" 0
        length (getProjectSubtasks ptr fs) `shouldBe` 2

  describe "Mutation Operations" $ do
    describe "addTask" $ do
      it "adds task to existing file" $ do
        let task1 = mkTask "TODO" "Task 1" 1
            task2 = mkTask "DONE" "Task 2" 1
            fs = mkFileState [("file1.org", [task1])]
            result = addTask "file1.org" task2 fs
        case result of
          Right (newFs, ptr) -> do
            view taskIndex ptr `shouldBe` 1
            getTask ptr newFs `shouldBe` Just task2
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

      it "returns error when file doesn't exist" $ do
        let task = mkTask "TODO" "Task" 1
            fs = mkFileState []
        addTask "nonexistent.org" task fs `shouldSatisfy` isLeft

    describe "markTaskDone" $ do
      it "changes TODO keyword to DONE" $ do
        let task = mkTask "TODO" "Task" 1
            fs = mkFileState [("file1.org", [task])]
            ptr = TaskPointer "file1.org" 0
        case markTaskDone ptr fs of
          Right newFs ->
            case getTask ptr newFs of
              Just updatedTask -> view todoKeyword updatedTask `shouldBe` "DONE"
              Nothing -> expectationFailure "Task not found after update"
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

      it "returns error when task doesn't exist" $ do
        let fs = mkFileState []
            ptr = TaskPointer "file1.org" 0
        markTaskDone ptr fs `shouldSatisfy` isLeft

    describe "editTask" $ do
      it "replaces task with new version" $ do
        let oldTask = mkTask "TODO" "Old Title" 1
            newTask = mkTask "DONE" "New Title" 2
            fs = mkFileState [("file1.org", [oldTask])]
            ptr = TaskPointer "file1.org" 0
        case editTask ptr newTask fs of
          Right newFs ->
            getTask ptr newFs `shouldBe` Just newTask
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

      it "returns error when task doesn't exist" $ do
        let task = mkTask "TODO" "Task" 1
            fs = mkFileState []
            ptr = TaskPointer "file1.org" 0
        editTask ptr task fs `shouldSatisfy` isLeft

    describe "deleteTask" $ do
      it "marks task as TRASH" $ do
        let task = mkTask "TODO" "Task" 1
            fs = mkFileState [("file1.org", [task])]
            ptr = TaskPointer "file1.org" 0
        case deleteTask ptr fs of
          Right newFs ->
            case getTask ptr newFs of
              Just updatedTask -> view todoKeyword updatedTask `shouldBe` "TRASH"
              Nothing -> expectationFailure "Task not found after deletion"
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

    describe "changeTodoKeyword" $ do
      it "changes task keyword" $ do
        let task = mkTask "TODO" "Task" 1
            fs = mkFileState [("file1.org", [task])]
            ptr = TaskPointer "file1.org" 0
        case changeTodoKeyword "WAITING" ptr fs of
          Right newFs ->
            case getTask ptr newFs of
              Just updatedTask -> view todoKeyword updatedTask `shouldBe` "WAITING"
              Nothing -> expectationFailure "Task not found after update"
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

    describe "changeTaskPriority" $ do
      it "sets priority on task without priority" $ do
        let task = mkTask "TODO" "Task" 1
            fs = mkFileState [("file1.org", [task])]
            ptr = TaskPointer "file1.org" 0
        case changeTaskPriority (Just 1) ptr fs of
          Right newFs ->
            case getTask ptr newFs of
              Just updatedTask -> view priority updatedTask `shouldBe` Just 1
              Nothing -> expectationFailure "Task not found after update"
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

      it "changes existing priority" $ do
        let task = (mkTask "TODO" "Task" 1) & priority .~ Just 2
            fs = mkFileState [("file1.org", [task])]
            ptr = TaskPointer "file1.org" 0
        case changeTaskPriority (Just 0) ptr fs of
          Right newFs ->
            case getTask ptr newFs of
              Just updatedTask -> view priority updatedTask `shouldBe` Just 0
              Nothing -> expectationFailure "Task not found after update"
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

    describe "addTaskTag" $ do
      it "adds tag to task" $ do
        let task = mkTask "TODO" "Task" 1
            fs = mkFileState [("file1.org", [task])]
            ptr = TaskPointer "file1.org" 0
        case addTaskTag "music" ptr fs of
          Right newFs ->
            case getTask ptr newFs of
              Just updatedTask -> view tags updatedTask `shouldBe` S.singleton "music"
              Nothing -> expectationFailure "Task not found after update"
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

      it "adds tag to existing tags" $ do
        let task = (mkTask "TODO" "Task" 1) & tags .~ S.singleton "work"
            fs = mkFileState [("file1.org", [task])]
            ptr = TaskPointer "file1.org" 0
        case addTaskTag "music" ptr fs of
          Right newFs ->
            case getTask ptr newFs of
              Just updatedTask -> view tags updatedTask `shouldBe` S.fromList ["work", "music"]
              Nothing -> expectationFailure "Task not found after update"
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

    describe "deleteTaskTag" $ do
      it "deletes tag from task" $ do
        let task = (mkTask "TODO" "Task" 1) & tags .~ S.fromList ["work", "music"]
            fs = mkFileState [("file1.org", [task])]
            ptr = TaskPointer "file1.org" 0
        case deleteTaskTag "music" ptr fs of
          Right newFs ->
            case getTask ptr newFs of
              Just updatedTask -> view tags updatedTask `shouldBe` S.singleton "work"
              Nothing -> expectationFailure "Task not found after update"
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

      it "handles deleting non-existent tag" $ do
        let task = (mkTask "TODO" "Task" 1) & tags .~ S.singleton "work"
            fs = mkFileState [("file1.org", [task])]
            ptr = TaskPointer "file1.org" 0
        case deleteTaskTag "music" ptr fs of
          Right newFs ->
            case getTask ptr newFs of
              Just updatedTask -> view tags updatedTask `shouldBe` S.singleton "work"
              Nothing -> expectationFailure "Task not found after update"
          Left err -> expectationFailure $ "Expected success but got error: " ++ err

-- Helper to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
