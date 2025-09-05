{-# LANGUAGE OverloadedStrings #-}

module Refile.OrgRefileableSpec (spec) where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Vector as V
import SpecHelper

import Model.OrgMode
import Parser.Parser
import Tui.Keybindings
import Tui.Types (TaskPointer (..))

-- Test data
sampleTask :: Task
sampleTask =
  Task
    { _level = 2
    , _todoKeyword = "TODO"
    , _priority = Nothing
    , _title = "Sample Task"
    , _tags = mempty
    , _scheduled = Nothing
    , _deadline = Nothing
    , _createdProp = Nothing
    , _closed = Nothing
    , _properties = []
    , _description = "Sample description"
    }

sampleProject :: Task
sampleProject =
  Task
    { _level = 1
    , _todoKeyword = "PROJECT"
    , _priority = Nothing
    , _title = "Sample Project"
    , _tags = mempty
    , _scheduled = Nothing
    , _deadline = Nothing
    , _createdProp = Nothing
    , _closed = Nothing
    , _properties = []
    , _description = "Project description"
    }

createTaskFile :: [Task] -> TaskFile Task
createTaskFile tasks = TaskFile{_name = Nothing, _content = V.fromList tasks}

spec :: Spec
spec = do
  describe "Project hierarchy functions" $ do
    describe "isProjectTask" $ do
      it "identifies PROJECTS tasks correctly" $ do
        let project = sampleProject
            regularTask = sampleTask
        isProjectTask project `shouldBe` True
        isProjectTask regularTask `shouldBe` False

    describe "getProjectForTask" $ do
      it "finds containing project for subtask" $ do
        let project = sampleProject
            subtask1 = sampleTask & level .~ 2
            subtask2 = sampleTask & level .~ 3 & title .~ "Sub-subtask"
            taskFile = createTaskFile [project, subtask1, subtask2]
            fileState = M.fromList [("/test.org", ParserSuccess taskFile)]

            -- Test finding project for subtask1
            subtask1Ptr = TaskPointer "/test.org" 1
            result1 = getProjectForTask subtask1Ptr fileState

            -- Test finding project for sub-subtask
            subtask2Ptr = TaskPointer "/test.org" 2
            result2 = getProjectForTask subtask2Ptr fileState

        result1 `shouldBe` Just (TaskPointer "/test.org" 0)
        result2 `shouldBe` Just (TaskPointer "/test.org" 0)

      it "returns Nothing for top-level project" $ do
        let project = sampleProject
            taskFile = createTaskFile [project]
            fileState = M.fromList [("/test.org", ParserSuccess taskFile)]

            projectPtr = TaskPointer "/test.org" 0
            result = getProjectForTask projectPtr fileState

        result `shouldBe` Nothing

      it "finds nested project for deeply nested subtask" $ do
        let mainProject = sampleProject & title .~ "Main Project"
            subProject = sampleProject & level .~ 2 & title .~ "Sub Project"
            deepSubtask = sampleTask & level .~ 3 & title .~ "Deep Subtask"
            taskFile = createTaskFile [mainProject, subProject, deepSubtask]
            fileState = M.fromList [("/test.org", ParserSuccess taskFile)]

            deepSubtaskPtr = TaskPointer "/test.org" 2
            result = getProjectForTask deepSubtaskPtr fileState

        -- Should find the immediate parent project (Sub Project), not Main Project
        result `shouldBe` Just (TaskPointer "/test.org" 1)

    describe "getProjectSubtasks" $ do
      it "finds all direct subtasks of project" $ do
        let project = sampleProject
            subtask1 = sampleTask & level .~ 2 & title .~ "Subtask 1"
            subtask2 = sampleTask & level .~ 2 & title .~ "Subtask 2"
            otherTask = sampleTask & level .~ 1 & title .~ "Other Task"
            taskFile = createTaskFile [project, subtask1, subtask2, otherTask]
            fileState = M.fromList [("/test.org", ParserSuccess taskFile)]

            projectPtr = TaskPointer "/test.org" 0
            result = getProjectSubtasks projectPtr fileState

        length result `shouldBe` 2
        result `shouldContain` [TaskPointer "/test.org" 1, TaskPointer "/test.org" 2]

      it "finds nested subtasks correctly" $ do
        let project = sampleProject
            subtask1 = sampleTask & level .~ 2 & title .~ "Subtask 1"
            subSubtask = sampleTask & level .~ 3 & title .~ "Sub-subtask"
            subtask2 = sampleTask & level .~ 2 & title .~ "Subtask 2"
            taskFile = createTaskFile [project, subtask1, subSubtask, subtask2]
            fileState = M.fromList [("/test.org", ParserSuccess taskFile)]

            projectPtr = TaskPointer "/test.org" 0
            result = getProjectSubtasks projectPtr fileState

        length result `shouldBe` 3
        result
          `shouldContain` [ TaskPointer "/test.org" 1
                          , TaskPointer "/test.org" 2
                          , TaskPointer "/test.org" 3
                          ]

      it "stops at same or lower level tasks" $ do
        let project1 = sampleProject & title .~ "Project 1"
            subtask = sampleTask & level .~ 2
            project2 = sampleProject & title .~ "Project 2"
            taskFile = createTaskFile [project1, subtask, project2]
            fileState = M.fromList [("/test.org", ParserSuccess taskFile)]

            project1Ptr = TaskPointer "/test.org" 0
            result = getProjectSubtasks project1Ptr fileState

        -- Should only find subtask, not project2
        result `shouldBe` [TaskPointer "/test.org" 1]

    describe "getAllProjectPointers" $ do
      it "finds all PROJECTS tasks across multiple files" $ do
        let project1 = sampleProject & title .~ "Project 1"
            regularTask = sampleTask
            project2 = sampleProject & title .~ "Project 2"

            file1 = createTaskFile [project1, regularTask]
            file2 = createTaskFile [project2]

            fileState =
              M.fromList
                [ ("/file1.org", ParserSuccess file1)
                , ("/file2.org", ParserSuccess file2)
                ]
            result = getAllProjectPointers fileState

        length result `shouldBe` 2
        result
          `shouldContain` [ TaskPointer "/file1.org" 0
                          , TaskPointer "/file2.org" 0
                          ]

      it "returns empty list when no projects exist" $ do
        let regularTask = sampleTask
            taskFile = createTaskFile [regularTask]
            fileState = M.fromList [("/test.org", ParserSuccess taskFile)]

            result = getAllProjectPointers fileState

        result `shouldBe` []
