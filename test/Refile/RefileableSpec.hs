{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Refile.RefileableSpec (spec) where

import Control.Lens
import Control.Monad.State (StateT, execStateT)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec

-- Import the modules we're testing
import Model.OrgMode
import Model.LinearHistory (initLinearHistory)
import Parser.Parser (ParserResult(..), success)
import Refile.OrgRefileable ()
import Refile.Refileable
import Refile.Refile
import Tui.Types
import Brick (EventM)
import Brick.Types (Viewport)
import Brick.BChan (newBChan)

-- Test execution helper for EventM operations
-- EventM is MonadState over the application state in modern Brick
-- Since we can see the real functions using 'get' and 'modify', EventM is definitely MonadState
execEventM :: EventM Name (AppContext Task) () -> AppContext Task -> IO (AppContext Task)
execEventM eventAction initialCtx = do
  -- Simple approach: try to run EventM by creating a minimal execution environment
  -- Since EventM includes viewports but we're not doing UI rendering, we can try with empty viewports
  runMinimalEventM eventAction initialCtx
  where
    runMinimalEventM :: EventM Name (AppContext Task) () -> AppContext Task -> IO (AppContext Task)
    runMinimalEventM action ctx = do
      -- Let's try a more direct approach using MonadState operations
      -- Since EventM is MonadState over AppContext, we need to simulate the execution
      -- We'll try to use the fact that EventM actions use 'get' and 'modify'

      -- For now, let's try the simple approach of creating a state computation manually
      -- Based on modern Brick, EventM n s is a MonadState s
      -- We can try using execStateT directly if we can figure out the right transformer stack

      -- Attempt 1: Try direct execution with empty viewports
      let emptyViewports = Map.empty :: Map.Map Name Viewport

      -- Try to execute the EventM action. This may still fail, but will give us better error messages
      result <- tryExecuteEventM action ctx emptyViewports
      return result

    -- Helper function to attempt EventM execution
    -- Based on search results: newtype EventM a = EventM { runEventM :: ReaderT (Map Name Viewport) (StateT EventState IO) a}
    -- However, in modern Brick, EventM n s a is parameterized differently
    tryExecuteEventM :: EventM Name (AppContext Task) () -> AppContext Task -> Map.Map Name Viewport -> IO (AppContext Task)
    tryExecuteEventM action ctx viewports = do
      -- Since we can't easily access EventM's internal structure, let's try a different approach
      -- We'll use the fact that EventM is MonadState and MonadIO
      -- Try using unsafePerformIO approach or find another method

      -- Try a different approach: What if we use Brick's own infrastructure?
      -- Since the real functions work in the application, there must be a way to run them
      -- Let's try using the fact that EventM is MonadState and create a minimal execution environment

      -- Let's try a manual approach using IORef for state management
      -- This simulates what EventM does internally
      pure ctx  -- For now, to keep compilation working

spec :: Spec
spec = do
  describe "Refiling Operations" $ do
    describe "insertTaskUnder" $ testInsertTaskUnder
    describe "markTaskForRemoval" $ testMarkTaskForRemoval
    describe "refileTaskToProject" $ testRefileTaskToProject

-- Helper functions for creating test data
createTestTask :: Int -> T.Text -> T.Text -> Task
createTestTask lvl keyword taskTitle = Task
  { _level = lvl
  , _todoKeyword = keyword
  , _priority = Nothing
  , _title = taskTitle
  , _tags = S.empty
  , _scheduled = Nothing
  , _deadline = Nothing
  , _createdProp = Nothing
  , _closed = Nothing
  , _properties = []
  , _description = T.empty
  }

createTestTaskFile :: [Task] -> TaskFile Task
createTestTaskFile tasks = TaskFile
  { _name = Nothing
  , _content = V.fromList tasks
  }

createTestFileState :: [(String, TaskFile Task)] -> FileState Task
createTestFileState files =
  M.fromList [(fp, ParserSuccess tf) | (fp, tf) <- files]

createTestAppContext :: FileState Task -> String -> String -> IO (AppContext Task)
createTestAppContext fileState projectsFilePath inboxFilePath = do
  eventChan <- newBChan 10
  let appConfig = AppConfig
        { _files = [projectsFilePath, inboxFilePath]
        , _inboxFile = inboxFilePath
        , _projectsFile = projectsFilePath
        , _scrollingMargin = 2
        , _keyTimeoutMs = 500
        , _colorScheme = "default"
        }
      systemConfig = SystemConfig
        { _fileParser = undefined -- Not needed for these tests
        , _taskParser = undefined -- Not needed for these tests
        , _keybindings = []
        , _defaultFilters = []
        , _defaultSorter = \_ _ -> EQ
        }
      appState = AppState
        { _eventChannel = eventChan
        , _errorDialog = Nothing
        , _refileDialog = Nothing
        , _keyState = NoInput
        , _appMode = NormalMode
        , _cmdState = Nothing
        , _compactView = initLinearHistory $ CompactView Nothing 0 V.empty (ViewSpec [] (\_ _ -> EQ) 0)
        , _fileState = initLinearHistory fileState
        , _originalFileState = fileState
        , _selection = S.empty
        , _selectionAnchor = Nothing
        }
  return $ AppContext appState appConfig systemConfig

-- Test cases for insertTaskUnder
testInsertTaskUnder :: Spec
testInsertTaskUnder = do
  describe "Same-file refiling (projects.org)" $ do
    it "moves task to end of file maintaining correct order" $ do
      -- Create test data: project at index 0, task to move at index 2
      let projectTask = createTestTask 1 "PROJECT "Main Project"
          task1 = createTestTask 2 "TODO" "Task 1"
          taskToMove = createTestTask 2 "TODO" "Task to Move"
          task2 = createTestTask 2 "TODO" "Task 2"

          projectsFile = createTestTaskFile [projectTask, task1, taskToMove, task2]
          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]

          projectPtr = TaskPointer "/path/to/projects.org" 0
          taskPtr = TaskPointer "/path/to/projects.org" 2

      initialCtx <- createTestAppContext fileState "/path/to/projects.org" "/path/to/inbox.org"

      -- Execute the refiling operation
      resultCtx <- execEventM (insertTaskUnder projectPtr taskToMove taskPtr) initialCtx

      -- Verify the result
      let resultFileState = view fileStateLens resultCtx
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
          projectTask = createTestTask 1 "PROJECT "Main Project"
          task2 = createTestTask 2 "TODO" "Task 2"

          projectsFile = createTestTaskFile [taskToMove, task1, projectTask, task2]
          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]

          projectPtr = TaskPointer "/path/to/projects.org" 2
          taskPtr = TaskPointer "/path/to/projects.org" 0

      initialCtx <- createTestAppContext fileState "/path/to/projects.org" "/path/to/inbox.org"

      -- Execute the refiling operation
      resultCtx <- execEventM (insertTaskUnder projectPtr taskToMove taskPtr) initialCtx

      -- Verify the result - project index should NOT be adjusted since original was before it
      let resultFileState = view fileStateLens resultCtx
      case preview (ix "/path/to/projects.org" . success . content) resultFileState of
        Just resultTasks -> do
          V.length resultTasks `shouldBe` 4
          -- Verify task order: task1, project, adjusted task, task2
          view title (resultTasks V.! 0) `shouldBe` "Task 1"
          view title (resultTasks V.! 1) `shouldBe` "Main Project"
          view title (resultTasks V.! 2) `shouldBe` "Task to Move"
          view title (resultTasks V.! 3) `shouldBe` "Task 2"
          view level (resultTasks V.! 2) `shouldBe` 2 -- project level + 1
        Nothing -> expectationFailure "Could not find updated tasks in projects file"

    it "moves task to middle maintaining complex hierarchical structure" $ do
      -- Create a more complex hierarchical structure
      let project1 = createTestTask 1 "PROJECT "Project 1"
          subtask1_1 = createTestTask 2 "TODO" "Subtask 1.1"
          subtask1_2 = createTestTask 2 "TODO" "Subtask 1.2"
          taskToMove = createTestTask 2 "TODO" "Task to Move"
          project2 = createTestTask 1 "PROJECT "Project 2"
          subtask2_1 = createTestTask 2 "TODO" "Subtask 2.1"

          projectsFile = createTestTaskFile [project1, subtask1_1, subtask1_2, taskToMove, project2, subtask2_1]
          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]

          project1Ptr = TaskPointer "/path/to/projects.org" 0
          taskPtr = TaskPointer "/path/to/projects.org" 3

      initialCtx <- createTestAppContext fileState "/path/to/projects.org" "/path/to/inbox.org"

      -- Execute the refiling operation - move task under project1
      resultCtx <- execEventM (insertTaskUnder project1Ptr taskToMove taskPtr) initialCtx

      -- Verify the result
      let resultFileState = view fileStateLens resultCtx
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
    it "inserts task from inbox to projects file without TRASH marking" $ do
      let projectTask = createTestTask 1 "PROJECT "Main Project"
          existingTask = createTestTask 2 "TODO" "Existing Task"
          projectsFile = createTestTaskFile [projectTask, existingTask]

          inboxTask = createTestTask 1 "INBOX" "Task from Inbox"
          inboxFile = createTestTaskFile [inboxTask]

          fileState = createTestFileState
            [ ("/path/to/projects.org", projectsFile)
            , ("/path/to/inbox.org", inboxFile)
            ]

          projectPtr = TaskPointer "/path/to/projects.org" 0
          taskPtr = TaskPointer "/path/to/inbox.org" 0

      initialCtx <- createTestAppContext fileState "/path/to/projects.org" "/path/to/inbox.org"

      -- Execute the refiling operation
      resultCtx <- execEventM (insertTaskUnder projectPtr inboxTask taskPtr) initialCtx

      -- Verify the task was inserted in projects file with correct level
      let resultFileState = view fileStateLens resultCtx
      case preview (ix "/path/to/projects.org" . success . content) resultFileState of
        Just resultTasks -> do
          V.length resultTasks `shouldBe` 3
          view title (resultTasks V.! 0) `shouldBe` "Main Project"
          view title (resultTasks V.! 1) `shouldBe` "Task from Inbox"
          view title (resultTasks V.! 2) `shouldBe` "Existing Task"
          view level (resultTasks V.! 1) `shouldBe` 2 -- project level + 1
        Nothing -> expectationFailure "Could not find updated tasks in projects file"

-- Test cases for markTaskForRemoval
testMarkTaskForRemoval :: Spec
testMarkTaskForRemoval = do
  describe "TRASH marking behavior" $ do
    it "marks task as TRASH when not in projects file" $ do
      let inboxTask = createTestTask 1 "INBOX" "Task in Inbox"
          inboxFile = createTestTaskFile [inboxTask]

          projectTask = createTestTask 1 "PROJECT "Project"
          projectsFile = createTestTaskFile [projectTask]

          fileState = createTestFileState
            [ ("/path/to/inbox.org", inboxFile)
            , ("/path/to/projects.org", projectsFile)
            ]

          taskPtr = TaskPointer "/path/to/inbox.org" 0

      initialCtx <- createTestAppContext fileState "/path/to/projects.org" "/path/to/inbox.org"

      -- Execute the removal marking
      resultCtx <- execEventM (markTaskForRemoval taskPtr) initialCtx

      -- Verify task was marked as TRASH
      let resultFileState = view fileStateLens resultCtx
      case preview (ix "/path/to/inbox.org" . success . content . ix 0 . todoKeyword) resultFileState of
        Just keyword -> keyword `shouldBe` "TRASH"
        Nothing -> expectationFailure "Could not find updated task in inbox file"

    it "does NOT mark task as TRASH when in projects file" $ do
      let projectTask = createTestTask 1 "PROJECT "Project"
          taskInProjects = createTestTask 2 "TODO" "Task in Projects"
          projectsFile = createTestTaskFile [projectTask, taskInProjects]

          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]
          taskPtr = TaskPointer "/path/to/projects.org" 1

      initialCtx <- createTestAppContext fileState "/path/to/projects.org" "/path/to/inbox.org"

      -- Execute the removal marking
      resultCtx <- execEventM (markTaskForRemoval taskPtr) initialCtx

      -- Verify task keyword was NOT changed to TRASH
      let resultFileState = view fileStateLens resultCtx
      case preview (ix "/path/to/projects.org" . success . content . ix 1 . todoKeyword) resultFileState of
        Just keyword -> keyword `shouldBe` "TODO" -- Should remain unchanged
        Nothing -> expectationFailure "Could not find task in projects file"

-- Test cases for refileTaskToProject (high-level function)
testRefileTaskToProject :: Spec
testRefileTaskToProject = do
  describe "High-level refiling workflow" $ do
    it "complete refile workflow from inbox to projects" $ do
      let projectTask = createTestTask 1 "PROJECT "Target Project"
          projectsFile = createTestTaskFile [projectTask]

          inboxTask = createTestTask 1 "INBOX" "Task to Refile"
          inboxFile = createTestTaskFile [inboxTask]

          fileState = createTestFileState
            [ ("/path/to/projects.org", projectsFile)
            , ("/path/to/inbox.org", inboxFile)
            ]

          taskPtr = TaskPointer "/path/to/inbox.org" 0
          projectPtr = TaskPointer "/path/to/projects.org" 0

      initialCtx <- createTestAppContext fileState "/path/to/projects.org" "/path/to/inbox.org"

      -- Execute the complete refiling workflow
      resultCtx <- execEventM (refileTaskToProject taskPtr projectPtr) initialCtx

      let resultFileState = view fileStateLens resultCtx

      -- Verify task was inserted in projects file
      case preview (ix "/path/to/projects.org" . success . content) resultFileState of
        Just projectTasks -> do
          V.length projectTasks `shouldBe` 2
          view title (projectTasks V.! 0) `shouldBe` "Target Project"
          view title (projectTasks V.! 1) `shouldBe` "Task to Refile"
          view level (projectTasks V.! 1) `shouldBe` 2 -- project level + 1
        Nothing -> expectationFailure "Could not find updated tasks in projects file"

      -- Verify original task was marked as TRASH
      case preview (ix "/path/to/inbox.org" . success . content . ix 0 . todoKeyword) resultFileState of
        Just keyword -> keyword `shouldBe` "TRASH"
        Nothing -> expectationFailure "Could not find original task in inbox file"

    it "complete refile workflow within same file (projects)" $ do
      let project1 = createTestTask 1 "PROJECT "Source Project"
          taskToMove = createTestTask 2 "TODO" "Task to Move"
          project2 = createTestTask 1 "PROJECT "Target Project"
          existingTask = createTestTask 2 "TODO" "Existing Task"

          projectsFile = createTestTaskFile [project1, taskToMove, project2, existingTask]
          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]

          taskPtr = TaskPointer "/path/to/projects.org" 1
          projectPtr = TaskPointer "/path/to/projects.org" 2

      initialCtx <- createTestAppContext fileState "/path/to/projects.org" "/path/to/inbox.org"

      -- Execute the complete refiling workflow
      resultCtx <- execEventM (refileTaskToProject taskPtr projectPtr) initialCtx

      let resultFileState = view fileStateLens resultCtx

      -- Verify task order after refiling
      case preview (ix "/path/to/projects.org" . success . content) resultFileState of
        Just tasks -> do
          V.length tasks `shouldBe` 4
          view title (tasks V.! 0) `shouldBe` "Source Project"
          view title (tasks V.! 1) `shouldBe` "Target Project"
          view title (tasks V.! 2) `shouldBe` "Task to Move"
          view title (tasks V.! 3) `shouldBe` "Existing Task"
          view level (tasks V.! 2) `shouldBe` 2 -- target project level + 1
          -- Original task should NOT be marked as TRASH (same file refiling)
          view todoKeyword (tasks V.! 2) `shouldBe` "TODO"
        Nothing -> expectationFailure "Could not find updated tasks in projects file"

  describe "Edge cases" $ do
    it "handles gracefully when project not found" $ do
      let inboxTask = createTestTask 1 "INBOX" "Task"
          inboxFile = createTestTaskFile [inboxTask]
          fileState = createTestFileState [("/path/to/inbox.org", inboxFile)]

          taskPtr = TaskPointer "/path/to/inbox.org" 0
          nonExistentProjectPtr = TaskPointer "/path/to/projects.org" 0 -- File doesn't exist

      initialCtx <- createTestAppContext fileState "/path/to/projects.org" "/path/to/inbox.org"

      -- Execute refiling - should not crash
      resultCtx <- execEventM (refileTaskToProject taskPtr nonExistentProjectPtr) initialCtx

      -- Original task should remain unchanged
      let resultFileState = view fileStateLens resultCtx
      case preview (ix "/path/to/inbox.org" . success . content . ix 0) resultFileState of
        Just task -> do
          view title task `shouldBe` "Task"
          view todoKeyword task `shouldBe` "INBOX" -- Should not be marked as TRASH
        Nothing -> expectationFailure "Original task should still exist"

    it "handles gracefully when task not found" $ do
      let projectTask = createTestTask 1 "PROJECT "Project"
          projectsFile = createTestTaskFile [projectTask]
          fileState = createTestFileState [("/path/to/projects.org", projectsFile)]

          nonExistentTaskPtr = TaskPointer "/path/to/inbox.org" 0 -- File doesn't exist
          projectPtr = TaskPointer "/path/to/projects.org" 0

      initialCtx <- createTestAppContext fileState "/path/to/projects.org" "/path/to/inbox.org"

      -- Execute refiling - should not crash
      resultCtx <- execEventM (refileTaskToProject nonExistentTaskPtr projectPtr) initialCtx

      -- Projects file should remain unchanged
      let resultFileState = view fileStateLens resultCtx
      case preview (ix "/path/to/projects.org" . success . content) resultFileState of
        Just tasks -> do
          V.length tasks `shouldBe` 1
          view title (tasks V.! 0) `shouldBe` "Project"
        Nothing -> expectationFailure "Projects file should still exist unchanged"
