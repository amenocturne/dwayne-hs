{-# LANGUAGE OverloadedStrings #-}

module Commands.Views
  ( viewAllCommand,
    viewInboxCommand,
    viewTodayCommand,
    viewTodoCommand,
    viewSoonCommand,
    viewRelevantCommand,
    viewSomedayCommand,
    viewNotesCommand,
    viewListCommand,
    viewWaitingCommand,
    viewProjectCommand,
    viewDoneCommand,
    viewTrashCommand,
    workQueueCommand,
  )
where

import Api.Handlers (makeKeywordViewHandler, makeViewHandler, viewAllHandler)
import Api.Types (ApiBinding (..), ApiMethod (..))
import Commands.CliHelpers (formatTaskLine, loadFileState)
import Commands.Command (Command (..), TuiBinding (..))
import qualified Commands.Projects as CmdProjects
import Control.Monad (forM_)
import Core.Filters (computeFilteredSortedView)
import qualified Data.Text as T
import qualified Data.Vector as V
import Model.OrgMode
  ( Task,
    orgDoneKeyword,
    orgInboxKeyword,
    orgListKeyword,
    orgNotesKeyword,
    orgProjectKeyword,
    orgRelevantKeyword,
    orgSomedayKeyword,
    orgSoonKeyword,
    orgTodayKeyword,
    orgTodoKeyword,
    orgTrashKeyword,
    orgWaitingKeyword,
  )
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import qualified Tui.Keybindings as KB
import Tui.Types (AppMode (NormalMode), KeyEvent (..))

-- | Generic builder for view filter commands with keyword filtering
-- Creates a command that filters tasks by TODO keyword with optional sorting
viewCommand ::
  -- | TODO keyword to filter by (e.g., "INBOX", "RELEVANT")
  T.Text ->
  -- | Key sequence (e.g., " ai" for view inbox)
  String ->
  -- | Command alias (e.g., "viewInbox")
  T.Text ->
  -- | API endpoint path (e.g., "views/inbox")
  T.Text ->
  -- | Optional sorting function
  Maybe (Task -> Task -> Ordering) ->
  Command Task
viewCommand keyword keySeq alias endpoint maybeSorter =
  Command
    { cmdName = "View Filter",
      cmdAlias = alias,
      cmdDescription = T.concat ["Show ", keyword, " tasks"],
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = View keyword,
              tuiKeybinding = KB.toKeySeq keySeq,
              tuiDescription = T.concat ["Show ", keyword, " tasks"],
              tuiAction = do
                CmdProjects.saveForJump $ Helpers.applyFilterToAllTasks (KB.todoKeywordFilter keyword)
                forM_ maybeSorter Helpers.applySorter,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Just $ pure $ do
        (_, fState) <- loadFileState
        let filters = [KB.todoKeywordFilter keyword]
            sorter = maybe (\_ _ -> EQ) id maybeSorter
            results = computeFilteredSortedView filters sorter fState
        mapM_ (\(task, ptr) -> putStrLn $ formatTaskLine task ptr) (V.toList results),
      cmdApi =
        Just $
          ApiBinding
            { apiEndpoint = endpoint,
              apiMethod = GET,
              apiHandler = makeKeywordViewHandler keyword maybeSorter
            }
    }

-- | View all tasks command
viewAllCommand :: Command Task
viewAllCommand =
  Command
    { cmdName = "View All",
      cmdAlias = "viewAll",
      cmdDescription = "Show all tasks",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = View "all",
              tuiKeybinding = KB.toKeySeq " aa",
              tuiDescription = "Show all tasks",
              tuiAction = CmdProjects.saveForJump $ Helpers.applyFilterToAllTasks (const True),
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Just $ pure $ do
        (_, fState) <- loadFileState
        let results = computeFilteredSortedView [] (\_ _ -> EQ) fState
        mapM_ (\(task, ptr) -> putStrLn $ formatTaskLine task ptr) (V.toList results),
      cmdApi =
        Just $
          ApiBinding
            { apiEndpoint = "views/all",
              apiMethod = GET,
              apiHandler = viewAllHandler
            }
    }

-- | View INBOX tasks
viewInboxCommand :: Command Task
viewInboxCommand = viewCommand orgInboxKeyword " ai" "viewInbox" "views/inbox" (Just KB.sortByCreatedDesc)

-- | View TODAY tasks
viewTodayCommand :: Command Task
viewTodayCommand = viewCommand orgTodayKeyword " at" "viewToday" "views/today" (Just KB.sortByPriorityAsc)

-- | View SOON tasks
viewSoonCommand :: Command Task
viewSoonCommand = viewCommand orgSoonKeyword " an" "viewSoon" "views/soon" (Just KB.sortByPriorityAsc)

-- | View RELEVANT tasks
viewRelevantCommand :: Command Task
viewRelevantCommand = viewCommand orgRelevantKeyword " aR" "viewRelevant" "views/relevant" (Just KB.sortByPriorityAsc)

-- | View SOMEDAY tasks
viewSomedayCommand :: Command Task
viewSomedayCommand = viewCommand orgSomedayKeyword " as" "viewSomeday" "views/someday" Nothing

-- | View NOTES tasks
viewNotesCommand :: Command Task
viewNotesCommand = viewCommand orgNotesKeyword " aN" "viewNotes" "views/notes" Nothing

-- | View LIST tasks
viewListCommand :: Command Task
viewListCommand = viewCommand orgListKeyword " al" "viewList" "views/list" Nothing

-- | View WAITING tasks
viewWaitingCommand :: Command Task
viewWaitingCommand = viewCommand orgWaitingKeyword " aw" "viewWaiting" "views/waiting" (Just KB.sortByPriorityAsc)

-- | View PROJECT tasks
viewProjectCommand :: Command Task
viewProjectCommand = viewCommand orgProjectKeyword " ap" "viewProject" "views/project" (Just KB.sortByPriorityAsc)

-- | View TODO tasks
viewTodoCommand :: Command Task
viewTodoCommand = viewCommand orgTodoKeyword " ao" "viewTodo" "views/todo" (Just KB.sortByPriorityAsc)

-- | View DONE tasks
viewDoneCommand :: Command Task
viewDoneCommand = viewCommand orgDoneKeyword " ad" "viewDone" "views/done" Nothing

-- | View TRASH tasks
viewTrashCommand :: Command Task
viewTrashCommand = viewCommand orgTrashKeyword " ax" "viewTrash" "views/trash" Nothing

workQueueCommand :: Command Task
workQueueCommand =
  Command
    { cmdName = "Work Queue",
      cmdAlias = "viewWorkQueue",
      cmdDescription = "Show TODAY and SOON tasks by priority and deadline",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = View "work-queue",
              tuiKeybinding = KB.toKeySeq " aq",
              tuiDescription = "Show work queue (TODAY + SOON)",
              tuiAction = do
                CmdProjects.saveForJump $ Helpers.applyFilterToAllTasks workQueueFilter
                Helpers.applySorter KB.sortByPriorityThenDeadline,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Just $ pure $ do
        (_, fState) <- loadFileState
        let results = computeFilteredSortedView [workQueueFilter] KB.sortByPriorityThenDeadline fState
        mapM_ (\(task, ptr) -> putStrLn $ formatTaskLine task ptr) (V.toList results),
      cmdApi =
        Just $
          ApiBinding
            { apiEndpoint = "views/work-queue",
              apiMethod = GET,
              apiHandler = makeViewHandler [workQueueFilter] KB.sortByPriorityThenDeadline
            }
    }

workQueueFilter :: Task -> Bool
workQueueFilter task = KB.todoKeywordFilter orgTodayKeyword task || KB.todoKeywordFilter orgSoonKeyword task
