{-# LANGUAGE OverloadedStrings #-}

module Commands.Views
  ( viewAllCommand,
    viewInboxCommand,
    viewRelevantCommand,
    viewSomedayCommand,
    viewNotesCommand,
    viewListCommand,
    viewWaitingCommand,
    viewProjectCommand,
    viewTodoCommand,
    viewDoneCommand,
    viewTrashCommand,
  )
where

import Api.Handlers (makeKeywordViewHandler, viewAllHandler)
import Api.Types (ApiBinding (..), ApiMethod (..))
import Commands.Command (Command (..), TuiBinding (..))
import qualified Commands.Projects as CmdProjects
import Control.Monad (forM_)
import qualified Data.Text as T
import Model.OrgMode
  ( Task,
    orgDoneKeyword,
    orgInboxKeyword,
    orgListKeyword,
    orgNotesKeyword,
    orgProjectKeyword,
    orgRelevantKeyword,
    orgSomedayKeyword,
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
      cmdCli = Nothing,
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
      cmdCli = Nothing,
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

-- | View RELEVANT tasks
viewRelevantCommand :: Command Task
viewRelevantCommand = viewCommand orgRelevantKeyword " ar" "viewRelevant" "views/relevant" (Just KB.sortByPriorityAsc)

-- | View SOMEDAY tasks
viewSomedayCommand :: Command Task
viewSomedayCommand = viewCommand orgSomedayKeyword " as" "viewSomeday" "views/someday" Nothing

-- | View NOTES tasks
viewNotesCommand :: Command Task
viewNotesCommand = viewCommand orgNotesKeyword " an" "viewNotes" "views/notes" Nothing

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
viewTodoCommand = viewCommand orgTodoKeyword " at" "viewTodo" "views/todo" (Just KB.sortByPriorityAsc)

-- | View DONE tasks
viewDoneCommand :: Command Task
viewDoneCommand = viewCommand orgDoneKeyword " ad" "viewDone" "views/done" Nothing

-- | View TRASH tasks
viewTrashCommand :: Command Task
viewTrashCommand = viewCommand orgTrashKeyword " ax" "viewTrash" "views/trash" Nothing
