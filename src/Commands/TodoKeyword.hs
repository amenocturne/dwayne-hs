{-# LANGUAGE OverloadedStrings #-}

module Commands.TodoKeyword
  ( inboxCommand,
    relevantCommand,
    somedayCommand,
    notesCommand,
    listCommand,
    waitingCommand,
    projectCommand,
    todoCommand,
    doneCommand,
    trashCommand,
  )
where

import Commands.Command (Command (..), TuiBinding (..))
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
import Tui.Keybindings (toKeySeq)
import qualified Tui.SelectionMode as SM
import Tui.Types (KeyEvent (..))

-- | Generic builder for TODO keyword change commands
-- Creates a command that changes the TODO keyword of a task or selection
todoKeywordCommand ::
  -- | Target TODO keyword (e.g., "INBOX", "RELEVANT")
  T.Text ->
  -- | Key sequence (e.g., "ti" for inbox)
  String ->
  -- | Command alias (e.g., "keywordInbox")
  T.Text ->
  Command Task
todoKeywordCommand keyword keySeq alias =
  Command
    { cmdName = "Change TODO Keyword",
      cmdAlias = alias,
      cmdDescription = T.concat ["Change the todo keyword of the current task or selection to ", keyword],
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = ChangeTodoKeyword keyword,
              tuiKeybinding = toKeySeq keySeq,
              tuiDescription = T.concat ["Change to ", keyword],
              tuiAction = Helpers.saveForUndo $ SM.smartApplyTodoKeyword keyword,
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Predefined commands for each TODO keyword
inboxCommand :: Command Task
inboxCommand = todoKeywordCommand orgInboxKeyword "ti" "keywordInbox"

relevantCommand :: Command Task
relevantCommand = todoKeywordCommand orgRelevantKeyword "tr" "keywordRelevant"

somedayCommand :: Command Task
somedayCommand = todoKeywordCommand orgSomedayKeyword "ts" "keywordSomeday"

notesCommand :: Command Task
notesCommand = todoKeywordCommand orgNotesKeyword "tn" "keywordNotes"

listCommand :: Command Task
listCommand = todoKeywordCommand orgListKeyword "tl" "keywordList"

waitingCommand :: Command Task
waitingCommand = todoKeywordCommand orgWaitingKeyword "tw" "keywordWaiting"

projectCommand :: Command Task
projectCommand = todoKeywordCommand orgProjectKeyword "tp" "keywordProject"

todoCommand :: Command Task
todoCommand = todoKeywordCommand orgTodoKeyword "tt" "keywordTodo"

doneCommand :: Command Task
doneCommand = todoKeywordCommand orgDoneKeyword "td" "keywordDone"

trashCommand :: Command Task
trashCommand = todoKeywordCommand orgTrashKeyword "tx" "keywordTrash"
