{-# LANGUAGE OverloadedStrings #-}

module Commands.TodoKeyword
  ( changeTodoKeywordCommand,
    inboxCommand,
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
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty.Input.Events as E
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
import qualified Tui.SelectionMode as SM
import Tui.Types
  ( AppContext,
    GlobalAppState,
    KeyEvent (..),
    KeyPress (..),
  )

-- | Generic function to create a command that changes todo keyword
changeTodoKeywordCommand :: T.Text -> String -> T.Text -> Command Task
changeTodoKeywordCommand keyword keySeq alias =
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
  where
    toKeySeq (c : cs) = KeyPress (E.KChar c) Set.empty :| map (\x -> KeyPress (E.KChar x) Set.empty) cs
    toKeySeq [] = error "toKeySeq: empty string"

-- | Predefined commands for each TODO keyword
inboxCommand :: Command Task
inboxCommand = changeTodoKeywordCommand orgInboxKeyword "ti" "keywordInbox"

relevantCommand :: Command Task
relevantCommand = changeTodoKeywordCommand orgRelevantKeyword "tr" "keywordRelevant"

somedayCommand :: Command Task
somedayCommand = changeTodoKeywordCommand orgSomedayKeyword "ts" "keywordSomeday"

notesCommand :: Command Task
notesCommand = changeTodoKeywordCommand orgNotesKeyword "tn" "keywordNotes"

listCommand :: Command Task
listCommand = changeTodoKeywordCommand orgListKeyword "tl" "keywordList"

waitingCommand :: Command Task
waitingCommand = changeTodoKeywordCommand orgWaitingKeyword "tw" "keywordWaiting"

projectCommand :: Command Task
projectCommand = changeTodoKeywordCommand orgProjectKeyword "tp" "keywordProject"

todoCommand :: Command Task
todoCommand = changeTodoKeywordCommand orgTodoKeyword "tt" "keywordTodo"

doneCommand :: Command Task
doneCommand = changeTodoKeywordCommand orgDoneKeyword "td" "keywordDone"

trashCommand :: Command Task
trashCommand = changeTodoKeywordCommand orgTrashKeyword "tx" "keywordTrash"
