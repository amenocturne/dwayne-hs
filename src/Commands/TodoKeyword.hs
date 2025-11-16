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

import Commands.Builders (mkTodoKeywordCommand)
import Commands.Command (Command)
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

-- | Predefined commands for each TODO keyword
inboxCommand :: Command Task
inboxCommand = mkTodoKeywordCommand orgInboxKeyword "ti" "keywordInbox"

relevantCommand :: Command Task
relevantCommand = mkTodoKeywordCommand orgRelevantKeyword "tr" "keywordRelevant"

somedayCommand :: Command Task
somedayCommand = mkTodoKeywordCommand orgSomedayKeyword "ts" "keywordSomeday"

notesCommand :: Command Task
notesCommand = mkTodoKeywordCommand orgNotesKeyword "tn" "keywordNotes"

listCommand :: Command Task
listCommand = mkTodoKeywordCommand orgListKeyword "tl" "keywordList"

waitingCommand :: Command Task
waitingCommand = mkTodoKeywordCommand orgWaitingKeyword "tw" "keywordWaiting"

projectCommand :: Command Task
projectCommand = mkTodoKeywordCommand orgProjectKeyword "tp" "keywordProject"

todoCommand :: Command Task
todoCommand = mkTodoKeywordCommand orgTodoKeyword "tt" "keywordTodo"

doneCommand :: Command Task
doneCommand = mkTodoKeywordCommand orgDoneKeyword "td" "keywordDone"

trashCommand :: Command Task
trashCommand = mkTodoKeywordCommand orgTrashKeyword "tx" "keywordTrash"
