module Commands.Registry (allCommands) where

import Commands.AddTask (addTaskCommand)
import Commands.CmdMode
  ( abortCmdCommand,
    applyCmdCommand,
    cmdDeleteCharCommand,
    switchToCmdModeCommand,
    switchToSearchModeCommand,
  )
import Commands.Command (Command (..))
import Commands.Edit (editTaskCommand)
import Commands.ErrorDialog
  ( errorDialogAcceptCommand,
    errorDialogQuitCommand,
  )
import Commands.Macros (musicMacroCommand)
import Commands.Navigation
  ( cleanKeyStateCommand,
    jumpBackwardCommand,
    jumpForwardCommand,
    jumpToBeginningCommand,
    jumpToEndCommand,
    moveDownCommand,
    moveUpCommand,
  )
import Commands.OpenUrl (openUrlCommand)
import Commands.Priority (priorityDownCommand, priorityUpCommand)
import Commands.Projects (goToProjectsCommand)
import Commands.Refile (refileCommand)
import Commands.RefreshView (refreshViewCommand)
import Commands.SelectionMode
  ( enterSelectionModeCommand,
    exitSelectionModeCommand,
    toggleCurrentSelectionCommand,
    toggleCurrentSelectionInSelectionCommand,
    toggleRangeSelectionCommand,
  )
import Commands.Sort
  ( sortCreatedAscCommand,
    sortCreatedDescCommand,
    sortPriorityAscCommand,
    sortPriorityDescCommand,
  )
import Commands.Tags
  ( addBookTagCommand,
    addCoolTagCommand,
    addMusicTagCommand,
    addSoftwareTagCommand,
    deleteBookTagCommand,
    deleteCoolTagCommand,
    deleteMusicTagCommand,
    deleteSoftwareTagCommand,
  )
import Commands.TodoKeyword
  ( doneCommand,
    inboxCommand,
    listCommand,
    notesCommand,
    projectCommand,
    relevantCommand,
    somedayCommand,
    todoCommand,
    trashCommand,
    waitingCommand,
  )
import Commands.UndoRedo (redoCommand, undoCommand)
import Commands.ValidationDialog
  ( validationDialogAcceptCommand,
    validationDialogRejectCommand,
  )
import Commands.Views
  ( viewAllCommand,
    viewDoneCommand,
    viewInboxCommand,
    viewListCommand,
    viewNotesCommand,
    viewProjectCommand,
    viewRelevantCommand,
    viewSomedayCommand,
    viewTodoCommand,
    viewTrashCommand,
    viewWaitingCommand,
  )
import Model.OrgMode (Task)

-- | All available commands in the application
allCommands :: [Command Task]
allCommands =
  [ -- Core commands
    goToProjectsCommand,
    addTaskCommand,
    editTaskCommand,
    refileCommand,
    openUrlCommand,
    refreshViewCommand,
    -- TODO Keyword commands
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
    -- Tag commands
    addMusicTagCommand,
    deleteMusicTagCommand,
    addCoolTagCommand,
    deleteCoolTagCommand,
    addSoftwareTagCommand,
    deleteSoftwareTagCommand,
    addBookTagCommand,
    deleteBookTagCommand,
    -- Priority commands
    priorityUpCommand,
    priorityDownCommand,
    -- View commands
    viewAllCommand,
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
    -- Navigation commands
    jumpToEndCommand,
    jumpToBeginningCommand,
    jumpBackwardCommand,
    jumpForwardCommand,
    -- Sort commands
    sortCreatedAscCommand,
    sortCreatedDescCommand,
    sortPriorityAscCommand,
    sortPriorityDescCommand,
    -- Selection mode commands
    enterSelectionModeCommand,
    toggleCurrentSelectionCommand,
    exitSelectionModeCommand,
    toggleRangeSelectionCommand,
    toggleCurrentSelectionInSelectionCommand,
    -- Cmd/Search Mode commands
    switchToSearchModeCommand,
    switchToCmdModeCommand,
    abortCmdCommand,
    cmdDeleteCharCommand,
    applyCmdCommand,
    -- Dialog commands
    errorDialogQuitCommand,
    errorDialogAcceptCommand,
    validationDialogAcceptCommand,
    validationDialogRejectCommand,
    -- Undo/Redo commands
    undoCommand,
    redoCommand,
    -- Movement commands
    moveUpCommand,
    moveDownCommand,
    cleanKeyStateCommand,
    -- Macros
    musicMacroCommand
  ]
