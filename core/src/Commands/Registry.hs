module Commands.Registry (allCommands, getEnabledCommands) where

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
import Data.Aeson (Object, Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
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

-- | Filter commands based on optional commands config object
-- Commands are enabled by default unless explicitly set to false in config
-- If Nothing, all commands are enabled
-- If Just object, only commands with value false are disabled
getEnabledCommands :: Maybe Object -> [Command Task]
getEnabledCommands Nothing = allCommands
getEnabledCommands (Just commandsObj) =
  filter (isCommandEnabled commandsObj) allCommands
  where
    isCommandEnabled :: Object -> Command Task -> Bool
    isCommandEnabled obj cmd =
      case KM.lookup (K.fromText $ cmdAlias cmd) obj of
        Nothing -> True -- Not specified = enabled by default
        Just (Bool False) -> False -- Explicitly disabled
        Just (Bool True) -> True -- Explicitly enabled
        _ -> True -- Invalid value = enabled by default
