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

import Commands.Builders (mkViewCommand)
import Commands.Command (Command (..), TuiBinding (..))
import qualified Commands.Projects as CmdProjects
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
import qualified Tui.Keybindings as KB
import Tui.Types (AppMode (NormalMode), KeyEvent (..), KeyPress (..))

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
              tuiKeybinding = toKeySeq " aa",
              tuiDescription = "Show all tasks",
              tuiAction = CmdProjects.saveForJump $ Helpers.applyFilterToAllTasks (const True),
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
  where
    toKeySeq (c : cs) = KeyPress (E.KChar c) Set.empty :| map (\x -> KeyPress (E.KChar x) Set.empty) cs
    toKeySeq [] = error "toKeySeq: empty string"

-- | View INBOX tasks
viewInboxCommand :: Command Task
viewInboxCommand = mkViewCommand orgInboxKeyword " ai" "viewInbox" (Just KB.sortByCreatedDesc)

-- | View RELEVANT tasks
viewRelevantCommand :: Command Task
viewRelevantCommand = mkViewCommand orgRelevantKeyword " ar" "viewRelevant" (Just KB.sortByPriorityAsc)

-- | View SOMEDAY tasks
viewSomedayCommand :: Command Task
viewSomedayCommand = mkViewCommand orgSomedayKeyword " as" "viewSomeday" Nothing

-- | View NOTES tasks
viewNotesCommand :: Command Task
viewNotesCommand = mkViewCommand orgNotesKeyword " an" "viewNotes" Nothing

-- | View LIST tasks
viewListCommand :: Command Task
viewListCommand = mkViewCommand orgListKeyword " al" "viewList" Nothing

-- | View WAITING tasks
viewWaitingCommand :: Command Task
viewWaitingCommand = mkViewCommand orgWaitingKeyword " aw" "viewWaiting" (Just KB.sortByPriorityAsc)

-- | View PROJECT tasks
viewProjectCommand :: Command Task
viewProjectCommand = mkViewCommand orgProjectKeyword " ap" "viewProject" (Just KB.sortByPriorityAsc)

-- | View TODO tasks
viewTodoCommand :: Command Task
viewTodoCommand = mkViewCommand orgTodoKeyword " at" "viewTodo" (Just KB.sortByPriorityAsc)

-- | View DONE tasks
viewDoneCommand :: Command Task
viewDoneCommand = mkViewCommand orgDoneKeyword " ad" "viewDone" Nothing

-- | View TRASH tasks
viewTrashCommand :: Command Task
viewTrashCommand = mkViewCommand orgTrashKeyword " ax" "viewTrash" Nothing
