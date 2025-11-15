{-# LANGUAGE OverloadedStrings #-}

module Commands.Views
  ( viewCommand,
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
  )
where

import Commands.Command (Command (..), TuiBinding (..))
import qualified Commands.Projects as CmdProjects
import Control.Lens ((^.))
import Control.Monad (forM_)
import qualified Core.Operations as Ops
import Core.Types (TaskPointer (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task, TaskFile (..), orgDoneKeyword, orgInboxKeyword, orgListKeyword, orgNotesKeyword, orgProjectKeyword, orgRelevantKeyword, orgSomedayKeyword, orgTodoKeyword, orgTrashKeyword, orgWaitingKeyword, tags)
import Parser.OrgParser (orgFileParser)
import Parser.Parser (ParserResult (..))
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import qualified Tui.Keybindings as KB
import Tui.Types (AppConfig (..), AppMode (NormalMode), GlobalAppState, KeyEvent (..), KeyPress (..), SystemConfig (..))

-- | Generic function to create a view filter command
viewCommand :: T.Text -> String -> T.Text -> Maybe (Task -> Task -> Ordering) -> Command Task
viewCommand keyword keySeq alias maybeSorter =
  Command
    { cmdName = "View Filter",
      cmdAlias = alias,
      cmdDescription = T.concat ["Show ", keyword, " tasks"],
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = View keyword,
              tuiKeybinding = toKeySeq keySeq,
              tuiDescription = T.concat ["Show ", keyword, " tasks"],
              tuiAction = do
                CmdProjects.saveForJump $ Helpers.applyFilterToAllTasks (KB.todoKeywordFilter keyword)
                forM_ maybeSorter Helpers.applySorter,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
  where
    toKeySeq (c : cs) = KeyPress (E.KChar c) Set.empty :| map (\x -> KeyPress (E.KChar x) Set.empty) cs
    toKeySeq [] = error "toKeySeq: empty string"

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
viewInboxCommand = viewCommand orgInboxKeyword " ai" "viewInbox" (Just KB.sortByCreatedDesc)

-- | View RELEVANT tasks
viewRelevantCommand :: Command Task
viewRelevantCommand = viewCommand orgRelevantKeyword " ar" "viewRelevant" (Just KB.sortByPriorityAsc)

-- | View SOMEDAY tasks
viewSomedayCommand :: Command Task
viewSomedayCommand = viewCommand orgSomedayKeyword " as" "viewSomeday" Nothing

-- | View NOTES tasks
viewNotesCommand :: Command Task
viewNotesCommand = viewCommand orgNotesKeyword " an" "viewNotes" Nothing

-- | View LIST tasks
viewListCommand :: Command Task
viewListCommand = viewCommand orgListKeyword " al" "viewList" Nothing

-- | View WAITING tasks
viewWaitingCommand :: Command Task
viewWaitingCommand = viewCommand orgWaitingKeyword " aw" "viewWaiting" (Just KB.sortByPriorityAsc)

-- | View PROJECT tasks
viewProjectCommand :: Command Task
viewProjectCommand = viewCommand orgProjectKeyword " ap" "viewProject" (Just KB.sortByPriorityAsc)

-- | View TODO tasks
viewTodoCommand :: Command Task
viewTodoCommand = viewCommand orgTodoKeyword " at" "viewTodo" (Just KB.sortByPriorityAsc)

-- | View DONE tasks
viewDoneCommand :: Command Task
viewDoneCommand = viewCommand orgDoneKeyword " ad" "viewDone" Nothing

-- | View TRASH tasks
viewTrashCommand :: Command Task
viewTrashCommand = viewCommand orgTrashKeyword " ax" "viewTrash" Nothing
