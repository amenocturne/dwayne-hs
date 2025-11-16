{-# LANGUAGE OverloadedStrings #-}

-- | Builder functions for common command patterns
-- This module reduces boilerplate by providing reusable command builders
module Commands.Builders
  ( mkTagCommand,
    mkAddTagCommand,
    mkDeleteTagCommand,
    mkViewCommand,
    mkTodoKeywordCommand,
  )
where

import Commands.Command (Command (..), TuiBinding (..))
import qualified Commands.Projects as CmdProjects
import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as S
import qualified Data.Text as T
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import Tui.Keybindings (toKeySeq, todoKeywordFilter)
import qualified Tui.SelectionMode as SM
import Tui.Types
  ( AppMode (NormalMode),
    GlobalAppState,
    KeyEvent (..),
    KeyPress,
  )

-- | Generic builder for tag commands
-- Creates a command that adds or deletes a tag using a custom tag operation
mkTagCommand ::
  -- | Tag name
  T.Text ->
  -- | Whether this is an add (True) or delete (False) operation
  Bool ->
  -- | Key sequence (e.g., "a,m" for add music)
  String ->
  -- | Command alias (e.g., "addTagMusic")
  T.Text ->
  Command Task
mkTagCommand tag isAdd keySeq alias =
  Command
    { cmdName = if isAdd then "Add Tag" else "Delete Tag",
      cmdAlias = alias,
      cmdDescription = T.concat [if isAdd then "Add " else "Delete ", tag, " tag to/from the current task or selection"],
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = if isAdd then AddTag tag else DeleteTag tag,
              tuiKeybinding = toKeySeq keySeq,
              tuiDescription = T.concat [if isAdd then "Add " else "Delete ", tag, " tag"],
              tuiAction = Helpers.saveForUndo $ SM.smartApplyTagAction (if isAdd then S.insert tag else S.delete tag),
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Convenience builder for add tag commands
mkAddTagCommand ::
  -- | Tag name
  T.Text ->
  -- | Key sequence (e.g., "a,m")
  String ->
  -- | Command alias (e.g., "addTagMusic")
  T.Text ->
  Command Task
mkAddTagCommand = flip mkTagCommand True

-- | Convenience builder for delete tag commands
mkDeleteTagCommand ::
  -- | Tag name
  T.Text ->
  -- | Key sequence (e.g., "d,m")
  String ->
  -- | Command alias (e.g., "deleteTagMusic")
  T.Text ->
  Command Task
mkDeleteTagCommand tag = mkTagCommand tag False

-- | Generic builder for view filter commands with keyword filtering
-- Creates a command that filters tasks by TODO keyword with optional sorting
mkViewCommand ::
  -- | TODO keyword to filter by (e.g., "INBOX", "RELEVANT")
  T.Text ->
  -- | Key sequence (e.g., " ai" for view inbox)
  String ->
  -- | Command alias (e.g., "viewInbox")
  T.Text ->
  -- | Optional sorting function
  Maybe (Task -> Task -> Ordering) ->
  Command Task
mkViewCommand keyword keySeq alias maybeSorter =
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
                CmdProjects.saveForJump $ Helpers.applyFilterToAllTasks (todoKeywordFilter keyword)
                forM_ maybeSorter Helpers.applySorter,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Generic builder for TODO keyword change commands
-- Creates a command that changes the TODO keyword of a task or selection
mkTodoKeywordCommand ::
  -- | Target TODO keyword (e.g., "INBOX", "RELEVANT")
  T.Text ->
  -- | Key sequence (e.g., "ti" for inbox)
  String ->
  -- | Command alias (e.g., "keywordInbox")
  T.Text ->
  Command Task
mkTodoKeywordCommand keyword keySeq alias =
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
