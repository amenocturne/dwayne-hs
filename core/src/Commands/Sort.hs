{-# LANGUAGE OverloadedStrings #-}

module Commands.Sort
  ( sortCreatedAscCommand,
    sortCreatedDescCommand,
    sortPriorityAscCommand,
    sortPriorityDescCommand,
  )
where

import Commands.Command (Command (..), TuiBinding (..))
import qualified Commands.Projects as CmdProjects
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import Tui.Keybindings
import qualified Tui.Keybindings as KB
import Tui.Types (AppMode (NormalMode), KeyEvent (..), KeyPress (..))

-- | Sort by created date ascending command
sortCreatedAscCommand :: Command Task
sortCreatedAscCommand =
  Command
    { cmdName = "Sort Created Ascending",
      cmdAlias = "sortCreatedAsc",
      cmdDescription = "Sort tasks by created date (ascending)",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = SortCreatedAsc,
              tuiKeybinding = toKeySeq "sca",
              tuiDescription = "Sort by created (asc)",
              tuiAction = CmdProjects.saveForJump (Helpers.applySorter KB.sortByCreatedAsc),
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Sort by created date descending command
sortCreatedDescCommand :: Command Task
sortCreatedDescCommand =
  Command
    { cmdName = "Sort Created Descending",
      cmdAlias = "sortCreatedDesc",
      cmdDescription = "Sort tasks by created date (descending)",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = SortCreatedDesc,
              tuiKeybinding = toKeySeq "scd",
              tuiDescription = "Sort by created (desc)",
              tuiAction = CmdProjects.saveForJump (Helpers.applySorter KB.sortByCreatedDesc),
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Sort by priority ascending command (A first)
sortPriorityAscCommand :: Command Task
sortPriorityAscCommand =
  Command
    { cmdName = "Sort Priority Ascending",
      cmdAlias = "sortPriorityAsc",
      cmdDescription = "Sort tasks by priority (ascending - A first)",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = SortPriorityAsc,
              tuiKeybinding = toKeySeq "spa",
              tuiDescription = "Sort by priority (asc)",
              tuiAction = CmdProjects.saveForJump (Helpers.applySorter KB.sortByPriorityAsc),
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Sort by priority descending command (C first)
sortPriorityDescCommand :: Command Task
sortPriorityDescCommand =
  Command
    { cmdName = "Sort Priority Descending",
      cmdAlias = "sortPriorityDesc",
      cmdDescription = "Sort tasks by priority (descending - C first)",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = SortPriorityDesc,
              tuiKeybinding = toKeySeq "spd",
              tuiDescription = "Sort by priority (desc)",
              tuiAction = CmdProjects.saveForJump (Helpers.applySorter KB.sortByPriorityDesc),
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
