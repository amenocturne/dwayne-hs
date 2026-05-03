{-# LANGUAGE OverloadedStrings #-}

module Commands.SelectionMode
  ( enterSelectionModeCommand,
    toggleCurrentSelectionCommand,
    exitSelectionModeCommand,
    toggleRangeSelectionCommand,
    toggleCurrentSelectionInSelectionCommand,
  )
where

import Commands.Command (Command (..), TuiBinding (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import qualified Tui.SelectionMode as SM
import Tui.Types (AppMode (..), KeyEvent (..), KeyPress (..))

-- | Enter selection mode command (Shift+v in Normal mode)
enterSelectionModeCommand :: Command Task
enterSelectionModeCommand =
  Command
    { cmdName = "Enter Selection Mode",
      cmdAlias = "enterSelectionMode",
      cmdDescription = "Enter selection mode and select the current task",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = EnterSelectionMode,
              tuiKeybinding = KeyPress (E.KChar 'v') (Set.singleton E.MShift) :| [],
              tuiDescription = "Enter selection mode",
              tuiAction = SM.enterSelectionMode,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Toggle current selection command (v in Normal mode)
toggleCurrentSelectionCommand :: Command Task
toggleCurrentSelectionCommand =
  Command
    { cmdName = "Toggle Current Selection",
      cmdAlias = "toggleCurrentSelection",
      cmdDescription = "Toggle selection of the current task without entering selection mode",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = ToggleCurrentSelection,
              tuiKeybinding = KeyPress (E.KChar 'v') Set.empty :| [],
              tuiDescription = "Toggle current item selection",
              tuiAction = SM.toggleCurrentSelection,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Exit selection mode command (Esc in Selection mode)
exitSelectionModeCommand :: Command Task
exitSelectionModeCommand =
  Command
    { cmdName = "Exit Selection Mode",
      cmdAlias = "exitSelectionMode",
      cmdDescription = "Exit selection mode and clear all selections",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = ExitSelectionMode,
              tuiKeybinding = KeyPress E.KEsc Set.empty :| [],
              tuiDescription = "Exit selection mode",
              tuiAction = SM.exitSelectionMode,
              tuiContext = Ctx.modeKeyContext SelectionMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Toggle range selection command (Shift+v in Selection mode)
toggleRangeSelectionCommand :: Command Task
toggleRangeSelectionCommand =
  Command
    { cmdName = "Toggle Range Selection",
      cmdAlias = "toggleRangeSelection",
      cmdDescription = "Extend selection from anchor to current cursor position",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = ToggleRangeSelection,
              tuiKeybinding = KeyPress (E.KChar 'v') (Set.singleton E.MShift) :| [],
              tuiDescription = "Toggle range selection",
              tuiAction = SM.toggleRangeSelection,
              tuiContext = Ctx.modeKeyContext SelectionMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Toggle current selection in selection mode command (v in Selection mode)
toggleCurrentSelectionInSelectionCommand :: Command Task
toggleCurrentSelectionInSelectionCommand =
  Command
    { cmdName = "Toggle Current Selection In Selection Mode",
      cmdAlias = "toggleCurrentSelectionInSelection",
      cmdDescription = "Toggle selection of the current task while in selection mode",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = ToggleCurrentSelection,
              tuiKeybinding = KeyPress (E.KChar 'v') Set.empty :| [],
              tuiDescription = "Toggle current selection",
              tuiAction = SM.toggleCurrentSelection,
              tuiContext = Ctx.modeKeyContext SelectionMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
