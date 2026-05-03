{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.UndoRedo where

import Brick (modify)
import Commands.Command (Command (..), TuiBinding (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import Tui.Keybindings (toKey, withMod)
import Tui.Types
  ( AppMode (..),
    KeyEvent (..),
    KeyPress (..),
  )

-- | Undo command
undoCommand :: Command Task
undoCommand =
  Command
    { cmdName = "Undo",
      cmdAlias = "undo",
      cmdDescription = "Undo the last action",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = Undo,
              tuiKeybinding = toKey 'u',
              tuiDescription = "Undo",
              tuiAction = modify Helpers.undo,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Redo command
redoCommand :: Command Task
redoCommand =
  Command
    { cmdName = "Redo",
      cmdAlias = "redo",
      cmdDescription = "Redo the last undone action",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = Redo,
              tuiKeybinding = withMod 'r' E.MCtrl,
              tuiDescription = "Redo",
              tuiAction = modify Helpers.redo,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
