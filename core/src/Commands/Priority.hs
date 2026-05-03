{-# LANGUAGE OverloadedStrings #-}

module Commands.Priority
  ( priorityUpCommand,
    priorityDownCommand,
  )
where

import Brick (modify)
import Commands.Command (Command (..), TuiBinding (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import qualified Tui.Keybindings as KB
import Tui.Types
  ( GlobalAppState,
    KeyEvent (..),
    KeyPress (..),
  )

-- | Increase priority command (A -> nothing, B -> A, C -> B, nothing -> B)
priorityUpCommand :: Command Task
priorityUpCommand =
  Command
    { cmdName = "Priority Up",
      cmdAlias = "priorityUp",
      cmdDescription = "Increase the priority of the current task (cycles through C->B->A->none)",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = UpPriority,
              tuiKeybinding = KeyPress E.KUp (Set.singleton E.MShift) :| [],
              tuiDescription = "Increase priority",
              tuiAction = Helpers.saveForUndo $ modify KB.upPriority,
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Decrease priority command (nothing -> B, A -> B, B -> C, C -> nothing)
priorityDownCommand :: Command Task
priorityDownCommand =
  Command
    { cmdName = "Priority Down",
      cmdAlias = "priorityDown",
      cmdDescription = "Decrease the priority of the current task (cycles through none->B->C->none)",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = DownPriority,
              tuiKeybinding = KeyPress E.KDown (Set.singleton E.MShift) :| [],
              tuiDescription = "Decrease priority",
              tuiAction = Helpers.saveForUndo $ modify KB.downPriority,
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
