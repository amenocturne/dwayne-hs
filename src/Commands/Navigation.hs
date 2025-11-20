{-# LANGUAGE OverloadedStrings #-}

module Commands.Navigation
  ( jumpToEndCommand,
    jumpToBeginningCommand,
    jumpBackwardCommand,
    jumpForwardCommand,
    cleanKeyStateCommand,
    moveDownCommand,
    moveUpCommand,
  )
where

import Brick (modify)
import Commands.Command (Command (..), TuiBinding (..))
import qualified Commands.Projects as CmdProjects
import Control.Lens (set)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import Tui.Keybindings (toKey)
import qualified Tui.SelectionMode as SM
import Tui.Types (AppMode (..), KeyEvent (..), KeyPress (..), KeyState (NoInput), appState, keyState)

-- | Jump to the end of the task list
jumpToEndCommand :: Command Task
jumpToEndCommand =
  Command
    { cmdName = "Jump to End",
      cmdAlias = "jumpToEnd",
      cmdDescription = "Jump to the end of the task list",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = JumpEnd,
              tuiKeybinding = toKey 'G',
              tuiDescription = "Jump to the end",
              tuiAction = CmdProjects.saveForJump $ SM.selectionAwareMove (const maxBound),
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Jump to the beginning of the task list
jumpToBeginningCommand :: Command Task
jumpToBeginningCommand =
  Command
    { cmdName = "Jump to Beginning",
      cmdAlias = "jumpToBeginning",
      cmdDescription = "Jump to the beginning of the task list",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = JumpBeginning,
              tuiKeybinding = KeyPress (E.KChar 'g') Set.empty :| [KeyPress (E.KChar 'g') Set.empty],
              tuiDescription = "Jump to the beginning",
              tuiAction = CmdProjects.saveForJump $ SM.selectionAwareMove (const 0),
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Jump backward in history
jumpBackwardCommand :: Command Task
jumpBackwardCommand =
  Command
    { cmdName = "Jump Backward",
      cmdAlias = "jumpBackward",
      cmdDescription = "Jump backward in history",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = JumpBackward,
              tuiKeybinding = KeyPress (E.KChar 'o') (Set.singleton E.MCtrl) :| [],
              tuiDescription = "Jump backward",
              tuiAction = modify Helpers.jumpBack,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Jump forward in history
jumpForwardCommand :: Command Task
jumpForwardCommand =
  Command
    { cmdName = "Jump Forward",
      cmdAlias = "jumpForward",
      cmdDescription = "Jump forward in history",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = JumpForward,
              tuiKeybinding = KeyPress (E.KChar '\t') Set.empty :| [],
              tuiDescription = "Jump forward",
              tuiAction = modify Helpers.jumpForward,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Move up command
moveUpCommand :: Command Task
moveUpCommand =
  Command
    { cmdName = "Move Up",
      cmdAlias = "moveUp",
      cmdDescription = "Move the cursor up",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = MoveUp,
              tuiKeybinding = toKey 'k',
              tuiDescription = "Move up",
              tuiAction = SM.selectionAwareMove (\i -> i - 1),
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Move down command
moveDownCommand :: Command Task
moveDownCommand =
  Command
    { cmdName = "Move Down",
      cmdAlias = "moveDown",
      cmdDescription = "Move the cursor down",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = MoveDown,
              tuiKeybinding = toKey 'j',
              tuiDescription = "Move down",
              tuiAction = SM.selectionAwareMove (+ 1),
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Clean key state command
cleanKeyStateCommand :: Command Task
cleanKeyStateCommand =
  Command
    { cmdName = "Clean Key State",
      cmdAlias = "cleanKeyState",
      cmdDescription = "Clean the key state",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = CleanKeyState,
              tuiKeybinding = toKey E.KEsc,
              tuiDescription = "Clean key state",
              tuiAction = modify $ set (appState . keyState) NoInput,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
