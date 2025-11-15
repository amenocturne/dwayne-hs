{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.CmdMode where

import Brick (modify)
import qualified Commands.Command as Cmd
import Control.Lens (set)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import Searcher.OrgSearcher ()
import Searcher.Searcher (Searcher)
import qualified Tui.CmdMode as TuiCmdMode
import qualified Tui.Contexts as Ctx
import Tui.Keybindings
import Tui.Types
  ( AppMode (..),
    CmdState (..),
    CmdType (..),
    KeyEvent (..),
    KeyPress (..),
    appState,
    cmdState,
    switchMode,
  )
import Tui.Types hiding (Command)
import Writer.OrgWriter ()
import Writer.Writer (Writer)

-- | Switch to search mode command
switchToSearchModeCommand :: Cmd.Command Task
switchToSearchModeCommand =
  Cmd.Command
    { Cmd.cmdName = "Switch to Search Mode",
      Cmd.cmdAlias = "switchToSearchMode",
      Cmd.cmdDescription = "Switch to search mode to filter tasks",
      Cmd.cmdTui =
        Just $
          Cmd.TuiBinding
            { Cmd.tuiKeyEvent = SwitchToSearchMode,
              Cmd.tuiKeybinding = toKeySeq "/",
              Cmd.tuiDescription = "Switch to search mode",
              Cmd.tuiAction = modify $ \ctx -> (switchMode CmdMode . set (appState . cmdState) (Just $ Typing Search T.empty)) ctx,
              Cmd.tuiContext = Ctx.modeKeyContext NormalMode
            },
      Cmd.cmdCli = Nothing,
      Cmd.cmdApi = Nothing
    }

-- | Switch to command mode command
switchToCmdModeCommand :: Cmd.Command Task
switchToCmdModeCommand =
  Cmd.Command
    { Cmd.cmdName = "Switch to Command Mode",
      Cmd.cmdAlias = "switchToCmdMode",
      Cmd.cmdDescription = "Switch to command mode to execute commands",
      Cmd.cmdTui =
        Just $
          Cmd.TuiBinding
            { Cmd.tuiKeyEvent = SwitchToCmdMode,
              Cmd.tuiKeybinding = toKeySeq ":",
              Cmd.tuiDescription = "Switch to command mode",
              Cmd.tuiAction = modify $ \ctx -> (switchMode CmdMode . set (appState . cmdState) (Just $ Typing Tui.Types.Command T.empty)) ctx,
              Cmd.tuiContext = Ctx.modeKeyContext NormalMode
            },
      Cmd.cmdCli = Nothing,
      Cmd.cmdApi = Nothing
    }

-- | Abort command mode command
abortCmdCommand :: Cmd.Command Task
abortCmdCommand =
  Cmd.Command
    { Cmd.cmdName = "Abort Command",
      Cmd.cmdAlias = "abortCmd",
      Cmd.cmdDescription = "Abort current command or search mode",
      Cmd.cmdTui =
        Just $
          Cmd.TuiBinding
            { Cmd.tuiKeyEvent = AbortCmd,
              Cmd.tuiKeybinding = pure $ KeyPress E.KEsc Set.empty,
              Cmd.tuiDescription = "Abort command",
              Cmd.tuiAction = modify TuiCmdMode.abortCmd,
              Cmd.tuiContext = Ctx.modeKeyContext CmdMode
            },
      Cmd.cmdCli = Nothing,
      Cmd.cmdApi = Nothing
    }

-- | Delete character in command mode command
cmdDeleteCharCommand :: Cmd.Command Task
cmdDeleteCharCommand =
  Cmd.Command
    { Cmd.cmdName = "Delete Character",
      Cmd.cmdAlias = "cmdDeleteChar",
      Cmd.cmdDescription = "Delete character in command or search mode",
      Cmd.cmdTui =
        Just $
          Cmd.TuiBinding
            { Cmd.tuiKeyEvent = CmdDeleteChar,
              Cmd.tuiKeybinding = pure $ KeyPress E.KBS Set.empty,
              Cmd.tuiDescription = "Delete char",
              Cmd.tuiAction = modify TuiCmdMode.cmdDeleteChar,
              Cmd.tuiContext = Ctx.modeKeyContext CmdMode
            },
      Cmd.cmdCli = Nothing,
      Cmd.cmdApi = Nothing
    }

-- | Apply command in command mode command
applyCmdCommand :: Cmd.Command Task
applyCmdCommand =
  Cmd.Command
    { Cmd.cmdName = "Apply Command",
      Cmd.cmdAlias = "applyCmd",
      Cmd.cmdDescription = "Execute command or search query",
      Cmd.cmdTui =
        Just $
          Cmd.TuiBinding
            { Cmd.tuiKeyEvent = ApplyCmd,
              Cmd.tuiKeybinding = pure $ KeyPress E.KEnter Set.empty,
              Cmd.tuiDescription = "Execute command",
              Cmd.tuiAction = TuiCmdMode.executeCommand,
              Cmd.tuiContext = Ctx.modeKeyContext CmdMode
            },
      Cmd.cmdCli = Nothing,
      Cmd.cmdApi = Nothing
    }
