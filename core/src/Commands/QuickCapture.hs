{-# LANGUAGE OverloadedStrings #-}

module Commands.QuickCapture (quickCaptureCommand) where

import Brick (modify)
import Commands.Command (Command (..), TuiBinding (..))
import Control.Lens (set)
import qualified Data.Text as T
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import Tui.Keybindings (toKeySeq)
import Tui.Types
  ( AppMode (..),
    CmdState (..),
    CmdType (Capture),
    KeyEvent (..),
    appState,
    cmdState,
    switchMode,
  )

quickCaptureCommand :: Command Task
quickCaptureCommand =
  Command
    { cmdName = "Quick Capture",
      cmdAlias = "quickCapture",
      cmdDescription = "Quickly add a task to inbox by typing the title",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = QuickCapture,
              tuiKeybinding = toKeySeq "ac",
              tuiDescription = "Quick capture task",
              tuiAction = modify $ \ctx -> (switchMode CmdMode . set (appState . cmdState) (Just $ Typing Capture T.empty)) ctx,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
