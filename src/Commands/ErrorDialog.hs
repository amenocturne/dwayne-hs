{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.ErrorDialog where

import Brick (modify)
import Commands.Command (Command (..), TuiBinding (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
-- For proceedInErrorDialog

import Graphics.Vty (Key (KEsc))
import Graphics.Vty.Input (Key (KEnter))
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import Tui.Keybindings
import qualified Tui.Keybindings as KB
import Tui.Types
  ( KeyEvent (..),
    KeyPress (..),
  )
import Writer.Writer (Writer)

-- | Quit error dialog command
errorDialogQuitCommand :: (Writer Task) => Command Task
errorDialogQuitCommand =
  Command
    { cmdName = "Quit Error Dialog",
      cmdAlias = "errorDialogQuit",
      cmdDescription = "Quit the error dialog",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = ErrorDialogQuit,
              tuiKeybinding = toKey KEsc,
              tuiDescription = "Quit error dialog",
              tuiAction = KB.proceedInErrorDialog,
              tuiContext = Ctx.errorDialogKeyContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Accept error dialog command
errorDialogAcceptCommand :: (Writer Task) => Command Task
errorDialogAcceptCommand =
  Command
    { cmdName = "Accept Error Dialog",
      cmdAlias = "errorDialogAccept",
      cmdDescription = "Accept the error dialog",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = ErrorDialogAccept,
              tuiKeybinding = toKey KEnter,
              tuiDescription = "Accept error dialog",
              tuiAction = KB.proceedInErrorDialog,
              tuiContext = Ctx.errorDialogKeyContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
