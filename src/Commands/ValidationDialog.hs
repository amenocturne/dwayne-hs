{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.ValidationDialog where

import Brick (modify)
import Commands.Command (Command (..), TuiBinding (..))
import Data.List.NonEmpty (NonEmpty (..), fromList)
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import Tui.Types
  ( KeyEvent (..),
    KeyPress (..),
  )
import qualified Tui.ValidationDialogs as TuiValidationDialogs -- For acceptValidation, rejectValidation
import Writer.Writer (Writer)

-- | Accept validation dialog command
validationDialogAcceptCommand :: (Writer Task) => Command Task
validationDialogAcceptCommand =
  Command
    { cmdName = "Accept Validation Dialog",
      cmdAlias = "validationDialogAccept",
      cmdDescription = "Accept the validation dialog",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = ValidationDialogAccept,
              tuiKeybinding = pure $ Tui.Types.KeyPress E.KEnter Set.empty,
              tuiDescription = "Accept validation",
              tuiAction = TuiValidationDialogs.acceptValidation,
              tuiContext = Ctx.validationDialogKeyContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Reject validation dialog command
validationDialogRejectCommand :: (Writer Task) => Command Task
validationDialogRejectCommand =
  Command
    { cmdName = "Reject Validation Dialog",
      cmdAlias = "validationDialogReject",
      cmdDescription = "Reject the validation dialog",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = ValidationDialogReject,
              tuiKeybinding = pure $ Tui.Types.KeyPress E.KEsc Set.empty,
              tuiDescription = "Reject validation",
              tuiAction = TuiValidationDialogs.rejectValidation,
              tuiContext = Ctx.validationDialogKeyContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
