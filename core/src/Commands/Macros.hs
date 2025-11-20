{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Macros where

import Commands.Command (Command (..), TuiBinding (..))
import qualified Data.Set as S
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import Tui.Keybindings (toKeySeq)
import qualified Tui.SelectionMode as SM
import Tui.Types
  ( KeyEvent (..),
  )

-- | Music macro command
musicMacroCommand :: Command Task
musicMacroCommand =
  Command
    { cmdName = "Music Macro",
      cmdAlias = "musicMacro",
      cmdDescription = "Apply music tags and set todo keyword to LIST",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = Macro "Music",
              tuiKeybinding = toKeySeq "mm",
              tuiDescription = "Macros for music entries",
              tuiAction =
                Helpers.saveForUndo $ do
                  SM.smartApplyTagAction (S.insert "music")
                  SM.smartApplyTagAction (S.insert "download")
                  SM.smartApplyTodoKeyword "LIST",
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
