{-# LANGUAGE OverloadedStrings #-}

module Commands.RefreshView (refreshViewCommand) where

import Brick (get, modify)
import Commands.Command (Command (..), TuiBinding (..))
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import Tui.Helpers (refreshTuiView)
import Tui.Keybindings
import Tui.Types
  ( AppMode (..),
    GlobalAppState,
    KeyEvent (..),
    KeyPress (..),
    cachedView,
    compactViewLens,
    recomputeCurrentView,
  )

-- | Refresh current view command
refreshViewCommand :: Command Task
refreshViewCommand =
  Command
    { cmdName = "Refresh View",
      cmdAlias = "refreshView",
      cmdDescription = "Refresh the current view by recomputing the cached view",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = RefreshView,
              tuiKeybinding = toKeySeq "r", -- 'r'
              tuiDescription = "Refresh current view",
              tuiAction = refreshTuiView,
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
