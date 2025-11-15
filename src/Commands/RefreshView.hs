{-# LANGUAGE OverloadedStrings #-}

module Commands.RefreshView (refreshViewCommand, refreshCurrentView) where

import Brick (get, modify)
import Commands.Command (Command (..), TuiBinding (..))
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
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
              tuiAction = refreshCurrentView,
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Action: Refresh the current view by recomputing the cached view
refreshCurrentView :: GlobalAppState a
refreshCurrentView = do
  ctx <- get
  let newCache = recomputeCurrentView ctx
  modify $ set (compactViewLens . cachedView) newCache
