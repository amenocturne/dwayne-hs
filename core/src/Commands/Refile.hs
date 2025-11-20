{-# LANGUAGE OverloadedStrings #-}

module Commands.Refile (refileCommand, openRefileDialog) where

import Brick (get, modify)
import Commands.Command (Command (..), TuiBinding (..))
import Commands.ErrorDialog (showError)
import Control.Lens
import qualified Core.Operations as Ops
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import Tui.Keybindings
import qualified Tui.Keybindings as KB
import Tui.Types
  ( AppContext,
    AppMode (..),
    GlobalAppState,
    KeyEvent (..),
    KeyPress (..),
    RefileDialog (..),
    appState,
    fileStateLens,
    refileDialog,
  )

-- | Refile task to project command
refileCommand :: Command Task
refileCommand =
  Command
    { cmdName = "Refile Task",
      cmdAlias = "refile",
      cmdDescription = "Refile the current task to a project by showing a project selection dialog",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = Refile,
              tuiKeybinding = toKeySeq "rf", -- 'r' then 'f'
              tuiDescription = "Refile task to project",
              tuiAction = openRefileDialog,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Action: Open refile dialog to select a project
openRefileDialog :: GlobalAppState Task
openRefileDialog = do
  ctx <- get
  let fs = view fileStateLens ctx
      allProjects = Ops.getAllProjects fs
  case allProjects of
    [] -> showError "No projects found in your task files.\n\nTo refile a task, you need at least one task with TODO state 'PROJECT'."
    _ -> do
      let dialog =
            RefileDialog
              { _rdProjects = allProjects,
                _rdSearchQuery = "",
                _rdSelectedIndex = 0
              }
      modify $ set (appState . refileDialog) (Just dialog)
