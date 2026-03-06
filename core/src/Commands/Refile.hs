{-# LANGUAGE OverloadedStrings #-}

module Commands.Refile (refileCommand, openRefileDialog) where

import Brick (get, modify)
import Commands.Command (Command (..))
import Commands.ErrorDialog (showError)
import Control.Lens
import qualified Core.Operations as Ops
import Model.OrgMode (Task)
import Tui.Types
  ( GlobalAppState,
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
      cmdTui = Nothing,
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
