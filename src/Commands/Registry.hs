module Commands.Registry where

import Commands.Command (Command)
import Commands.Projects (goToProjectsCommand)
import Model.OrgMode (Task)

-- | All available commands in the application
-- This list will be populated with commands as we extract them from Tui.Keybindings
allCommands :: [Command Task]
allCommands = [goToProjectsCommand]
