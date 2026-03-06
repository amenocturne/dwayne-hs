{-# LANGUAGE RankNTypes #-}

module Commands.Command where

import Api.Types (ApiBinding)
import Data.Aeson (Object, Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Options.Applicative (Parser)
import Tui.Types (AppContext, GlobalAppState, KeyEvent, KeyPress)

-- | TUI-specific binding information for a command
-- Contains all the TUI frontend-specific fields
data TuiBinding a = TuiBinding
  { -- | Unique identifier for this command in TUI
    tuiKeyEvent :: KeyEvent,
    -- | Key sequence to trigger this command in TUI
    tuiKeybinding :: NonEmpty KeyPress,
    -- | Short description shown in TUI help
    tuiDescription :: T.Text,
    -- | The action to execute when the command is invoked in TUI
    tuiAction :: GlobalAppState a,
    -- | Predicate to determine if this command is valid in the current TUI context
    tuiContext :: AppContext a -> Bool
  }

-- | A Command represents a user action that can be invoked from multiple frontends
-- (TUI, CLI, or API). Each command has core metadata and optional frontend-specific bindings.
data Command a = Command
  { -- | Human-readable name of the command (e.g., "Go to Projects")
    cmdName :: T.Text,
    -- | Config key for this command (e.g., "projects", "archive", "refile")
    -- Used in config.yml: commands: { projects: true, archive: false }
    cmdAlias :: T.Text,
    -- | Detailed description of what the command does
    cmdDescription :: T.Text,
    -- | TUI-specific binding (Nothing if command not available in TUI)
    cmdTui :: Maybe (TuiBinding a),
    -- | Placeholder for CLI binding (Phase 3)
    cmdCli :: Maybe (Parser (IO ())),
    -- | API binding (Nothing if command not available via API)
    -- Commands can be enabled/disabled in config.yml via the 'commands' field
    -- Example: commands: { viewInbox: true, viewTrash: false }
    cmdApi :: Maybe (ApiBinding a)
  }

-- | Filter commands based on optional commands config object.
-- Commands are enabled by default unless explicitly set to false in config.
getEnabledCommands :: Maybe Object -> [Command a] -> [Command a]
getEnabledCommands Nothing cmds = cmds
getEnabledCommands (Just commandsObj) cmds =
  filter (isCommandEnabled commandsObj) cmds
  where
    isCommandEnabled :: Object -> Command a -> Bool
    isCommandEnabled obj cmd =
      case KM.lookup (K.fromText $ cmdAlias cmd) obj of
        Nothing -> True
        Just (Bool False) -> False
        Just (Bool True) -> True
        _ -> True
