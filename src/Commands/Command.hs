{-# LANGUAGE RankNTypes #-}

module Commands.Command where

import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import Tui.Types (GlobalAppState, AppContext, KeyPress, KeyEvent)

-- | A Command represents a user action that can be invoked from multiple frontends
-- (TUI, CLI, or API). Each command has:
-- - A unique identifier (KeyEvent)
-- - TUI keybinding information (key sequence and description)
-- - The action to execute
-- - Context for when the keybinding is valid
data Command a = Command
  { cmdName :: T.Text
  -- ^ Human-readable name of the command (e.g., "Go to Projects")

  , cmdDescription :: T.Text
  -- ^ Detailed description of what the command does

  , cmdKeyEvent :: KeyEvent
  -- ^ Unique identifier for this command

  , cmdTuiKeybinding :: NonEmpty KeyPress
  -- ^ Key sequence to trigger this command in TUI

  , cmdTuiDescription :: T.Text
  -- ^ Short description shown in TUI help

  , cmdAction :: GlobalAppState a
  -- ^ The action to execute when the command is invoked

  , cmdContext :: AppContext a -> Bool
  -- ^ Predicate to determine if this command is valid in the current context
  }
