{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Tui.Keybindings
  ( orgKeyBindings,
    sortByPriorityAsc,
    sortByPriorityDesc,
    sortByCreatedAsc,
    sortByCreatedDesc,
    todoKeywordFilter,
    upPriority,
    downPriority,
    proceedInErrorDialog,
    toKey,
    withMod,
    toKeySeq,
  )
where

import Brick
import qualified Commands.Command as Cmd
import Control.Lens
import Data.Aeson (Object, Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (LocalTime (LocalTime), midnight)
import Data.Time.Calendar (fromGregorian)
import Graphics.Vty.Input.Events
import Model.OrgMode
  ( OrgTime (time),
    Task (..),
    priority,
    tags,
    todoKeyword,
  )
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import qualified Tui.SelectionMode as SM
import Tui.Types
import Writer.Writer

-- | Convert a Command to a KeyBinding
-- Note: This should only be called for commands that have TUI bindings
commandToKeyBinding :: Cmd.Command Task -> Maybe (KeyBinding Task)
commandToKeyBinding cmd =
  case Cmd.cmdTui cmd of
    Nothing -> Nothing
    Just tuiBinding ->
      Just $
        KeyBinding
          (Cmd.tuiKeyEvent tuiBinding)
          (Cmd.tuiKeybinding tuiBinding)
          (Cmd.tuiDescription tuiBinding)
          (Cmd.tuiAction tuiBinding)
          (Cmd.tuiContext tuiBinding)

-- | Create org-mode keybindings with optional command filtering
-- Takes a list of commands and optional config object to filter them
-- If enabledCommands is Nothing, all commands are enabled
-- If enabledCommands is Just object, only commands not explicitly disabled are included
orgKeyBindings :: [Cmd.Command Task] -> Maybe Object -> [KeyBinding Task]
orgKeyBindings allCommands enabledCommands =
  mapMaybe commandToKeyBinding (filterCommands allCommands enabledCommands)
  where
    -- Filter commands based on config
    filterCommands :: [Cmd.Command Task] -> Maybe Object -> [Cmd.Command Task]
    filterCommands cmds Nothing = cmds
    filterCommands cmds (Just commandsObj) = filter (isCommandEnabled commandsObj) cmds

    isCommandEnabled :: Object -> Cmd.Command Task -> Bool
    isCommandEnabled obj cmd =
      case KM.lookup (K.fromText $ Cmd.cmdAlias cmd) obj of
        Nothing -> True -- Not specified = enabled by default
        Just (Bool False) -> False -- Explicitly disabled
        Just (Bool True) -> True -- Explicitly enabled
        _ -> True -- Invalid value = enabled by default

veryOldTime :: LocalTime
veryOldTime = LocalTime (fromGregorian 1970 1 1) midnight

sortByPriorityAsc :: Task -> Task -> Ordering
sortByPriorityAsc = comparing getPriority
  where
    getPriority t = fromMaybe maxBound (_priority t)

sortByPriorityDesc :: Task -> Task -> Ordering
sortByPriorityDesc t1 t2 = comparing getPriority t2 t1
  where
    getPriority t = fromMaybe maxBound (_priority t)

sortByCreatedAsc :: Task -> Task -> Ordering
sortByCreatedAsc = comparing getCreated
  where
    getCreated t = case fmap time (_createdProp t) of
      Nothing -> veryOldTime
      Just (Left day) -> LocalTime day midnight
      Just (Right lt) -> lt

sortByCreatedDesc :: Task -> Task -> Ordering
sortByCreatedDesc t1 t2 =
  comparing getCreated t2 t1
  where
    getCreated t = case fmap time (_createdProp t) of
      Nothing -> veryOldTime
      Just (Left day) -> LocalTime day midnight
      Just (Right lt) -> lt

todoKeywordFilter :: T.Text -> Task -> Bool
todoKeywordFilter keyword task = _todoKeyword task == keyword

upPriority :: AppContext Task -> AppContext Task
upPriority = over (currentTaskLens . priority) f
  where
    f Nothing = Just 1
    f (Just 0) = Nothing
    f (Just x) = Just (x - 1 `mod` 3)

downPriority :: AppContext Task -> AppContext Task
downPriority = over (currentTaskLens . priority) f
  where
    f Nothing = Just 1
    f (Just 2) = Nothing
    f (Just x) = Just (x + 1 `mod` 3)

proceedInErrorDialog :: (Writer a) => GlobalAppState a
proceedInErrorDialog = modify (over (appState . errorDialog) (const Nothing))

class WithMod a where
  withMod :: a -> Modifier -> NonEmpty KeyPress

instance WithMod Char where
  withMod c m = pure $ KeyPress (KChar $ toLower c) withShift
    where
      withShift = if isUpper c then S.fromList [MShift, m] else S.singleton m

instance WithMod Key where
  withMod c m = pure $ KeyPress c (S.singleton m)

class ToBind a where
  toKey :: a -> NonEmpty KeyPress

instance ToBind Char where
  toKey c = pure $ KeyPress (KChar $ toLower c) withShift
    where
      withShift = if isUpper c then S.singleton MShift else S.empty

instance ToBind Key where
  toKey c = pure $ KeyPress c S.empty

instance ToBind String where
  toKey s = fromList s >>= toKey

instance ToBind T.Text where
  toKey s = toKey $ T.unpack s

instance ToBind (NonEmpty KeyPress) where
  toKey = id

toKeySeq :: String -> NonEmpty KeyPress
toKeySeq s = fromList s >>= toKey
