{-# LANGUAGE FlexibleContexts #-}

module Refile.Refileable where

import Control.Lens
import Tui.Types

-- | Type class for tasks that support refile operations
class Refileable a where
  -- | Insert a task under another task in a file, adjusting its level appropriately
  insertTaskUnder :: TaskPointer -> a -> TaskPointer -> GlobalAppState a
  
  -- | Mark a task for removal (e.g., set to TRASH keyword)
  markTaskForRemoval :: TaskPointer -> GlobalAppState a