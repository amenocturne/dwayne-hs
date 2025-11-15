{-# LANGUAGE FlexibleContexts #-}

module Refile.Refileable where

import Control.Lens
import Core.Types (TaskPointer, FileState)
import qualified Data.Map as M

-- | Result of a refile operation
data RefileResult a = RefileResult
  { _newFileState :: FileState a
  , _wasTaskMoved :: Bool  -- True if task was moved within same file, False if copied to new file
  } deriving (Show, Eq)

-- | Type class for tasks that support refile operations
class Refileable a where
  -- | Insert a task under another task in a file, adjusting its level appropriately
  -- Returns the updated FileState and whether the task was moved (vs copied)
  insertTaskUnder :: TaskPointer -> a -> TaskPointer -> FileState a -> String -> RefileResult a
  
  -- | Mark a task for removal (e.g., set to TRASH keyword)
  -- Returns the updated FileState  
  markTaskForRemoval :: TaskPointer -> FileState a -> String -> FileState a