module Refile.Refile where

import Brick (get)
import Control.Lens

import Refile.Refileable
import Tui.Types

-- | Refile the current task to the selected project
refileTaskToProject :: (Refileable a) => TaskPointer -> TaskPointer -> GlobalAppState a
refileTaskToProject taskPtr projectPtr = do
  ctx <- get
  let fs = view fileStateLens ctx
  
  -- Get the task to be refiled
  case preview (taskBy taskPtr) fs of
    Just task -> do
      -- Insert the task under the selected project
      insertTaskUnder projectPtr task taskPtr
      
      -- Mark original task for removal
      markTaskForRemoval taskPtr
    
    Nothing -> return () -- Task not found