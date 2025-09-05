module Refile.Refile where

import Brick (get, modify)
import Control.Lens

import Refile.Refileable
import Tui.Types

-- | Refile the current task to the selected project
refileTaskToProject :: (Refileable a) => TaskPointer -> TaskPointer -> GlobalAppState a
refileTaskToProject taskPtr projectPtr = do
  ctx <- get
  let fs = view fileStateLens ctx
      projectsFilePath = view (config . projectsFile) ctx
  
  -- Get the task to be refiled
  case preview (taskBy taskPtr) fs of
    Just task -> do
      -- Insert the task under the selected project (pure operation)
      let refileResult = insertTaskUnder projectPtr task taskPtr fs projectsFilePath
          newFs = _newFileState refileResult
          wasTaskMoved = _wasTaskMoved refileResult
      
      -- Update the file state with the result of insertion
      modify $ set fileStateLens newFs
      
      -- Mark original task for removal only if it wasn't moved within the same file
      if not wasTaskMoved
        then do
          updatedCtx <- get
          let currentFs = view fileStateLens updatedCtx
              finalFs = markTaskForRemoval taskPtr currentFs projectsFilePath
          modify $ set fileStateLens finalFs
        else return ()
    
    Nothing -> return () -- Task not found