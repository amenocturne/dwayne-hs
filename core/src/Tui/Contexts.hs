{-# LANGUAGE OverloadedStrings #-}

module Tui.Contexts where

import Control.Lens (view)
import Data.Maybe (isJust)
import Tui.Types (AppContext, AppMode (..), appMode, appState, errorDialog, validationDialog)

errorDialogKeyContext :: AppContext a -> Bool
errorDialogKeyContext = isJust . view (appState . errorDialog)

validationDialogKeyContext :: AppContext a -> Bool
validationDialogKeyContext = isJust . view (appState . validationDialog)

modeKeyContext :: AppMode a -> AppContext a -> Bool
modeKeyContext mode ctx = view (appState . appMode) ctx == mode && not (errorDialogKeyContext ctx) && not (validationDialogKeyContext ctx)

selectionModeKeyContext :: AppContext a -> Bool
selectionModeKeyContext ctx = view (appState . appMode) ctx == SelectionMode && not (errorDialogKeyContext ctx) && not (validationDialogKeyContext ctx)

anyModeKeyContext :: [AppMode a] -> AppContext a -> Bool
anyModeKeyContext modes ctx =
  let currentMode = view (appState . appMode) ctx
   in currentMode `elem` modes && not (errorDialogKeyContext ctx) && not (validationDialogKeyContext ctx)

normalOrSelectionContext :: AppContext a -> Bool
normalOrSelectionContext = anyModeKeyContext [NormalMode, SelectionMode]
