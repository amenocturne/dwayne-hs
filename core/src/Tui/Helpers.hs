{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tui.Helpers where

import Brick
import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Model.LinearHistory as L
import Model.OrgMode (Task, todoKeyword)
import Tui.MutationEvents (emitMutationEventsForChange)
import Tui.Types
import qualified Tui.Types as TT
import Writer.Writer

adjustViewport :: GlobalAppState a
adjustViewport = do
  mExt <- lookupExtent CompactViewWidget
  case mExt of
    Nothing -> return ()
    Just (Extent _ _ (w, h)) -> do
      ctx <- get
      let margin = view (config . scrollingMargin) ctx
          viewSize = V.length $ view currentViewLens ctx
          viewStartOld = view (compactViewLens . viewportStart) ctx

      case view cursorLens ctx of
        Nothing -> return ()
        Just cursor -> do
          let maxViewStart = max 0 (viewSize - h)
          let vStartNew
                | cursor < viewStartOld + margin = max 0 (cursor - margin)
                | cursor >= viewStartOld + h - margin =
                    min maxViewStart (cursor - h + 1 + margin)
                | otherwise = viewStartOld
              vFinalStart = max 0 (min maxViewStart vStartNew)
          when (vFinalStart /= viewStartOld) $
            modify $
              set (compactViewLens . viewportStart) vFinalStart

adjustCursor :: (Int -> Int) -> GlobalAppState a
adjustCursor f = do
  ctx <- get
  let cv = view currentViewLens ctx
  let currentCursor = view cursorLens ctx
  let newCursor =
        if null cv
          then Nothing
          else fmap (\c -> clamp 0 (length cv - 1) (f c)) currentCursor
  modify $ set cursorLens newCursor
  adjustViewport

-- | Wrap a Task-mutating action so the change is captured in undo history
-- AND emitted to the events table. This is the single seam through which
-- every TUI keybinding-driven mutation should flow.
--
-- The action is run as-is; afterwards we compare the prior fileState against
-- the new one. If they differ we (a) push the new state onto undo history
-- and (b) compute one event per (file, taskIndex) that changed and write
-- those events to the configured DB.
--
-- Event emission failures are logged to stderr but never thrown — see
-- 'Tui.MutationEvents.emitMutationEvents'.
saveForUndo :: GlobalAppState Task -> GlobalAppState Task
saveForUndo f = do
  ctx <- get
  let oldHist = view (appState . fileState) ctx
      oldState = view L.currentState oldHist
  f
  newCtx <- get
  let newState = view fileStateLens newCtx
  when (newState /= oldState) $ do
    modify $ over (appState . fileState) (const $ L.append newState oldHist)
    let dbPath = TT._database (view config newCtx)
    liftIO $ emitMutationEventsForChange dbPath oldState newState

undo :: AppContext a -> AppContext a
undo = over (appState . fileState) L.undo

redo :: AppContext a -> AppContext a
redo = over (appState . fileState) L.redo

jumpBack :: AppContext a -> AppContext a
jumpBack = over (appState . compactView) L.undo

jumpForward :: AppContext a -> AppContext a
jumpForward = over (appState . compactView) L.redo

applySorter :: (a -> a -> Ordering) -> GlobalAppState a
applySorter sorter = do
  modify $ set viewSorterLens sorter
  modify $ set cursorLens (Just 0)
  modify $ set (compactViewLens . viewportStart) 0

applyFilterToAllTasks :: (a -> Bool) -> GlobalAppState a
applyFilterToAllTasks f = do
  modify $ set viewFilterLens [f]
  modify $ set cursorLens (Just 0)
  modify $ set (compactViewLens . viewportStart) 0

-- | Refreshes the TUI view by recomputing the cached view based on the current application state.
refreshTuiView :: GlobalAppState a
refreshTuiView = do
  ctx <- get
  let newCache = recomputeCurrentView ctx
  modify $ set (compactViewLens . cachedView) newCache

changeTodoKeyword :: T.Text -> AppContext Task -> AppContext Task
changeTodoKeyword keyword = over (currentTaskLens . todoKeyword) (const keyword)
