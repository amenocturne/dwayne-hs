{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tui.Helpers where

import Brick
import Control.Lens
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Model.LinearHistory as L
import Model.OrgMode (Task, todoKeyword)
import Tui.Types
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

saveForUndo :: (Eq a) => GlobalAppState a -> GlobalAppState a
saveForUndo f = do
  ctx <- get
  let oldHist = view (appState . fileState) ctx
  f
  newCtx <- get
  let newState = view fileStateLens newCtx
  when (newState /= view L.currentState oldHist) $ modify $ over (appState . fileState) (const $ L.append newState oldHist)

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
