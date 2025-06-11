{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tui.Render where

import Brick
import Control.Lens
import Graphics.Vty.Attributes
import Render.Render
import qualified Render.Render as R
import Tui.Types

import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Dialog (renderDialog)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Vector as V
import Searcher.Searcher

highlightAttr :: AttrName
highlightAttr = attrName "highlight"

theAppAttrMap :: AttrMap
theAppAttrMap =
  attrMap
    defAttr
    [ (highlightAttr, fg yellow) -- Set foreground to yellow
    ]

drawUI :: (RenderTask a Name, Searcher a) => AppContext a -> [Widget Name]
drawUI ctx =
  let mainLayers = case view (appState . appMode) ctx of
        NormalMode -> [drawCompactListView True ctx]
        CmdMode ->
          let cmdW = case view (appState . cmdState) ctx of
                Just (Typing p q) -> cmdWidget p q
                Just (ShowingMessage m) -> messageWidget m
                Nothing -> emptyWidget
              mainView = case view (appState . cmdState) ctx of
                Just (Typing "/" q) -> drawCompactSearchView q ctx
                _ -> drawCompactListView False ctx
           in [vBox [mainView, cmdW]]
   in case view (appState . errorDialog) ctx of
        Just dlg -> renderDialog (view edDialog dlg) (strWrap $ view edMessage dlg) : mainLayers
        Nothing -> mainLayers

drawCompactSearchView :: (RenderTask a Name, Searcher a) => T.Text -> AppContext a -> Widget Name
drawCompactSearchView query ctx =
  hBox
    [ padRight Max $ reportExtent CompactViewWidget compactTasks
    , hLimit 1 $ fill ' '
    , vBorder
    , padRight Max $ vBox [str $ "Number of tasks in view: " ++ show numberOfTasks, vLimit 1 $ fill '-']
    ]
 where
  compView = view compactViewLens ctx
  start = view compactViewTaskStartIndex compView
  end = view compactViewTasksEndIndex compView

  cv = view currentViewLens ctx
  fs = view fileStateLens ctx
  tasks = V.catMaybes $ fmap (\p -> preview (taskBy p) fs) cv
  searchResults = if T.null query then tasks else V.filter (matches query) tasks
  displayedTasks = V.slice 0 (min (end - start + 1) (V.length searchResults)) searchResults
  numberOfTasks = V.length searchResults
  compactTasks =
    if V.length displayedTasks == 0
      then fill ' '
      else
        vBox $ V.toList (V.map R.renderCompact displayedTasks) ++ [padBottom Max (fill ' ')]

drawCompactListView :: (RenderTask a Name) => Bool -> AppContext a -> Widget Name
drawCompactListView withPadding ctx =
  hBox
    [ padRight Max $ reportExtent CompactViewWidget compactTasks
    , hLimit 1 $ fill ' '
    , vBorder
    , padRight Max $ vBox [str $ "Number of tasks in view: " ++ show numberOfTasks, vLimit 1 $ fill '-', maybeFocusedTask]
    ]
 where
  compView = view compactViewLens ctx
  start = view compactViewTaskStartIndex compView
  end = view compactViewTasksEndIndex compView
  taskPointers = V.slice start (end - start + 1) (view currentViewLens ctx)
  numberOfTasks = V.length (view currentViewLens ctx)

  fs = view fileStateLens ctx
  padding = if withPadding then [padBottom Max (fill ' ')] else []
  compactTasks = vBox $ V.toList (V.mapMaybe renderTask taskPointers) ++ padding
  maybeFocusedTask = maybe emptyWidget R.renderFull (preview currentTaskLens ctx)
  selectedTaskPtr = preview currentTaskPtr ctx

  renderTask ptr = if Just ptr == selectedTaskPtr then fmap (withAttr highlightAttr) renderedTask else renderedTask
   where
    renderedTask = fmap R.renderCompact (preview (taskBy ptr) fs)

cmdWidget :: T.Text -> T.Text -> Widget Name
cmdWidget prefix query = vLimit 1 $ showCursor CmdWidget (Location (T.length prefix + T.length query, 0)) (txt (prefix <> query <> " "))

messageWidget :: T.Text -> Widget Name
messageWidget msg = vLimit 1 $ txt msg
