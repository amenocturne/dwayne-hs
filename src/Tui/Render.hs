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
import Data.Maybe (fromMaybe, maybeToList)
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
  case view (appState . appMode) ctx of
    NormalMode ->
      let mainLayers = drawCompactListView ctx
       in case view (appState . errorDialog) ctx of
            Just dlg -> renderDialog (view edDialog dlg) (strWrap $ view edMessage dlg) : mainLayers
            Nothing -> mainLayers
    SearchMode ->
      let searchQuery = fromMaybe T.empty (preview (appState . searchState . _Just . searchInput) ctx)
          maybeSearchWidget =
            if view (appState . appMode) ctx == SearchMode
              then Just $ searchWidget searchQuery
              else Nothing
          mainLayers = [vBox $ drawCompactSearchView ctx ++ maybeToList maybeSearchWidget]
       in case view (appState . errorDialog) ctx of
            Just dlg -> renderDialog (view edDialog dlg) (strWrap $ view edMessage dlg) : mainLayers
            Nothing -> mainLayers

drawCompactSearchView :: (RenderTask a Name, Searcher a) => AppContext a -> [Widget Name]
drawCompactSearchView ctx =
  [ hBox
      [ padRight Max $ reportExtent CompactViewWidget compactTasks
      , hLimit 1 $ fill ' '
      , vBorder
      , padRight Max emptyWidget
      ]
  ]
 where
  compView = view compactViewLens ctx
  start = view compactViewTaskStartIndex compView
  end = view compactViewTasksEndIndex compView

  maybeQuery = preview (appState . searchState . _Just . searchInput) ctx
  cv = view currentViewLens ctx
  fs = view fileStateLens ctx
  tasks = V.catMaybes $ fmap (\p -> preview (taskBy p) fs) cv
  searchResults = maybe tasks (\q -> if T.null q then tasks else V.filter (matches q) tasks) maybeQuery
  displayedTasks = V.slice 0 (min (end - start + 1) (V.length searchResults)) searchResults
  compactTasks =
    if V.length displayedTasks == 0
      then fill ' '
      else
        vBox $ V.toList $ V.map R.renderCompact displayedTasks

drawCompactListView :: (RenderTask a Name) => AppContext a -> [Widget Name]
drawCompactListView ctx =
  [ hBox
      [ padRight Max $ reportExtent CompactViewWidget compactTasks
      , hLimit 1 $ fill ' '
      , vBorder
      , padRight Max $ maybeFocusedTask
      ]
  ]
 where
  compView = view compactViewLens ctx
  start = view compactViewTaskStartIndex compView
  end = view compactViewTasksEndIndex compView
  taskPointers = V.slice start (end - start + 1) (view currentViewLens ctx)

  fs = view fileStateLens ctx
  compactTasks = if V.length taskPointers == 0 then fill ' ' else vBox $ V.toList $ V.mapMaybe renderTask taskPointers
  maybeFocusedTask = maybe emptyWidget R.renderFull (preview currentTaskLens ctx)
  selectedTaskPtr = preview currentTaskPtr ctx

  renderTask ptr = if Just ptr == selectedTaskPtr then fmap (withAttr highlightAttr) renderedTask else renderedTask
   where
    renderedTask = fmap R.renderCompact (preview (taskBy ptr) fs)

searchWidget :: T.Text -> Widget Name
searchWidget query = vLimit 1 $ hBox [str "/", txt query]
