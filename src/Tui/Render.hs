{-# LANGUAGE FlexibleContexts #-}

module Tui.Render where

import Brick
import Control.Lens
import Graphics.Vty.Attributes
import Render.Render
import qualified Render.Render as R
import Tui.Types

import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Dialog (renderDialog)
import qualified Data.Vector as V

highlightAttr :: AttrName
highlightAttr = attrName "highlight"

theAppAttrMap :: AttrMap
theAppAttrMap =
  attrMap
    defAttr
    [ (highlightAttr, fg yellow) -- Set foreground to yellow
    ]

drawUI :: (RenderTask a Name) => AppContext a -> [Widget Name]
drawUI ctx =
  let mainLayers = drawCompactListView ctx
   in case view (appState . errorDialog) ctx of
        Just dlg -> renderDialog (view edDialog dlg) (strWrap $ view edMessage dlg) : mainLayers
        Nothing -> mainLayers

drawCompactListView :: (RenderTask a Name) => AppContext a -> [Widget Name]
drawCompactListView ctx =
  [ hBox
      [ padRight Max $ hLimitPercent 50 $ reportExtent CompactViewWidget compactTasks
      , hLimit 1 $ fill ' '
      , vBorder
      , padRight Max $ hLimitPercent 50 maybeFocusedTask
      ]
  ]
 where
  compView = view compactViewLens ctx
  start = view compactViewTaskStartIndex compView
  end = view compactViewTasksEndIndex compView
  taskPointers = V.slice start (end - start + 1) (view currentViewLens ctx)

  fs = view fileStateLens ctx
  compactTasks = vBox $ V.toList $ V.mapMaybe renderTask taskPointers
  maybeFocusedTask = maybe emptyWidget R.renderFull (preview currentTaskLens ctx)
  selectedTaskPtr = preview currentTaskPtr ctx

  renderTask ptr = if Just ptr == selectedTaskPtr then fmap (withAttr highlightAttr) renderedTask else renderedTask
   where
    renderedTask = fmap R.renderCompact (preview (taskBy ptr) fs)
