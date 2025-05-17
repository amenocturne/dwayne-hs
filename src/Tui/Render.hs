{-# LANGUAGE FlexibleContexts #-}

module Tui.Render where

import Brick
import Brick.Widgets.Center
import Control.Lens
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Render.Render
import qualified Render.Render as R
import Tui.Types

import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Dialog (renderDialog)
import qualified Data.Vector as V

ui :: String -> Widget Name
ui text = vBox [hCenter $ str "Top widget", center $ txtWrap (T.pack text)]

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
  [joinBorders $ withBorderStyle unicodeRounded $ hBox [hLimitPercent 50 $ viewport Viewport1 Vertical compactTasks, hLimit 1 $ fill ' ', vBorder, maybeFocusedTask]]
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
