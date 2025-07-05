{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tui.Render where

import Brick
import Control.Lens
import Graphics.Vty.Attributes
import Render.Render
import qualified Render.Render as R
import Tui.ColorScheme (ColorScheme, defaultColor, descriptionAttr, descriptionColor, highlightBgAttr, highlightBgColor, levelAttr, levelColors, priorityAttr, priorityColors, propertyAttr, propertyColor, tagAttr, tagColor, timeFieldAttr, timeFieldColor, todoKeywordAttr, todoKeywordColors, getColorScheme)
import Tui.Types

import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Dialog (renderDialog)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Vector as V
import Searcher.Searcher

-- Create attribute map from color scheme
createAttrMap :: ColorScheme -> AttrMap
createAttrMap scheme =
  attrMap
    (fg $ view defaultColor scheme)
    $ [ (highlightBgAttr, bg $ view highlightBgColor scheme)
      , (tagAttr, fg $ view tagColor scheme)
      , (timeFieldAttr, fg $ view timeFieldColor scheme)
      , (propertyAttr, fg $ view propertyColor scheme)
      , (descriptionAttr, fg $ view descriptionColor scheme)
      ]
      ++ todoKeywordAttrs
      ++ priorityAttrs
      ++ levelAttrs
 where
  todoKeywordAttrs =
    [ (todoKeywordAttr keyword, fg color)
    | (keyword, color) <- M.toList (view todoKeywordColors scheme)
    ]

  priorityAttrs =
    [ (priorityAttr i, fg color)
    | (i, color) <- zip [0 ..] (view priorityColors scheme)
    ]

  levelAttrs =
    [ (levelAttr i, fg color)
    | (i, color) <- zip [1 ..] (view levelColors scheme)
    ]

theAppAttrMap :: ColorScheme -> AttrMap
theAppAttrMap = createAttrMap

drawUI :: (RenderTask a Name, Searcher a) => AppContext a -> [Widget Name]
drawUI ctx =
  let mainLayers = case view (appState . appMode) ctx of
        NormalMode -> [drawCompactListView ctx]
        CmdMode ->
          let cmdW = case view (appState . cmdState) ctx of
                Just (Typing t q) -> cmdWidget t q
                Just (ShowingMessage m) -> messageWidget m
                Nothing -> emptyWidget
              mainView = case view (appState . cmdState) ctx of
                Just (Typing Search q) -> drawCompactSearchView q ctx
                _ -> drawCompactListView ctx
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
  scheme = view (config . colorScheme) ctx
  cv = view currentViewLens ctx
  fs = view fileStateLens ctx
  numberOfTasks = V.length searchResults

  tasks = V.catMaybes $ fmap (\p -> preview (taskBy p) fs) cv
  searchResults = if T.null query then tasks else V.filter (matches query) tasks
  displayedTasks = V.take (min (end - start + 1) (V.length searchResults)) searchResults
  compactTasks =
    if V.length displayedTasks == 0
      then fill ' '
      else
        vBox $ V.toList (V.map (R.renderCompactWithColors $ getColorScheme scheme) displayedTasks) ++ [padBottom Max (fill ' ')]

drawCompactListView :: (RenderTask a Name) => AppContext a -> Widget Name
drawCompactListView ctx =
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
  scheme = view (config . colorScheme) ctx
  cv = view currentViewLens ctx
  fs = view fileStateLens ctx
  numberOfTasks = V.length cv


  taskPointers = V.take (end - start + 1) $ V.drop start cv
  compactTasks = vBox $ V.toList (V.mapMaybe renderTask taskPointers)
  maybeFocusedTask = maybe emptyWidget (R.renderFullWithColors $ getColorScheme scheme) (preview currentTaskLens ctx)
  selectedTaskPtr = preview currentTaskPtr ctx

  renderTask ptr =
    if Just ptr == selectedTaskPtr
      then fmap (withDefAttr highlightBgAttr . padRight Max) renderedTask
      else renderedTask
   where
    renderedTask = fmap (R.renderCompactWithColors $ getColorScheme scheme) (preview (taskBy ptr) fs)

cmdTypeToPrefix :: CmdType -> T.Text
cmdTypeToPrefix Command = ":"
cmdTypeToPrefix Search = "/"

cmdWidget :: CmdType -> T.Text -> Widget Name
cmdWidget cmdType query =
  let prefix = cmdTypeToPrefix cmdType
   in vLimit 1 $ showCursor CmdWidget (Location (T.length prefix + T.length query, 0)) (txt (prefix <> query <> " "))

messageWidget :: T.Text -> Widget Name
messageWidget msg = vLimit 1 $ txt msg
