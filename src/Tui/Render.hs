{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Tui.Render where

import Brick
import Control.Lens
import Graphics.Vty.Attributes
import Render.Render
import qualified Render.Render as R
import Tui.ColorScheme (
  ColorScheme,
  defaultColor,
  descriptionAttr,
  descriptionColor,
  getColorScheme,
  highlightBgAttr,
  highlightBgColor,
  levelAttr,
  levelColors,
  priorityAttr,
  priorityColors,
  propertyAttr,
  propertyColor,
  tagAttr,
  tagColor,
  timeFieldAttr,
  timeFieldColor,
  todoKeywordAttr,
  todoKeywordColors,
 )

import Tui.Types

import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Dialog (renderDialog)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Searcher.Searcher
import System.Directory.Internal.Prelude (fromMaybe)

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
        NormalMode -> [drawCompactView Nothing ctx]
        SelectionMode -> [drawCompactView Nothing ctx]
        CmdMode ->
          let cmdW = case view (appState . cmdState) ctx of
                Just (Typing t q) -> cmdWidget t q
                Just (ShowingMessage m) -> messageWidget m
                Nothing -> emptyWidget
              mainView = case view (appState . cmdState) ctx of
                Just (Typing Search q) -> drawCompactView (Just q) ctx
                _ -> drawCompactView Nothing ctx
           in [vBox [mainView, cmdW]]
   in case view (appState . errorDialog) ctx of
        Just dlg -> renderDialog (view edDialog dlg) (strWrap $ view edMessage dlg) : mainLayers
        Nothing -> mainLayers

drawCompactView :: (RenderTask a Name, Searcher a) => Maybe T.Text -> AppContext a -> Widget Name
drawCompactView mQuery ctx =
  hBox
    [ padRight Max $ reportExtent CompactViewWidget compactTasks
    , hLimit 1 $ fill ' '
    , vBorder
    , padRight Max $
        vBox
          [ str $ "Number of tasks in view: " ++ show numberOfTasks
          , str $ "Task file: " ++ maybeCurrentFile
          , str modeIndicator
          , vLimit 1 $ fill '-'
          , maybeFocusedTask
          ]
    ]
 where
  compView = view compactViewLens ctx
  start = view compactViewTaskStartIndex compView
  end = view compactViewTasksEndIndex compView
  scheme = view (config . colorScheme) ctx
  cv = view currentViewLens ctx
  fs = view fileStateLens ctx

  modeIndicator = case view (appState . appMode) ctx of
    NormalMode -> ""
    CmdMode -> ""
    SelectionMode ->
      let selCount = Set.size (view selectionLens ctx)
       in " -- SELECTION (" ++ show selCount ++ ") --"

  (compactTasks, maybeFocusedTask, numberOfTasks, maybeCurrentFile) = case mQuery of
    Just query -> (compactTasks, emptyWidget, numberOfTasks, "-")
     where
      allPtrs = getAllPointers fs
      vs = view (compactViewLens . viewSpec) ctx
      ptrsWithTasks = V.mapMaybe (\ptr -> fmap (ptr,) (fs ^? taskBy ptr)) allPtrs
      currentFilters = view vsFilters vs
      searchFilter = matches query
      effectiveFilters = if T.null query then currentFilters else searchFilter : currentFilters
      filtered = V.filter (\(_, task) -> all (\f -> f task) effectiveFilters) ptrsWithTasks
      sorter = view vsSorter vs
      sorted = sortByVector (\(_, t1) (_, t2) -> sorter t1 t2) filtered
      searchResults = V.map snd sorted

      numberOfTasks = V.length searchResults
      displayedTasks = V.take (min (end - start + 1) (V.length searchResults)) searchResults
      compactTasks =
        vBox $ V.toList (V.map (R.renderCompactWithColors $ getColorScheme scheme) displayedTasks) ++ [padBottom Max (fill ' ')]
    Nothing -> (compactTasks, maybeFocusedTask, numberOfTasks, maybeCurrentFile)
     where
      numberOfTasks = V.length cv
      selectedTaskPtr = view currentTaskPtr ctx
      selection = view selectionLens ctx
      taskPointers = V.take (end - start + 1) $ V.drop start cv
      renderTask idx ptr =
        let absoluteIdx = start + idx
            isSelected = Set.member absoluteIdx selection
            isCursor = Just ptr == selectedTaskPtr
            baseWidget = fmap (R.renderCompactWithColors $ getColorScheme scheme) (preview (taskBy ptr) fs)
         in case baseWidget of
              Just widget ->
                let styledWidget
                      | isSelected || isCursor = withDefAttr highlightBgAttr widget
                      | otherwise = widget
                 in Just $ padRight Max styledWidget
              Nothing -> Nothing
      compactTasks = vBox $ V.toList (V.imapMaybe renderTask taskPointers)
      maybeFocusedTask = maybe emptyWidget (R.renderFullWithColors $ getColorScheme scheme) (preview currentTaskLens ctx)
      maybeCurrentFile :: String
      maybeCurrentFile = fromMaybe "-" (preview (currentTaskPtr . _Just . file) ctx)

cmdTypeToPrefix :: CmdType -> T.Text
cmdTypeToPrefix Command = ":"
cmdTypeToPrefix Search = "/"

cmdWidget :: CmdType -> T.Text -> Widget Name
cmdWidget cmdType query =
  let prefix = cmdTypeToPrefix cmdType
   in vLimit 1 $ showCursor CmdWidget (Location (T.length prefix + T.length query, 0)) (txt (prefix <> query <> " "))

messageWidget :: T.Text -> Widget Name
messageWidget msg = vLimit 1 $ txt msg
