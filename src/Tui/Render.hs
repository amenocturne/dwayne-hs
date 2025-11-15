{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Tui.Render where

import Brick
import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Dialog (dialog, renderDialog)
import Control.Lens
import Core.Types (TaskPointer, file)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Graphics.Vty.Attributes
import Render.Render
import qualified Render.Render as R
import Searcher.Searcher
import System.Directory.Internal.Prelude (fromMaybe)
import Tui.ColorScheme
  ( ColorScheme,
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

-- Create attribute map from color scheme
createAttrMap :: ColorScheme -> AttrMap
createAttrMap scheme =
  attrMap
    (fg $ view defaultColor scheme)
    $ [ (highlightBgAttr, bg $ view highlightBgColor scheme),
        (tagAttr, fg $ view tagColor scheme),
        (timeFieldAttr, fg $ view timeFieldColor scheme),
        (propertyAttr, fg $ view propertyColor scheme),
        (descriptionAttr, fg $ view descriptionColor scheme)
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
   in let maybeErrorDialog = view (appState . errorDialog) ctx
          maybeRefileDialog = view (appState . refileDialog) ctx
          maybeValidationDialog = view (appState . validationDialog) ctx

          withRefileDialog dlg = renderRefileDialog dlg ctx : mainLayers
          withErrorDialog dlg = renderDialog (view edDialog dlg) (strWrap $ view edMessage dlg) : mainLayers
          withValidationDialog dlg = renderDialog (view vdDialog dlg) (strWrap $ view vdMessage dlg) : mainLayers

          layersWithRefile = maybe mainLayers withRefileDialog maybeRefileDialog
          layersWithValidation = maybe layersWithRefile withValidationDialog maybeValidationDialog
       in maybe layersWithValidation withErrorDialog maybeErrorDialog

drawCompactView :: (RenderTask a Name, Searcher a) => Maybe T.Text -> AppContext a -> Widget Name
drawCompactView mQuery ctx =
  hBox
    [ hLimitPercent 50 $
        maybe renderNormalView renderSearchView mQuery,
      vBorder,
      renderRightPane
    ]
  where
    fs = view fileStateLens ctx
    scheme = view (config . colorScheme) ctx
    cv = view currentViewLens ctx

    renderSearchView query =
      Widget Fixed Greedy $ do
        c <- getContext
        let h = availHeight c
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
            searchResultsSize = V.length searchResults
            v_start = 0 -- Always start from beginning in search view
        render $
          if searchResultsSize == 0
            then emptyWidget
            else
              let visibleTasks = V.slice v_start (min h searchResultsSize) searchResults
                  renderTask task = R.renderCompactWithColors (getColorScheme scheme) task
               in reportExtent CompactViewWidget $
                    vBox $
                      V.toList (V.map (padRight Max . renderTask) visibleTasks) ++ [padBottom Max (fill ' ')]

    renderNormalView =
      Widget Fixed Greedy $ do
        c <- getContext
        let h = availHeight c
            viewSize = V.length cv
            v_start = view (compactViewLens . viewportStart) ctx

        render $
          if viewSize == 0
            then emptyWidget
            else
              let taskPointers = V.slice v_start (min h (viewSize - v_start)) cv
                  selection = view selectionLens ctx
                  selectedTaskPtr = view currentTaskPtr ctx

                  renderTask idx ptr =
                    let absoluteIdx = v_start + idx
                        isSelected = Set.member absoluteIdx selection
                        isCursor = Just ptr == selectedTaskPtr
                        baseWidget = fmap (R.renderCompactWithColors $ getColorScheme scheme) (preview (taskBy ptr) fs)
                     in fmap
                          ( \widget ->
                              let styledWidget
                                    | isSelected || isCursor = withDefAttr highlightBgAttr widget
                                    | otherwise = widget
                               in padRight Max styledWidget
                          )
                          baseWidget
               in reportExtent CompactViewWidget $ vBox $ catMaybes $ V.toList (V.imap renderTask taskPointers)

    renderRightPane =
      let numberOfTasks = V.length cv
          maybeCurrentFile = fromMaybe "-" (preview (currentTaskPtr . _Just . file) ctx)
          modeIndicator = case view (appState . appMode) ctx of
            NormalMode -> ""
            CmdMode -> ""
            SelectionMode ->
              let selCount = Set.size (view selectionLens ctx)
               in " -- SELECTION (" ++ show selCount ++ ") --"
          maybeFocusedTask = case mQuery of
            Just _ -> emptyWidget -- No focused task in search view
            Nothing -> maybe emptyWidget (R.renderFullWithColors $ getColorScheme scheme) (preview currentTaskLens ctx)
       in vBox
            [ str $ "Number of tasks in view: " ++ show numberOfTasks,
              str $ "Task file: " ++ maybeCurrentFile,
              str modeIndicator,
              vLimit 1 $ fill '-',
              maybeFocusedTask
            ]

cmdTypeToPrefix :: CmdType -> T.Text
cmdTypeToPrefix Command = ":"
cmdTypeToPrefix Search = "/"

cmdWidget :: CmdType -> T.Text -> Widget Name
cmdWidget cmdType query =
  let prefix = cmdTypeToPrefix cmdType
   in vLimit 1 $ showCursor CmdWidget (Location (T.length prefix + T.length query, 0)) (txt (prefix <> query <> " "))

messageWidget :: T.Text -> Widget Name
messageWidget msg = vLimit 1 $ txt msg

renderRefileDialog :: (RenderTask a Name, Searcher a) => RefileDialog -> AppContext a -> Widget Name
renderRefileDialog refileDialog ctx =
  let fs = view fileStateLens ctx
      allProjects = view rdProjects refileDialog
      searchQuery = view rdSearchQuery refileDialog
      selectedIdx = view rdSelectedIndex refileDialog

      filteredProjects =
        if T.null searchQuery
          then allProjects
          else filter (\ptr -> maybe False (matches searchQuery) (preview (taskBy ptr) fs)) allProjects

      renderProject idx ptr =
        let isSelected = idx == selectedIdx
            widget = maybe (txt "<missing task>") renderCompact (preview (taskBy ptr) fs)
         in if isSelected
              then withDefAttr highlightBgAttr widget
              else widget

      projectWidgets = zipWith renderProject [0 ..] filteredProjects
      projectList = vBox $ take 10 projectWidgets

      searchWidget = txt ("Search: " <> searchQuery <> "_")

      dialogContent =
        vBox
          [ str "Refile to project:",
            vLimit 1 $ fill '-',
            searchWidget,
            vLimit 1 $ fill '-',
            projectList,
            vLimit 1 $ fill '-',
            str "Enter: Select  Escape: Cancel  Up/Down: Navigate"
          ]
   in renderDialog
        (dialog (Just $ str "Refile") Nothing 60)
        dialogContent
