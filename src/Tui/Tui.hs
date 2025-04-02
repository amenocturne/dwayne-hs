{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tui.Tui where

import Brick
import qualified Brick.Types as BT
import Brick.Widgets.Center
import Control.Lens
import Data.Functor
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Model.OrgMode (TaskFile (content))
import Render.Render
import qualified Render.Render as R
import Writer.Writer

import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import GHC.Base (when)
import Parser.Parser
import TextUtils

-- TODO: I should go through the code, collect all the errors and create widget to properly display them
-- TODO: make a shortcut to open in a default browser first found link in a task (useful for music/articles)
-- TODO: make a shortcut to download music from youtube/youtube music links
-- TODO: make a shortcut to save note contents directly to obsidian vault and open obsidian with this file to continue editing

data AppConfig a = AppConfig
  { files :: [String]
  , fileParser :: Parser (TaskFile a)
  , taskParser :: Parser a
  , scrollingMargin :: Int
  }

class Tui a where
  tui :: AppConfig a -> IO ()

data AppState = CompactMode | FullMode

data AppContext a = AppContext
  { tasks :: [a]
  , currentCursor :: Int
  , appState :: AppState
  , config :: AppConfig a
  }

-- TODO: shortcuts abstraction
-- data AppShortcut = SimpleShortcut {key :: Char, modifyState :: AppContext -> AppContext}
--
-- shortcuts :: [AppShortcut]
-- shortcuts =
--   [ SimpleShortcut 'k' (\s -> s{currentCursor = max 0 (currentCursor s - 1)})
--   ]

highlightAttr :: AttrName
highlightAttr = attrName "highlight"

theAppAttrMap :: AttrMap
theAppAttrMap =
  attrMap
    defAttr
    [ (highlightAttr, fg yellow) -- Set foreground to yellow
    ]

handleEvent :: (Writer a) => BrickEvent Name e -> EventM Name (AppContext a) ()
handleEvent (VtyEvent (EvKey (KChar 'k') [])) = do
  state <- get
  let newCursor = max 0 (currentCursor state - 1)
  modify (\s -> s{currentCursor = newCursor})
  adjustViewport
handleEvent (VtyEvent (EvKey (KChar 'j') [])) = do
  state <- get
  let newCursor = min (currentCursor state + 1) (length (tasks state) - 1)
  modify (\s -> s{currentCursor = newCursor})
  adjustViewport
handleEvent (VtyEvent (EvKey (KChar 'G') [])) = do
  state <- get
  let newCursor = length (tasks state) - 1
  modify (\s -> s{currentCursor = newCursor})
  adjustViewport
handleEvent (VtyEvent (EvKey (KChar 'q') [])) = halt -- Exit application
handleEvent (VtyEvent (EvKey KEnter [])) = do
  state <- get
  when (null (tasks state)) $ return ()
  let currentTask = tasks state !! currentCursor state
  suspendAndResume $ do
    editedContent <- editWithEditor (write currentTask)
    when (null editedContent) $ return ()
    case editedContent of
      Nothing -> return state
      Just editedStr -> do
        let (_, _, result) = runParser (taskParser $ config state) (T.pack editedStr)
        case result of
          ParserSuccess t -> do
            let updatedTasks = take (currentCursor state) (tasks state) ++ [t] ++ drop (currentCursor state + 1) (tasks state)
            return state{tasks = updatedTasks}
          ParserFailure e -> do
            putStrLn $ "Parser error: " ++ show e -- TODO: show error in UI
            return state
handleEvent _ = return () -- Ignore other events

adjustViewport :: EventM Name (AppContext a) ()
adjustViewport = do
  ctx <- get
  let cursor = currentCursor ctx
      marginVal = scrollingMargin $ config ctx
  mvp <- lookupViewport Viewport1
  case mvp of
    Just vp -> do
      let currentTop = vp ^. BT.vpTop
          visibleHeight = snd (vp ^. BT.vpSize)
          newTop
            | cursor >= currentTop + visibleHeight - marginVal =
                cursor - (visibleHeight - marginVal - 1)
            | cursor <= currentTop + marginVal =
                max 0 (cursor - marginVal)
            | otherwise = currentTop
      setTop (viewportScroll Viewport1) newTop
    Nothing -> return ()

ui :: String -> Widget Name
ui text = vBox [hCenter $ str "Top widget", center $ txtWrap (T.pack text)]

drawUI :: (RenderTask a Name) => AppContext a -> [Widget Name]
drawUI (AppContext ts cursor appState _) = case appState of
  FullMode ->
    [hCenter $ str "Top widget", hCenter $ vCenter renderedTask]
   where
    renderedTask = R.renderFull $ ts !! cursor
  CompactMode -> drawCompactListView cursor ts

data Name = Viewport1 deriving (Eq, Ord, Show)

drawCompactListView :: (RenderTask a Name) => Int -> [a] -> [Widget Name]
drawCompactListView cursor ts =
  [ joinBorders $
      withBorderStyle unicodeRounded $
        hBox [hLimitPercent 50 $ viewport Viewport1 Vertical compactTasks, hLimit 1 $ fill ' ', vBorder, focusedTask]
  ]
 where
  compactTasks = vBox withHighlight
  withHighlight = zipWith (\i x -> if i == cursor then withAttr highlightAttr x else x) [0 ..] simplyRendered
  simplyRendered = fmap R.renderCompact ts
  focusedTask = R.renderFull (ts !! cursor)

app :: (RenderTask a Name, Writer a) => App (AppContext a) e Name
app =
  App
    { appDraw = drawUI -- List in type signature because each element is a layer and thus you can put widgets on top of one another
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const theAppAttrMap
    }

instance (RenderTask a Name, Writer a) => Tui a where
  tui config = do
    parsedFiles <- sequence <$> mapM (readTasks (fileParser config)) (files config)
    case parsedFiles of
      ParserSuccess files -> void $ defaultMain app (AppContext (concatMap content files) 0 CompactMode config)
      -- NOTE: useful code below to save file
      -- ParserSuccess (TaskFile name tasks) -> do
      -- let wrote = write (TaskFile name tasks)
      -- void $ writeFileExample "./resources/parsed.org" wrote
      ParserFailure e -> simpleMain (ui (show e))
    return ()
   where
    readTasks :: Parser a -> FilePath -> IO (ParserResult a)
    readTasks p file = do
      content <- readFileExample file
      let (_, _, tasks) = runParser p content
      return tasks
