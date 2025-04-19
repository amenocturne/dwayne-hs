{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tui.Tui where

import Control.Monad (forM_, when)

import Brick
import qualified Brick.Types as BT
import Brick.Widgets.Center
import Control.Lens
import Data.Functor
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Model.OrgMode
import Render.Render
import qualified Render.Render as R
import System.Exit (exitFailure)
import Writer.Writer

import Brick.Keybindings as K
import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Data.List
import Parser.Parser
import TextUtils

-- TODO: I should go through the code, collect all the errors and create widget to properly display them
-- TODO: make a shortcut to open in a default browser first found link in a task (useful for music/articles)
-- TODO: make a shortcut to download music from youtube/youtube music links
-- TODO: make a shortcut to save note contents directly to obsidian vault and open obsidian with this file to continue editing
-- TODO: Should handle external edits to files when the app is opened and update its state correctly

data AppContext a = AppContext
  { _appState :: AppState a
  , _config :: AppConfig a
  , _keyEventDispatcher :: K.KeyDispatcher KeyEvent (EventM Name (AppContext a))
  }

data KeyEvent = MoveUp | MoveDown | JumpEnd | Quit | EditInEditor
  deriving (Eq, Show, Ord, Enum, Bounded)

data Name = Viewport1 deriving (Eq, Ord, Show)

type FileState a = M.Map String (ParserResult (TaskFile a))

data AppConfig a = AppConfig
  { _files :: [String]
  , _fileParser :: Parser (TaskFile a)
  , _taskParser :: Parser a
  , _scrollingMargin :: Int
  }

data AppState a = AppState
  { _fileState :: FileState a
  , _currentView :: [TaskPointer]
  , _currentTask :: Maybe Int -- Index of a currently focused task in a view
  }

data TaskPointer = TaskPointer
  { _file :: FilePath
  , _taskIndex :: Int
  }
  deriving (Eq)

--------------------------------- Optics ---------------------------------------

makeLenses ''TaskPointer
makeLenses ''AppState
makeLenses ''AppContext
makeLenses ''AppConfig

currentCursor :: Traversal' (AppContext a) (Maybe Int)
currentCursor = appState . currentTask

-- TODO: rewrite as traversable
modifyView :: ([TaskPointer] -> [TaskPointer]) -> AppState a -> AppState a
modifyView f s@(AppState _ cv ct) = (set currentView newView . set currentTask selectedTask) s
 where
  newView = f cv
  selectedTask = do
    selected <- ct
    selectedPtr <- cv ^? element selected
    found <- find (== selectedPtr) newView
    fmap fst $ find (\(_, t) -> t == found) $ zip [0 ..] newView

-- TODO: rewrite as traversable
filterView :: (TaskPointer -> Bool) -> AppState a -> AppState a
filterView f = modifyView (filter f)

taskBy :: TaskPointer -> Traversal' (FileState a) a
taskBy ptr =
  ix (ptr ^. file)
    . success
    . content
    . ix (ptr ^. taskIndex)

currentTaskLens :: Traversal' (AppState a) a
currentTaskLens f appState =
  case (appState ^. currentTask, appState ^. currentView) of
    (Just i, cv)
      | i >= 0
      , i < length cv ->
          let ptr = cv !! i
           in (\modifiedFile -> set fileState modifiedFile appState)
                <$> traverseOf (taskBy ptr) f (view fileState appState)
    _ -> pure appState

currentTaskPtr :: Traversal' (AppState a) TaskPointer
currentTaskPtr f appState =
  case (appState ^. currentTask, appState ^. currentView) of
    (Just i, cv)
      | i >= 0
      , i < length cv ->
          appState & currentView . ix i %%~ f
    _ -> pure appState

----------------------- Key bindings ------------------------------------------

allKeyEvents :: [KeyEvent]
allKeyEvents = [minBound .. maxBound]

keyEventsMapping :: K.KeyEvents KeyEvent
keyEventsMapping =
  K.keyEvents $
    fmap (\k -> (T.pack $ show k, k)) allKeyEvents

bindKey :: KeyEvent -> [K.Binding]
bindKey MoveUp = [K.bind 'k']
bindKey MoveDown = [K.bind 'j']
bindKey JumpEnd = [K.bind 'G']
bindKey Quit = [K.bind 'q']
bindKey EditInEditor = [K.bind KEnter]

defaultBindings :: [(KeyEvent, [K.Binding])]
defaultBindings = fmap (\k -> (k, bindKey k)) allKeyEvents

-- TODO: refactor
adjustViewport :: EventM Name (AppContext a) ()
adjustViewport = do
  ctx <- get
  case view (appState . currentTask) ctx of
    Just cursor -> do
      let marginVal = view (config . scrollingMargin) ctx
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
    Nothing -> return ()

adjustCursor :: (Int -> Int) -> EventM Name (AppContext a) ()
adjustCursor f = do
  state <- get
  let cv = view (appState . currentView) state
  let modifyCursor c = clamp 0 (length cv - 1) (f c)
  modify $ over (currentCursor . _Just) modifyCursor
  adjustViewport

-- TODO: refactor
editSelectedTaskInEditor :: (Writer a) => EventM Name (AppContext a) ()
editSelectedTaskInEditor = do
  ctx <- get
  let currentTask = preview (appState . currentTaskLens) ctx
  let maybePtr = preview (appState . currentTaskPtr) ctx
  case (currentTask, maybePtr) of
    (Just task, Just ptr) -> suspendAndResume $ do
      editedContent <- editWithEditor (write task)
      when (null editedContent) $ return ()
      case editedContent of
        Nothing -> return ctx
        Just editedStr -> do
          let (_, _, result) = runParser (view (config . taskParser) ctx) (T.pack editedStr)
          case result of
            ParserSuccess t -> do
              return $ set (appState . fileState . taskBy ptr) t ctx
            ParserFailure e -> do
              putStrLn $ "Parser error: " ++ show e -- TODO: show error in UI
              return ctx
    _ -> return ()

handleKeyEvent :: (Writer a) => KeyEvent -> K.KeyEventHandler KeyEvent (EventM Name (AppContext a))
handleKeyEvent MoveUp = K.onEvent MoveUp "Move up" $ adjustCursor (\i -> i - 1)
handleKeyEvent MoveDown = K.onEvent MoveDown "Move down" $ adjustCursor (+ 1)
handleKeyEvent JumpEnd = K.onEvent JumpEnd "Jump to the end" $ adjustCursor (const maxBound)
handleKeyEvent Quit = K.onEvent Quit "Quit" halt
handleKeyEvent EditInEditor = K.onEvent EditInEditor "Edit in editor" editSelectedTaskInEditor

handlers :: (Writer a) => [K.KeyEventHandler KeyEvent (EventM Name (AppContext a))]
handlers = fmap handleKeyEvent allKeyEvents

---------------------------- Events -------------------------------------------

handleEvent :: BrickEvent Name e -> EventM Name (AppContext a) ()
handleEvent (VtyEvent (EvKey k mods)) = do
  state <- get
  void $ K.handleKey (view keyEventDispatcher state) k mods
handleEvent _ = return ()

----------------------------- UI -----------------------------------------------

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
  [ joinBorders $
      withBorderStyle unicodeRounded $
        hBox [hLimitPercent 50 $ viewport Viewport1 Vertical compactTasks, hLimit 1 $ fill ' ', vBorder, maybeFocusedTask]
  ]
 where
  taskPointers = view (appState . currentView) ctx
  fs = view (appState . fileState) ctx
  compactTasks = vBox $ mapMaybe renderTask taskPointers
  maybeFocusedTask = maybe emptyWidget R.renderFull (preview (appState . currentTaskLens) ctx)
  selectedTaskPtr = preview (appState . currentTaskPtr) ctx
  renderTask ptr = if Just ptr == selectedTaskPtr then fmap (withAttr highlightAttr) renderedTask else renderedTask
   where
    renderedTask = fmap R.renderCompact (preview (taskBy ptr) fs)

------------------------ Initialization ----------------------------------------

getAllPointers :: FileState a -> [TaskPointer]
getAllPointers files = concatMap f (M.toList files)
 where
  f (file, result) =
    maybe
      []
      (\taskFile -> (\(i, _) -> TaskPointer file i) <$> zip [0 ..] (_content taskFile))
      (resultToMaybe result)

getKeyDispatcher :: (Writer a) => IO (KeyDispatcher KeyEvent (EventM Name (AppContext a)))
getKeyDispatcher = do
  let kc = K.newKeyConfig keyEventsMapping defaultBindings [] -- Maybe I should add config files in the future
  case K.keyDispatcher kc handlers of
    Right d -> return d
    Left collisions -> do
      putStrLn "Error: some key events have the same keys bound to them."
      forM_ collisions $ \(b, hs) -> do
        T.putStrLn $ "Handlers with the '" <> K.ppBinding b <> "' binding:"
        forM_ hs $ \h -> do
          let trigger = case K.kehEventTrigger $ K.khHandler h of
                K.ByKey k -> "triggered by the key '" <> K.ppBinding k <> "'"
                K.ByEvent e -> "triggered by the event '" <> fromJust (K.keyEventName keyEventsMapping e) <> "'"
              desc = K.handlerDescription $ K.kehHandler $ K.khHandler h

          T.putStrLn $ "  " <> desc <> " (" <> trigger <> ")"
      exitFailure

app :: (RenderTask a Name, Writer a) => App (AppContext a) e Name
app =
  App
    { appDraw = drawUI -- List in type signature because each element is a layer and thus you can put widgets on top of one another
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const theAppAttrMap
    }

class Tui a where
  tui :: AppConfig a -> IO ()

instance (RenderTask a Name, Writer a, Show a) => Tui a where
  tui config = do
    keyDispatcher <- getKeyDispatcher
    parsedFiles <- mapM (\f -> fmap (\t -> (f, t)) (readTasks (view fileParser config) f)) (view files config)
    let fState = M.fromList parsedFiles
    let pointers = getAllPointers fState
    let state =
          AppState
            { _fileState = fState
            , _currentView = pointers
            , _currentTask = 0 <$ listToMaybe pointers
            }
    let ctx =
          AppContext
            { _appState = state
            , _config = config
            , _keyEventDispatcher = keyDispatcher
            }
    void $ defaultMain app ctx
   where
    -- case parsedFiles of
    -- ParserSuccess files -> void $ defaultMain app (AppContext [] (M. parsedFiles) 0 CompactMode config keyDispatcher)
    -- NOTE: useful code below to save file
    -- ParserSuccess (TaskFile name tasks) -> do
    -- let wrote = write (TaskFile name tasks)
    -- void $ writeFileExample "./resources/parsed.org" wrote
    -- ParserFailure e -> simpleMain (ui (show e))
    -- return ()

    readTasks :: Parser a -> FilePath -> IO (ParserResult a)
    readTasks p file = do
      content <- readFileExample file
      let (_, _, tasks) = runParser p content
      return tasks
