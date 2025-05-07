{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui.Types where

import Brick
import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Model.OrgMode

import Brick.BChan
import Brick.Keybindings as K
import Brick.Widgets.Dialog (Dialog)
import Data.List
import Parser.Parser

data AppContext a = AppContext
  { _appState :: AppState a
  , _config :: AppConfig a
  , _keyEventDispatchers :: [KeyEventDispatcher a]
  }

data KeyEventDispatcher a = KeyEventDispatcher
  { _dispatcher :: K.KeyDispatcher KeyEvent (GlobalAppStateF a)
  , _dispatcherPrecondition :: AppContext a -> Bool
  }

data KeyEvent
  = -- Normal mode
    MoveUp
  | MoveDown
  | JumpEnd
  | Quit
  | EditInEditor
  | -- Error dialog
    ErrorDialogQuit
  | ErrorDialogAccept
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
  , _eventChannel :: BChan AppEvent
  , _errorDialog :: Maybe ErrorDialog
  }

data ErrorDialog = ErrorDialog
  { _edDialog :: Dialog () Name
  , _edMessage :: String
  }

data TaskPointer = TaskPointer
  { _file :: FilePath
  , _taskIndex :: Int
  }
  deriving (Eq)

data AppEvent = Error String deriving (Eq)

data DialogResult = DialogOK deriving (Eq)

type GlobalAppStateF a = EventM Name (AppContext a)

type GlobalAppState a = GlobalAppStateF a ()

data KeyBinding a = KeyBinding
  { _keyEvent :: KeyEvent
  , _keyBinding :: [K.Binding]
  , _keyDecription :: T.Text
  , _keyAction :: GlobalAppState a
  }

--------------------------------- Optics ---------------------------------------

makeLenses ''TaskPointer
makeLenses ''AppState
makeLenses ''AppContext
makeLenses ''AppConfig
makeLenses ''ErrorDialog
makeLenses ''KeyBinding
makeLenses ''KeyEventDispatcher

currentCursor :: Traversal' (AppContext a) (Maybe Int)
currentCursor = appState . currentTask

-- TODO: rewrite as traversable
modifyView :: ([TaskPointer] -> [TaskPointer]) -> AppState a -> AppState a
modifyView f s@(AppState _ cv ct _ _) = (set currentView newView . set currentTask selectedTask) s
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
currentTaskLens f state =
  case (state ^. currentTask, state ^. currentView) of
    (Just i, cv)
      | i >= 0
      , i < length cv ->
          let ptr = cv !! i
           in (\modifiedFile -> set fileState modifiedFile state)
                <$> traverseOf (taskBy ptr) f (view fileState state)
    _ -> pure state

currentTaskPtr :: Traversal' (AppState a) TaskPointer
currentTaskPtr f state =
  case (state ^. currentTask, state ^. currentView) of
    (Just i, cv)
      | i >= 0
      , i < length cv ->
          state & currentView . ix i %%~ f
    _ -> pure state
