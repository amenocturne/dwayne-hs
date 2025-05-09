{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui.Types where

import Brick
import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode

import Brick.BChan
import Brick.Widgets.Dialog (Dialog)
import Data.List.NonEmpty (NonEmpty)
import Data.Time (UTCTime)
import Parser.Parser
import Data.Set (Set)

data AppContext a = AppContext
  { _appState :: AppState a
  , _config :: AppConfig a
  }

data KeyEvent
  = -- Normal mode
    MoveUp
  | MoveDown
  | JumpEnd
  | Quit
  | EditInEditor
  | Undo
  | Redo
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
  , _keybindings :: [KeyBinding a]
  , _keyTimeoutMs :: Int
  }

data AppState a = AppState
  { _tasksState :: TasksState a
  , _eventChannel :: BChan AppEvent
  , _errorDialog :: Maybe ErrorDialog
  , _keyState :: KeyState
  , _undoState :: UndoState a
  }

data TasksState a = TasksState
  { _fileState :: FileState a
  , _currentView :: [TaskPointer]
  , _currentTask :: Maybe Int -- Index of a currently focused task in a view
  } deriving (Show)

data UndoState a = UndoState
  { _undoStack :: [TasksState a]
  , _redoStack :: [TasksState a]
  } deriving (Show)

data KeyState
  = NoInput
  | KeysPressed
      { _keyBuffer :: NonEmpty KeyPress
      , _lastKeyPressed :: UTCTime
      }
data ErrorDialog = ErrorDialog
  { _edDialog :: Dialog () Name
  , _edMessage :: String
  }

data TaskPointer = TaskPointer
  { _file :: FilePath
  , _taskIndex :: Int
  }
  deriving (Eq, Show)

data AppEvent = Error String deriving (Eq)

data DialogResult = DialogOK deriving (Eq)

type GlobalAppStateF a = EventM Name (AppContext a)

type GlobalAppState a = GlobalAppStateF a ()

data KeyBinding a = KeyBinding
  { _keyEvent :: KeyEvent
  , _keyBinding :: NonEmpty KeyPress
  , _keyDecription :: T.Text
  , _keyAction :: GlobalAppState a
  , _keyContext :: AppContext a -> Bool -- defines when this keybinding is valid
  }

data KeyPress = KeyPress {_key :: E.Key, _mods :: Set E.Modifier} deriving (Eq, Ord, Show)

--------------------------------- Optics ---------------------------------------

makeLenses ''TaskPointer
makeLenses ''AppState
makeLenses ''AppContext
makeLenses ''AppConfig
makeLenses ''ErrorDialog
makeLenses ''KeyBinding
makeLenses ''KeyPress
makeLenses ''TasksState
makeLenses ''UndoState

tasksStateLens :: Lens' (AppContext a) (TasksState a)
tasksStateLens = appState . tasksState

currentCursorLens :: Lens' (AppContext a) (Maybe Int)
currentCursorLens = tasksStateLens . currentTask

currentViewLens :: Lens' (AppContext a) [TaskPointer]
currentViewLens = tasksStateLens . currentView

fileStateLens :: Lens' (AppContext a) (FileState a)
fileStateLens = tasksStateLens . fileState

undoStackLens :: Lens' (AppContext a) [TasksState a]
undoStackLens = appState . undoState . undoStack

redoStackLens :: Lens' (AppContext a) [TasksState a]
redoStackLens = appState . undoState . redoStack

taskBy :: TaskPointer -> Traversal' (FileState a) a
taskBy ptr =
  ix (ptr ^. file)
    . success
    . content
    . ix (ptr ^. taskIndex)

currentTaskLens :: Traversal' (AppContext a) a
currentTaskLens f ctx =
  case (view currentCursorLens ctx, view currentViewLens ctx) of
    (Just i, cv)
      | i >= 0
      , i < length cv ->
          let ptr = cv !! i
           in (\modifiedFile -> set fileStateLens modifiedFile ctx)
                <$> traverseOf (taskBy ptr) f (view fileStateLens ctx)
    _ -> pure ctx

currentTaskPtr :: Traversal' (AppContext a) TaskPointer
currentTaskPtr f ctx =
  case (view currentCursorLens ctx, view currentViewLens ctx) of
    (Just i, cv)
      | i >= 0
      , i < length cv ->
          ctx & currentViewLens . ix i %%~ f
    _ -> pure ctx
