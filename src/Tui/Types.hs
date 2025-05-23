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
import Data.Set (Set)
import Data.Time (UTCTime)
import qualified Data.Vector as V
import Parser.Parser

data AppContext a = AppContext
  { _appState :: AppState a
  , _config :: AppConfig a
  }

data KeyEvent
  = -- Normal mode
    MoveUp
  | MoveDown
  | JumpEnd
  | JumpBeginning
  | Quit
  | EditInEditor
  | ChangeTodoKeyword T.Text
  | Undo
  | Redo
  | CleanKeyState
  | -- Error dialog
    ErrorDialogQuit
  | ErrorDialogAccept
  | -- Searching
    SwitchToSearchMode
  | AbortSearch
  | SearchDeleteChar
  | ApplySearch
  deriving (Eq, Show, Ord)

data Name = CompactViewWidget deriving (Eq, Ord, Show)

type FileState a = M.Map String (ParserResult (TaskFile a))

data AppConfig a = AppConfig
  { _files :: [String]
  , _fileParser :: Parser (TaskFile a)
  , _taskParser :: Parser a
  , _scrollingMargin :: Int
  , _keybindings :: [KeyBinding a]
  , _keyTimeoutMs :: Int
  , _autoSave :: Bool
  }

data AppState a = AppState
  { _eventChannel :: BChan AppEvent
  , _errorDialog :: Maybe ErrorDialog
  , _keyState :: KeyState
  , _undoState :: UndoState a
  , _appMode :: AppMode a
  , _searchState :: Maybe (SearchState a)
  , _compactView :: CompactView
  , _fileState :: FileState a
  }

data SearchState a = SearchState
  { _searchInput :: T.Text
  , _searchResult :: V.Vector a
  }

data AppMode a = NormalMode | SearchMode deriving (Eq)

data CompactView = CompactView
  { _compactViewTaskStartIndex :: Int
  , _compactViewTasksEndIndex :: Int
  , _cursor :: Maybe Int -- Index of a currently focused task in a view
  , _currentView :: V.Vector TaskPointer
  }
  deriving (Show)

data UndoState a = UndoState
  { _undoStack :: [FileState a]
  , _redoStack :: [FileState a]
  }
  deriving (Show)

data KeyState
  = NoInput
  | KeysPressed {_keyBuffer :: NonEmpty KeyPress, _lastKeyPressed :: UTCTime}

data ErrorDialog = ErrorDialog
  { _edDialog :: Dialog () Name
  , _edMessage :: String
  }

instance Show ErrorDialog where
  show (ErrorDialog _ msg) = msg

data TaskPointer = TaskPointer
  { _file :: FilePath
  , _taskIndex :: Int
  }
  deriving (Eq, Show)

data AppEvent = Error String | SaveAllFiles deriving (Eq)

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
makeLenses ''UndoState
makeLenses ''CompactView
makeLenses ''SearchState

cursorLens :: Lens' (AppContext a) (Maybe Int)
cursorLens = appState . compactView . cursor

currentViewLens :: Lens' (AppContext a) (V.Vector TaskPointer)
currentViewLens = appState . compactView . currentView

compactViewLens :: Lens' (AppContext a) CompactView
compactViewLens = appState . compactView

fileStateLens :: Lens' (AppContext a) (FileState a)
fileStateLens = appState . fileState

undoStackLens :: Lens' (AppContext a) [FileState a]
undoStackLens = appState . undoState . undoStack

redoStackLens :: Lens' (AppContext a) [FileState a]
redoStackLens = appState . undoState . redoStack

taskBy :: TaskPointer -> Traversal' (FileState a) a
taskBy ptr =
  ix (view file ptr)
    . success
    . content
    . ix (view taskIndex ptr)

currentTaskLens :: Traversal' (AppContext a) a
currentTaskLens f ctx =
  case (view cursorLens ctx, view currentViewLens ctx) of
    (Just i, cv)
      | i >= 0
      , i < length cv ->
          let ptr = preview (ix i) cv
           in maybe
                (pure ctx)
                ( \p ->
                    (\modifiedFile -> set fileStateLens modifiedFile ctx)
                      <$> traverseOf (taskBy p) f (view fileStateLens ctx)
                )
                ptr
    _ -> pure ctx

currentTaskPtr :: Traversal' (AppContext a) TaskPointer
currentTaskPtr f ctx =
  case (view cursorLens ctx, view currentViewLens ctx) of
    (Just i, cv)
      | i >= 0
      , i < length cv ->
          ctx & currentViewLens . ix i %%~ f
    _ -> pure ctx
