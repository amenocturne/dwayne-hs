{-# LANGUAGE FlexibleInstances #-}
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
import Model.LinearHistory
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
  | JumpBackward
  | JumpForward
  | Quit
  | EditInEditor
  | ChangeTodoKeyword T.Text
  | Undo
  | Redo
  | CleanKeyState
  | SaveAll
  | AddTask
  | -- Error dialog
    ErrorDialogQuit
  | ErrorDialogAccept
  | -- Command/Search mode
    SwitchToSearchMode
  | SwitchToCmdMode
  | AbortCmd
  | CmdDeleteChar
  | ApplyCmd
  | -- Views
    View T.Text
  deriving (Eq, Show, Ord)

data Name = CompactViewWidget | CmdWidget deriving (Eq, Ord, Show)

type FileState a = M.Map String (ParserResult (TaskFile a))

data AppConfig a = AppConfig
  { _files :: [String]
  , _inboxFile :: String
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
  , _appMode :: AppMode a
  , _cmdState :: Maybe CmdState
  , _compactView :: LinearHistory CompactView
  , _fileState :: LinearHistory (FileState a)
  , _originalFileState :: FileState a
  }

data CmdType = Command | Search deriving (Eq, Show)

data CmdState
  = Typing {_cmdType :: CmdType, _cmdInput :: T.Text}
  | ShowingMessage T.Text
  deriving (Eq, Show)

data AppMode a = NormalMode | CmdMode deriving (Eq)

-- TODO: store a function that rebuilds this view and map it to 'r'
data CompactView = CompactView
  { _compactViewTaskStartIndex :: Int
  , _compactViewTasksEndIndex :: Int
  , _cursor :: Maybe Int -- Index of a currently focused task in a view
  , _currentView :: V.Vector TaskPointer
  }
  deriving (Show, Eq)

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

data AppEvent = Error String | SaveAllFiles | ForceWriteAll | QuitApp deriving (Eq)

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
makeLenses ''CompactView
makeLenses ''CmdState

cursorLens :: Lens' (AppContext a) (Maybe Int)
cursorLens = appState . compactView . currentState . cursor

currentViewLens :: Lens' (AppContext a) (V.Vector TaskPointer)
currentViewLens = appState . compactView . currentState . currentView

compactViewLens :: Lens' (AppContext a) CompactView
compactViewLens = appState . compactView . currentState

fileStateLens :: Lens' (AppContext a) (FileState a)
fileStateLens = appState . fileState . currentState

originalFileStateLens :: Lens' (AppContext a) (FileState a)
originalFileStateLens = appState . originalFileState

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

-- | Traversal to the current TaskFile at a given FilePath
fileLens :: FilePath -> Traversal' (AppContext a) (TaskFile a)
fileLens fp = appState . fileState . currentState . ix fp . success
