{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Tui.Types where

import Brick
import Brick.BChan
import Brick.Widgets.Dialog (Dialog)
import Control.Lens
import Control.Monad.ST (runST)
import Core.Types (FileState, TaskPointer (..), file, taskIndex)
import Data.Aeson (Object, Options (..), defaultOptions)
import Data.Aeson.Types (genericParseJSON)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms as VA
import qualified Data.Vector.Algorithms.Intro as VA
import Data.Yaml.Aeson (FromJSON (..))
import GHC.Generics hiding (to)
import qualified Graphics.Vty.Input.Events as E
import Model.LinearHistory
import Model.OrgMode
import Parser.Parser

data AppContext a = AppContext
  { _appState :: AppState a,
    _config :: AppConfig a,
    _system :: SystemConfig a
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
  | AddTag T.Text
  | DeleteTag T.Text
  | Undo
  | Redo
  | CleanKeyState
  | SaveAll
  | AddTask
  | OpenUrl
  | SortCreatedAsc
  | SortCreatedDesc
  | SortPriorityAsc
  | SortPriorityDesc
  | UpPriority
  | DownPriority
  | GoToProject
  | Refile
  | MarkDone
  | EditTask
  | -- Selection mode
    EnterSelectionMode
  | ToggleRangeSelection
  | ToggleCurrentSelection
  | ExitSelectionMode
  | -- Macros
    Macro T.Text
  | -- Error dialog
    ErrorDialogQuit
  | ErrorDialogAccept
  | -- Validation dialog
    ValidationDialogAccept
  | ValidationDialogReject
  | -- Command/Search mode
    SwitchToSearchMode
  | SwitchToCmdMode
  | AbortCmd
  | CmdDeleteChar
  | ApplyCmd
  | RefreshView
  | -- Views
    View T.Text
  deriving (Eq, Show, Ord)

data Name = CompactViewWidget | CmdWidget deriving (Eq, Ord, Show)

data AppConfig a = AppConfig
  { _files :: [String],
    _inboxFile :: String,
    _projectsFile :: String,
    _scrollingMargin :: Int,
    _keyTimeoutMs :: Int,
    _colorScheme :: String,
    _commands :: Maybe Object -- Optional map of command aliases to Bool (default: all enabled)
  }
  deriving (Generic, Show)

instance FromJSON (AppConfig a) where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = drop 1 -- drops the leading underscore
        }

data SystemConfig a = SystemConfig
  { _fileParser :: Parser (TaskFile a),
    _taskParser :: Parser a,
    _keybindings :: [KeyBinding a],
    _defaultFilters :: [a -> Bool],
    _defaultSorter :: a -> a -> Ordering
  }

data AppState a = AppState
  { _eventChannel :: BChan AppEvent,
    _errorDialog :: Maybe ErrorDialog,
    _refileDialog :: Maybe RefileDialog,
    _validationDialog :: Maybe ValidationDialog,
    _keyState :: KeyState,
    _appMode :: AppMode a,
    _cmdState :: Maybe CmdState,
    _compactView :: LinearHistory (CompactView a),
    _fileState :: LinearHistory (FileState a),
    _originalFileState :: FileState a,
    _selection :: Set Int,
    _selectionAnchor :: Maybe Int
  }

data CmdType = Command | Search deriving (Eq, Show)

data CmdState
  = Typing {_cmdType :: CmdType, _cmdInput :: T.Text}
  | ShowingMessage T.Text
  deriving (Eq, Show)

data AppMode a = NormalMode | CmdMode | SelectionMode deriving (Eq)

data ViewSpec a = ViewSpec
  { _vsFilters :: [a -> Bool],
    _vsSorter :: a -> a -> Ordering,
    _vsVersion :: Int
  }

-- NOTE: currently we don't care about comparing functions (needed for history)
instance Eq (ViewSpec a) where
  (==) (ViewSpec _ _ v1) (ViewSpec _ _ v2) = v1 == v2

instance Show (ViewSpec a) where
  show _ = "ViewSpec"

data CompactView a = CompactView
  { _cursor :: Maybe Int, -- Index of a currently focused task in a view
    _viewportStart :: Int, -- The index of the first visible task
    _cachedView :: V.Vector TaskPointer,
    _viewSpec :: ViewSpec a
  }
  deriving (Eq, Show)

data KeyState
  = NoInput
  | KeysPressed {_keyBuffer :: NonEmpty KeyPress, _lastKeyPressed :: UTCTime}

data ErrorDialog = ErrorDialog
  { _edDialog :: Dialog () Name,
    _edMessage :: String
  }

instance Show ErrorDialog where
  show (ErrorDialog _ msg) = msg

data RefileDialog = RefileDialog
  { _rdProjects :: [TaskPointer],
    _rdSearchQuery :: T.Text,
    _rdSelectedIndex :: Int
  }
  deriving (Show)

data ValidationDialog = ValidationDialog
  { _vdDialog :: Dialog () Name, -- Simple dialog like ErrorDialog
    _vdMisplacedTasks :: [TaskPointer],
    _vdMessage :: String
  }

instance Show ValidationDialog where
  show (ValidationDialog _ _ msg) = msg

data AppEvent = Error String | SaveAllFiles | ForceWriteAll | QuitApp | ForceQuit | ValidationDialogCreated ValidationDialog

instance Eq AppEvent where
  Error s1 == Error s2 = s1 == s2
  SaveAllFiles == SaveAllFiles = True
  ForceWriteAll == ForceWriteAll = True
  QuitApp == QuitApp = True
  ForceQuit == ForceQuit = True
  ValidationDialogCreated _ == ValidationDialogCreated _ = True -- Just check constructor
  _ == _ = False

data DialogResult = DialogOK deriving (Eq)

type GlobalAppStateF a = EventM Name (AppContext a)

type GlobalAppState a = GlobalAppStateF a ()

data KeyBinding a = KeyBinding
  { _keyEvent :: KeyEvent,
    _keyBinding :: NonEmpty KeyPress,
    _keyDecription :: T.Text,
    _keyAction :: GlobalAppState a,
    _keyContext :: AppContext a -> Bool -- defines when this keybinding is valid
  }

data KeyPress = KeyPress {_key :: E.Key, _mods :: Set E.Modifier} deriving (Eq, Ord, Show)

--------------------------------- Optics ---------------------------------------

makeLenses ''AppState
makeLenses ''AppContext
makeLenses ''AppConfig
makeLenses ''SystemConfig
makeLenses ''ErrorDialog
makeLenses ''RefileDialog
makeLenses ''ValidationDialog
makeLenses ''KeyBinding
makeLenses ''KeyPress
makeLenses ''CompactView
makeLenses ''CmdState
makeLenses ''ViewSpec

selectionLens :: Lens' (AppContext a) (Set Int)
selectionLens = appState . selection

selectionAnchorLens :: Lens' (AppContext a) (Maybe Int)
selectionAnchorLens = appState . selectionAnchor

cursorLens :: Lens' (AppContext a) (Maybe Int)
cursorLens = appState . compactView . currentState . cursor

getAllPointers :: FileState a -> V.Vector TaskPointer
getAllPointers fs = V.concat $ M.foldlWithKey' accumulate [] fs
  where
    accumulate acc f result =
      case resultToMaybe result of
        Nothing -> acc
        Just taskFile ->
          let taskCount = V.length (_content taskFile)
              pointers = V.generate taskCount (\i -> TaskPointer f i)
           in pointers : acc

sortByVector :: (a -> a -> Ordering) -> V.Vector a -> V.Vector a
sortByVector cmp vec = runST $ do
  mvec <- V.thaw vec
  VA.sortBy cmp mvec
  V.freeze mvec

currentViewLens :: Lens' (AppContext a) (V.Vector TaskPointer)
currentViewLens = compactViewLens . cachedView

compactViewLens :: Lens' (AppContext a) (CompactView a)
compactViewLens = appState . compactView . currentState

computeCurrentView :: M.Map String (ParserResult (TaskFile a)) -> V.Vector TaskPointer -> ViewSpec a -> V.Vector TaskPointer
computeCurrentView fs allPtrs vs =
  let ptrsWithTasks = V.mapMaybe (\ptr -> fmap (ptr,) (fs ^? taskBy ptr)) allPtrs
      filtered = V.filter (\(p, t) -> all (\ff -> ff t) (view vsFilters vs)) ptrsWithTasks
      sorted = sortByVector (\(_, a1) (_, a2) -> view vsSorter vs a1 a2) filtered
   in V.map fst sorted

recomputeCurrentView :: AppContext a -> V.Vector TaskPointer
recomputeCurrentView ctx = computeCurrentView fs allPtrs vs
  where
    fs = view fileStateLens ctx
    allPtrs = getAllPointers fs
    vs = view (compactViewLens . viewSpec) ctx

cachingViewSpecLens ::
  (ViewSpec a -> b) ->
  (ViewSpec a -> b -> ViewSpec a) ->
  Lens' (AppContext a) b
cachingViewSpecLens getField setField =
  lens
    (\ctx -> getField (ctx ^. compactViewLens . viewSpec))
    ( \ctx newVal ->
        let oldVS = ctx ^. compactViewLens . viewSpec
            newVer = oldVS ^. vsVersion + 1
            newVS = setField oldVS newVal
            newVS' = newVS {_vsVersion = newVer}
            newCtx = ctx & compactViewLens . viewSpec .~ newVS'
            newCache = recomputeCurrentView newCtx
         in newCtx
              & compactViewLens . cachedView .~ newCache
    )

viewFilterLens :: Lens' (AppContext a) [a -> Bool]
viewFilterLens = cachingViewSpecLens _vsFilters (\vs f -> vs {_vsFilters = f})

viewSorterLens :: Lens' (AppContext a) (a -> a -> Ordering)
viewSorterLens = cachingViewSpecLens _vsSorter (\vs s -> vs {_vsSorter = s})

fileStateLens :: Lens' (AppContext a) (FileState a)
fileStateLens = appState . fileState . currentState

originalFileStateLens :: Lens' (AppContext a) (FileState a)
originalFileStateLens = appState . originalFileState

switchMode :: AppMode a -> AppContext a -> AppContext a
switchMode mode = over (appState . appMode) (const mode)

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
      | i >= 0,
        i < length cv ->
          let ptr = preview (ix i) cv
           in maybe
                (pure ctx)
                ( \p ->
                    (\modifiedFile -> set fileStateLens modifiedFile ctx)
                      <$> traverseOf (taskBy p) f (view fileStateLens ctx)
                )
                ptr
    _ -> pure ctx

currentTaskPtr :: Getter (AppContext a) (Maybe TaskPointer)
currentTaskPtr = to $ \ctx ->
  case view cursorLens ctx of
    Just i ->
      let cv = view currentViewLens ctx
       in cv V.!? i
    _ -> Nothing

-- | Traversal to the current TaskFile at a given FilePath
fileLens :: FilePath -> Traversal' (AppContext a) (TaskFile a)
fileLens fp = appState . fileState . currentState . ix fp . success
