{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Tui.Tui where

import Brick hiding (Location)
import Brick.BChan
import Brick.Widgets.Dialog (dialog)
import Control.Lens
import Control.Monad (guard, void, when)
import Data.Functor
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml (ParseException)
import Data.Yaml.Aeson (decodeFileEither)
import Graphics.Vty.Attributes (blue, brightBlue, brightCyan, brightGreen, brightMagenta, brightRed, brightWhite, brightYellow, cyan, green, magenta, red, rgbColor, white, yellow)
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Model.LinearHistory (initLinearHistory)
import Model.OrgMode (Task)
import Parser.Parser
import Refile.Refileable (Refileable)
import Render.Render
import Searcher.Searcher
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>))
import TextUtils
import Tui.ColorScheme
import Tui.Events (handleEvent)
import Tui.Render
import Tui.Types
import qualified Validation.SystemValidation as SV
import Writer.Writer

class Tui a where
  tui :: SystemConfig a -> IO ()

instance (Searcher a, Render a Name, Writer a, Show a, Eq a, Refileable a, SV.SystemValidator a) => Tui a where
  tui sysConf = do
    configFilePath <- getConfigPath
    parsedConfig <- decodeFileEither configFilePath :: IO (Either ParseException (AppConfig a))
    conf <- case parsedConfig of
      Left err ->
        die $
          unlines
            [ "ERROR: Failed to load configuration file",
              "Location: " ++ configFilePath,
              "",
              "Reason: " ++ show err,
              "",
              "Please check that:",
              "  - The configuration file exists",
              "  - The YAML syntax is valid",
              "  - All required fields are present"
            ]
      Right conf -> expandConfigPaths conf
    -- Auto-include inboxFile and projectsFile in files list and remove duplicates
    let allFiles = nub $ view files conf ++ [view inboxFile conf, view projectsFile conf]
    parsedFiles <- mapM (\f -> fmap (f,) (readTasks (view fileParser sysConf) f)) allFiles
    eventChan <- newBChan 10 -- maybe should use different event channel size
    let fState = M.fromList (fmap (\(a, (_, c)) -> (a, c)) parsedFiles)
    let parsingErrors = mapMaybe (\(f, (l, e)) -> fmap (f,l,) (errorToMaybe e)) parsedFiles
    let pointers = getAllPointers fState

    let viewSpec = ViewSpec {_vsFilters = view defaultFilters sysConf, _vsSorter = view defaultSorter sysConf, _vsVersion = 0}
    let state =
          AppState
            { _eventChannel = eventChan,
              _errorDialog = Nothing,
              _refileDialog = Nothing,
              _validationDialog = Nothing,
              _keyState = NoInput,
              _appMode = NormalMode,
              _cmdState = Nothing,
              _compactView =
                initLinearHistory
                  CompactView
                    { _cursor = 0 <$ listToMaybe (V.toList pointers),
                      _cachedView = computeCurrentView fState pointers viewSpec,
                      _viewSpec = viewSpec,
                      _viewportStart = 0
                    },
              _fileState = initLinearHistory fState,
              _originalFileState = fState,
              _selection = Set.empty,
              _selectionAnchor = Nothing
            }
    let ctx =
          AppContext
            { _appState = state,
              _config = conf,
              _system = sysConf
            }
    let app =
          App
            { appDraw = drawUI,
              appChooseCursor = showFirstCursor,
              appHandleEvent = handleEvent,
              appStartEvent = return (),
              appAttrMap = return $ theAppAttrMap $ getColorScheme (view colorScheme conf)
            }
    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    case parsingErrors of
      [] -> return ()
      errs ->
        writeBChan (view (appState . eventChannel) ctx) $
          Error $
            intercalate "\n" $
              fmap (\(f, l, e) -> "Error while parsing `" ++ f ++ "`: " ++ e ++ " at " ++ show (line l) ++ ":" ++ show (column l)) errs

    -- Check for system validation issues (only for Task instances)
    checkSystemValidation ctx eventChan

    void $ customMain initialVty buildVty (Just eventChan) app ctx
    where
      readTasks :: Parser a -> FilePath -> IO (Location, ParserResult a)
      readTasks p f = do
        c <- readFileExample f
        let (l, _, tasks) = runParser p c
        return (l, tasks)

-- Function to check system validation for any SystemValidator instance
checkSystemValidation :: (SV.SystemValidator a) => AppContext a -> BChan AppEvent -> IO ()
checkSystemValidation ctx eventChan = do
  let issues = SV.validateSystem ctx
  case issues of
    [] -> return () -- No issues found
    (issue : _) -> do
      -- Handle first issue (for now)
      let message = T.unpack (SV.issueDescription issue)
          dlg =
            ValidationDialog
              { _vdDialog = dialog (Just $ str "System Validation") Nothing 60,
                _vdMisplacedTasks = SV.affectedItems issue,
                _vdMessage = message ++ " (Enter to accept, Esc to cancel)"
              }
      writeBChan eventChan $ ValidationDialogCreated dlg
