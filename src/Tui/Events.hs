{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tui.Events where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.Dialog (dialog, dialogSelection)
import Control.Exception (IOException, catch)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Char (isUpper, toLower)
import Data.List (find, intercalate)
import Data.List.NonEmpty (isPrefixOf, nonEmpty, toList)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V
import Graphics.Vty.Input.Events
import Model.OrgMode (Task)
import Parser.Parser
import Refile.Refile (refileTaskToProject)
import Refile.Refileable (Refileable)
import Searcher.Searcher (Searcher, matches)
import TextUtils (readFileExample)
import Tui.Types
import Validation.ProjectValidation
import Writer.OrgWriter ()
import Writer.Writer

checkFilesUnmodified :: (Eq a) => AppContext a -> IO [FilePath]
checkFilesUnmodified ctx = do
  let fps = getAllFiles (view config ctx)
      parser = view (system . fileParser) ctx
      originalMap = view originalFileStateLens ctx
  pairs <- liftIO $ forM fps $ \fp -> do
    txt <- readFileExample fp
    let (_, _, newRes) = runParser parser txt
    return (fp, newRes)
  let modified =
        [ fp
        | (fp, newRes) <- pairs,
          case M.lookup fp originalMap of
            Just originalRes -> originalRes /= newRes
            Nothing -> True
        ]
  return modified

checkUnsavedChanges :: (Eq a) => AppContext a -> Bool
checkUnsavedChanges ctx =
  let currentState = view fileStateLens ctx
      originalState = view originalFileStateLens ctx
   in currentState /= originalState

performReload :: GlobalAppState a
performReload = do
  ctx <- get
  let fps = getAllFiles (view config ctx)
      parser = view (system . fileParser) ctx
  results <- liftIO $ forM fps $ \fp -> do
    txt <- readFileExample fp
    let (_, _, parsed) = runParser parser txt
    return (fp, parsed)
  let newFileState = M.fromList results
      filesReloaded = length fps
  modify $ \c ->
    c & set fileStateLens newFileState
      & set originalFileStateLens newFileState
      & set (appState . cmdState) (Just $ ShowingMessage $ T.pack $ show filesReloaded ++ " file(s) reloaded")
  modify $ switchMode NormalMode
  -- Refresh the view to reflect the reloaded data
  ctx' <- get
  let newCache = recomputeCurrentView ctx'
  modify $ set (compactViewLens . cachedView) newCache

matchesSubsequence :: [KeyPress] -> KeyBinding a -> Bool
matchesSubsequence s = isPrefixOf s . view keyBinding

millisBetween :: UTCTime -> UTCTime -> Int
millisBetween t1 t2 = abs $ round $ diffUTCTime t1 t2 * 1000

cleanKeyState :: GlobalAppState a
cleanKeyState = modify $ set (appState . keyState) NoInput

makeKeyPress :: Key -> [Modifier] -> KeyPress
makeKeyPress (KChar c) mods = KeyPress (KChar $ toLower c) (S.fromList withShift)
  where
    withShift = if isUpper c then MShift : mods else mods
makeKeyPress k mods = KeyPress k (S.fromList mods)

handleNormalModeInput :: Key -> [Modifier] -> GlobalAppState a
handleNormalModeInput key mods = do
  ctx <- get
  now <- liftIO getCurrentTime
  let currentKeyPress = makeKeyPress key mods
  let buffer = case view (appState . keyState) ctx of
        KeysPressed b lastPressed ->
          if millisBetween lastPressed now > view (config . keyTimeoutMs) ctx
            then [currentKeyPress]
            else toList b ++ [currentKeyPress]
        _ -> [currentKeyPress]
  case nonEmpty buffer of
    Nothing -> return ()
    Just l -> do
      let keys = view (system . keybindings) ctx
      let applicableKeys = filter (\k -> matchesSubsequence buffer k && view keyContext k ctx) keys
      case applicableKeys of
        [] -> cleanKeyState
        _ -> case find (\k -> view keyBinding k == l) applicableKeys of
          Nothing -> modify $ set (appState . keyState) (KeysPressed l now)
          Just x -> cleanKeyState >> view keyAction x

handleCmdInput :: Char -> GlobalAppState a
handleCmdInput c = modify $ over (appState . cmdState) (fmap appendC)
  where
    appendC (Typing cmdType input) = Typing cmdType (input `T.snoc` c)
    appendC other = other

handleRefileDialogInput :: (Searcher a, Refileable a) => Key -> [Modifier] -> GlobalAppState a
handleRefileDialogInput key mods = do
  case key of
    KEsc -> modify $ set (appState . refileDialog) Nothing
    KUp -> do
      modify $ over (appState . refileDialog . _Just . rdSelectedIndex) (\idx -> max 0 (idx - 1))
    KDown -> do
      ctx <- get
      case view (appState . refileDialog) ctx of
        Just dialog -> do
          let fs = view fileStateLens ctx
              allProjects = view rdProjects dialog
              searchQuery = view rdSearchQuery dialog
              filteredProjects =
                if T.null searchQuery
                  then allProjects
                  else
                    filter
                      ( \ptr -> case preview (taskBy ptr) fs of
                          Just task -> matches searchQuery task
                          Nothing -> False
                      )
                      allProjects
              currentIdx = view rdSelectedIndex dialog
              maxIdx = max 0 (length filteredProjects - 1)
              newIdx = min maxIdx (currentIdx + 1)
          modify $ set (appState . refileDialog . _Just . rdSelectedIndex) newIdx
        Nothing -> return ()
    KEnter -> do
      ctx <- get
      case (view (appState . refileDialog) ctx, view currentTaskPtr ctx) of
        (Just dialog, Just currentTask) -> do
          let fs = view fileStateLens ctx
              allProjects = view rdProjects dialog
              searchQuery = view rdSearchQuery dialog
              selectedIdx = view rdSelectedIndex dialog

              filteredProjects =
                if T.null searchQuery
                  then allProjects
                  else filter (\ptr -> maybe False (matches searchQuery) (preview (taskBy ptr) fs)) allProjects

          case listToMaybe (drop selectedIdx filteredProjects) of
            Just selectedProject -> do
              refileTaskToProject currentTask selectedProject
              modify $ set (appState . refileDialog) Nothing
            Nothing ->
              modify $ set (appState . refileDialog) Nothing
        _ -> modify $ set (appState . refileDialog) Nothing
    KBS -> do
      modify $
        over (appState . refileDialog . _Just) $
          \d ->
            set rdSearchQuery (let q = view rdSearchQuery d in if T.null q then T.empty else T.dropEnd 1 q) $
              set rdSelectedIndex 0 d
    KChar c -> do
      modify $
        over (appState . refileDialog . _Just) $
          \d ->
            set rdSearchQuery (view rdSearchQuery d `T.snoc` c) $
              set rdSelectedIndex 0 d
    _ -> return ()

handleValidationDialogInput :: Key -> [Modifier] -> GlobalAppState a
handleValidationDialogInput key mods = do
  case key of
    _ -> return ()

handleEvent :: (Writer a, Show a, Eq a, Searcher a, Refileable a) => BrickEvent Name AppEvent -> GlobalAppState a
handleEvent (VtyEvent (EvKey key mods)) = do
  ctx <- get
  case view (appState . cmdState) ctx of
    Just (ShowingMessage _) ->
      modify $ switchMode NormalMode . set (appState . cmdState) Nothing
    _ -> return ()
  ctx' <- get
  case view (appState . validationDialog) ctx' of
    Just _ -> do
      -- Handle validation dialog input, but also process key bindings
      handleValidationDialogInput key mods
      handleNormalModeInput key mods -- Let key bindings work too
    Nothing ->
      case view (appState . refileDialog) ctx' of
        Just _ -> handleRefileDialogInput key mods
        Nothing ->
          case (view (appState . appMode) ctx', key) of
            (NormalMode, _) -> handleNormalModeInput key mods
            (CmdMode, KChar c) -> handleCmdInput c
            (CmdMode, _) -> handleNormalModeInput key mods
            (SelectionMode, _) -> handleNormalModeInput key mods
handleEvent (AppEvent event) = case event of
  Error msg -> do
    let dlg =
          ErrorDialog
            { _edDialog =
                dialog
                  (Just $ str "Error")
                  Nothing
                  50,
              _edMessage = msg
            }
    modify $ set (appState . errorDialog) (Just dlg)
  ValidationDialogCreated dlg -> do
    modify $ set (appState . validationDialog) (Just dlg)
  SaveAllFiles -> do
    ctx <- get
    let currentState = view fileStateLens ctx
        originalState = view originalFileStateLens ctx
        changedFiles =
          [ (fp, file)
          | (fp, file) <- M.toList currentState,
            M.lookup fp originalState /= Just file
          ]
    if null changedFiles
      then modify $ set (appState . cmdState) (Just $ ShowingMessage "No changes to save")
      else do
        modified <- liftIO $ checkFilesUnmodified ctx
        let modifiedChanged = filter (`elem` map fst changedFiles) modified
        if not (null modifiedChanged)
          then do
            modify $ switchMode NormalMode . set (appState . cmdState) Nothing
            liftIO $
              writeBChan
                (view (appState . eventChannel) ctx)
                ( Error $
                    "Aborting save, external edits detected in: "
                      ++ intercalate ", " modifiedChanged
                )
          else do
            results <- liftIO $ traverse (uncurry writeTaskFile) changedFiles
            let errors = [err | Left err <- results]
            if null errors
              then do
                modify $ \c ->
                  let updated = foldl (\m (fp, f) -> M.insert fp f m) (view originalFileStateLens c) changedFiles
                   in set originalFileStateLens updated c
                modify $ set (appState . cmdState) (Just $ ShowingMessage $ T.pack $ "Saved " ++ show (length changedFiles) ++ " file(s)")
              else do
                liftIO $
                  writeBChan
                    (view (appState . eventChannel) ctx)
                    (Error $ "Failed to save some files:\n" ++ intercalate "\n" errors)
  ForceWriteAll -> do
    ctx <- get
    let currentState = view fileStateLens ctx
        originalState = view originalFileStateLens ctx
        changedFiles =
          [ (fp, file)
          | (fp, file) <- M.toList currentState,
            M.lookup fp originalState /= Just file
          ]
    if null changedFiles
      then modify $ set (appState . cmdState) (Just $ ShowingMessage "No changes to save")
      else do
        results <- liftIO $ traverse (uncurry writeTaskFile) changedFiles
        let errors = [err | Left err <- results]
        if null errors
          then do
            modify $ \c ->
              let updated = foldl (\m (fp, f) -> M.insert fp f m) (view originalFileStateLens c) changedFiles
               in set originalFileStateLens updated c
            modify $ set (appState . cmdState) (Just $ ShowingMessage $ T.pack $ "Saved " ++ show (length changedFiles) ++ " file(s)")
          else do
            liftIO $
              writeBChan
                (view (appState . eventChannel) ctx)
                (Error $ "Failed to save some files:\n" ++ intercalate "\n" errors)
  QuitApp -> do
    ctx <- get
    if checkUnsavedChanges ctx
      then do
        modify $ switchMode NormalMode . set (appState . cmdState) Nothing
        liftIO $
          writeBChan
            (view (appState . eventChannel) ctx)
            (Error "No write since last change (add ! to override)")
      else halt
  ForceQuit -> halt
  ReloadFiles -> do
    ctx <- get
    let currentState = view fileStateLens ctx
        originalState = view originalFileStateLens ctx
        hasUnsavedChanges = currentState /= originalState
    
    if hasUnsavedChanges
      then do
        modified <- liftIO $ checkFilesUnmodified ctx
        if not (null modified)
          then do
            modify $ switchMode NormalMode . set (appState . cmdState) Nothing
            liftIO $
              writeBChan
                (view (appState . eventChannel) ctx)
                ( Error $
                    "Cannot reload: unsaved changes and external edits detected in: "
                      ++ intercalate ", " modified
                      ++ "\nSave (:w) or force write (:w!) first, or use :e! to force reload"
                )
          else do
            modify $ switchMode NormalMode . set (appState . cmdState) Nothing
            liftIO $
              writeBChan
                (view (appState . eventChannel) ctx)
                (Error "No write since last change (save with :w first, or use :e! to discard changes)")
      else performReload
  ForceReloadFiles -> performReload
handleEvent _ = return ()

writeTaskFile :: (Writer a) => FilePath -> ParserResult a -> IO (Either String ())
writeTaskFile path file = case file of
  ParserSuccess a ->
    (TIO.writeFile path (write a) >> return (Right ())) `catch` handleIOError
    where
      handleIOError :: IOException -> IO (Either String ())
      handleIOError e =
        return $
          Left $
            unlines
              [ "Failed to write file: " ++ path,
                "Reason: " ++ show e
              ]
  ParserFailure _ -> return (Right ())
