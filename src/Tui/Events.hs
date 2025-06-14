{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tui.Events where

import Brick
import Control.Lens
import Graphics.Vty.Input.Events
import Tui.Types

import Brick.BChan (writeBChan)
import Brick.Widgets.Dialog
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Char (isUpper, toLower)
import Data.List (find, intercalate)
import Data.List.NonEmpty (isPrefixOf, nonEmpty, toList)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Clock (UTCTime)
import Parser.Parser
import TextUtils (readFileExample)
import Writer.OrgWriter ()
import Writer.Writer

checkFilesUnmodified :: (Eq a) => AppContext a -> IO [FilePath]
checkFilesUnmodified ctx = do
  let fps = view (config . files) ctx
      parser = view (config . fileParser) ctx
      originalMap = view originalFileStateLens ctx
  pairs <- liftIO $ forM fps $ \fp -> do
    txt <- readFileExample fp
    let (_, _, newRes) = runParser parser txt
    return (fp, newRes)
  let modified =
        [ fp
        | (fp, newRes) <- pairs
        , case M.lookup fp originalMap of
            Just originalRes -> originalRes /= newRes
            Nothing -> True
        ]
  return modified

checkUnsavedChanges :: (Eq a) => AppContext a -> Bool
checkUnsavedChanges ctx = 
  let currentState = view fileStateLens ctx
      originalState = view originalFileStateLens ctx
  in currentState /= originalState

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
      let keys = view (config . keybindings) ctx
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

handleEvent :: (Writer a, Show a, Eq a) => BrickEvent Name AppEvent -> GlobalAppState a
handleEvent (VtyEvent (EvKey key mods)) = do
  ctx <- get
  case view (appState . cmdState) ctx of
    Just (ShowingMessage _) ->
      modify $ over (appState . appMode) (const NormalMode) . set (appState . cmdState) Nothing
    _ ->
      case (view (appState . appMode) ctx, key) of
        (NormalMode, _) -> handleNormalModeInput key mods
        (CmdMode, KChar c) -> handleCmdInput c
        (CmdMode, _) -> handleNormalModeInput key mods
handleEvent (AppEvent event) = case event of
  Error msg -> do
    let dlg =
          ErrorDialog
            { _edDialog =
                dialog
                  (Just $ str "Error")
                  Nothing
                  50
            , _edMessage = msg
            }
    modify $ set (appState . errorDialog) (Just dlg)
  SaveAllFiles -> do
    ctx <- get
    modified <- liftIO $ checkFilesUnmodified ctx
    if not (null modified)
      then
        -- alert user and abort
        liftIO $
          writeBChan
            (view (appState . eventChannel) ctx)
            ( Error $
                "Aborting save, external edits detected in: "
                  ++ intercalate ", " modified
            )
      else when (view (config . autoSave) ctx) $ do
        let files = M.toList $ view fileStateLens ctx
            saveFiles = traverse (uncurry writeTaskFile) files
        liftIO $ void saveFiles
        -- Update original file state after successful save
        modify $ set originalFileStateLens (view fileStateLens ctx)
  ForceWriteAll -> do
    ctx <- get
    when (view (config . autoSave) ctx) $ do
      let files = M.toList $ view fileStateLens ctx
          saveFiles = traverse (uncurry writeTaskFile) files
      liftIO $ void saveFiles
      -- Update original file state after successful save
      modify $ set originalFileStateLens (view fileStateLens ctx)
  QuitApp -> do
    ctx <- get
    if checkUnsavedChanges ctx
      then
        -- alert user and abort quit
        liftIO $ writeBChan
          (view (appState . eventChannel) ctx)
          ( Error $
              "No write since last change (add ! to override)"
          )
      else halt
  ForceQuit -> halt
handleEvent _ = return ()

writeTaskFile :: (Writer a) => FilePath -> ParserResult a -> IO ()
writeTaskFile path file = case file of
  ParserSuccess a -> TIO.writeFile path (write a)
  ParserFailure _ -> return ()
