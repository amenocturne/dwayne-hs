{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tui.Events where

import Brick
import Control.Lens
import Graphics.Vty.Input.Events
import Tui.Types

import Brick.Widgets.Dialog
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Char (isUpper, toLower)
import Data.List (find)
import Data.List.NonEmpty (isPrefixOf, nonEmpty, toList)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V
import Parser.Parser
import Writer.OrgWriter ()
import Writer.Writer

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

handleSearchInput :: Char -> GlobalAppState a
handleSearchInput c = modify $ over (appState . searchState) (Just . maybe initSS appendC)
 where
  appendC = over searchInput (`T.snoc` c)
  initSS = SearchState (T.singleton c) V.empty

handleEvent :: (Writer a, Show a) => BrickEvent Name AppEvent -> GlobalAppState a
handleEvent (VtyEvent (EvKey key mods)) = do
  ctx <- get
  case (view (appState . appMode) ctx, key) of
    (NormalMode, _) -> handleNormalModeInput key mods
    (SearchMode, KChar c) -> handleSearchInput c
    (SearchMode, _) -> handleNormalModeInput key mods
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
    let files = M.toList $ view fileStateLens ctx
    let saveFiles = traverse (uncurry writeTaskFile) files
    when (view (config . autoSave) ctx) $ void $ liftIO saveFiles
handleEvent _ = return ()

writeTaskFile :: (Writer a) => FilePath -> ParserResult a -> IO ()
writeTaskFile path file = case file of
  ParserSuccess a -> TIO.writeFile path (write a)
  ParserFailure _ -> return ()
