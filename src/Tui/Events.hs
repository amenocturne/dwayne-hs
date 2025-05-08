{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tui.Events where

import Brick
import Control.Lens
import Graphics.Vty.Input.Events
import Tui.Types

import Brick.Widgets.Dialog
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.List.NonEmpty (isPrefixOf, nonEmpty, toList)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Clock (UTCTime)

matchesSubsequence :: [KeyPress] -> KeyBinding a -> Bool
matchesSubsequence s = isPrefixOf s . view keyBinding

millisBetween :: UTCTime -> UTCTime -> Int
millisBetween t1 t2 = abs $ round $ diffUTCTime t1 t2 * 1000

cleanKeyState :: GlobalAppState a
cleanKeyState = modify $ set (appState . keyState) NoInput

handleEvent :: BrickEvent Name AppEvent -> GlobalAppState a
handleEvent (VtyEvent (EvKey key mods)) = do
  ctx <- get
  now <- liftIO getCurrentTime
  let buffer = case view (appState . keyState) ctx of
        NoInput -> [KeyPress key mods]
        KeysPressed b lastPressed ->
          if millisBetween lastPressed now > view (config . keyTimeoutMs) ctx
            then [KeyPress key mods]
            else toList b ++ [KeyPress key mods]
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
handleEvent (AppEvent event) = case event of
  Error msg -> do
    let dlg =
          ErrorDialog
            { _edDialog =
                dialog
                  (Just $ str "Error")
                  (Just (Viewport1, [("OK", Viewport1, ())]))
                  50
            , _edMessage = msg
            }
    modify $ set (appState . errorDialog) (Just dlg)
handleEvent _ = return ()
