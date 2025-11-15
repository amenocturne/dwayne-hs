{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tui.CmdMode where

import Brick
import Brick.BChan (writeBChan)
import qualified Commands.Projects as CmdProjects
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Searcher.Searcher
import Tui.Types
import Writer.Writer

abortCmd :: AppContext a -> AppContext a
abortCmd = switchMode NormalMode . set (appState . cmdState) Nothing

removeLastIfExists :: T.Text -> T.Text
removeLastIfExists t
  | T.null t = t
  | otherwise = T.dropEnd 1 t

cmdDeleteChar :: AppContext a -> AppContext a
cmdDeleteChar ctx =
  case view (appState . cmdState) ctx of
    Just (Typing cmdType input)
      | not (T.null input) ->
          over (appState . cmdState) (\_ -> Just (Typing cmdType (removeLastIfExists input))) ctx
    _ ->
      (switchMode NormalMode . set (appState . cmdState) Nothing) ctx

executeCommand :: (Searcher a, Writer a, Show a) => GlobalAppState a
executeCommand = do
  ctx <- get
  case view (appState . cmdState) ctx of
    Just (Typing cmdType cmd) ->
      case cmdType of
        Command -> do
          case T.strip cmd of
            "w" -> do
              saveAll
              let filesSavedCount = M.size $ view fileStateLens ctx
              let msg = T.pack $ show filesSavedCount <> if filesSavedCount == 1 then " file written" else " files written"
              modify $ set (appState . cmdState) (Just $ ShowingMessage msg)
            "w!" -> do
              forceWriteAll
              let filesSavedCount = M.size $ view fileStateLens ctx
              let msg = T.pack $ show filesSavedCount <> if filesSavedCount == 1 then " file written (forced)" else " files written (forced)"
              modify $ set (appState . cmdState) (Just $ ShowingMessage msg)
            "q" -> quit
            "q!" -> forceQuit
            "wq" -> do
              forceWriteAll
              let filesSavedCount = M.size $ view fileStateLens ctx
              let msg = T.pack $ show filesSavedCount <> if filesSavedCount == 1 then " file written (forced)" else " files written (forced)"
              modify $ set (appState . cmdState) (Just $ ShowingMessage msg)
              quit
            unknown -> do
              let msg = "E492: Not an editor command: " <> unknown
              modify $ set (appState . cmdState) (Just $ ShowingMessage msg)
        Search -> CmdProjects.saveForJump $ do
          ctx <- get
          modify $ over viewFilterLens ((matches $ T.strip cmd) :)
          modify $ set cursorLens (Just 0)
          modify $ switchMode NormalMode . set (appState . cmdState) Nothing
    _ -> return ()

saveAll :: GlobalAppState a
saveAll = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) SaveAllFiles

forceWriteAll :: GlobalAppState a
forceWriteAll = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) ForceWriteAll

quit :: GlobalAppState a
quit = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) QuitApp

forceQuit :: GlobalAppState a
forceQuit = get >>= \ctx -> liftIO $ writeBChan (view (appState . eventChannel) ctx) ForceQuit
