{-# LANGUAGE OverloadedStrings #-}

module Commands.Edit (editTaskCommand, editSelectedTaskInEditor) where

import Brick (get, modify, suspendAndResume)
import Brick.BChan (writeBChan)
import Commands.Command (Command (..), TuiBinding (..))
import Commands.ErrorDialog (showError)
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import Parser.Parser (Location (..), ParserResult (..), runParser)
import TextUtils (editWithEditor)
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import Tui.Keybindings
import Tui.Types
  ( AppEvent (..),
    AppMode (..),
    GlobalAppState,
    KeyEvent (..),
    KeyPress (..),
    appState,
    currentTaskLens,
    currentTaskPtr,
    eventChannel,
    fileStateLens,
    system,
    taskBy,
    taskParser,
  )
import Writer.OrgWriter ()
import Writer.Writer (write)

-- | Edit task in external editor command
editTaskCommand :: Command Task
editTaskCommand =
  Command
    { cmdName = "Edit Task",
      cmdAlias = "editTask",
      cmdDescription = "Edit the current task in an external editor",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = EditTask,
              tuiKeybinding = toKey E.KEnter,
              tuiDescription = "Edit task in editor",
              tuiAction = editSelectedTaskInEditor,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Action: Edit the current task in an external editor
editSelectedTaskInEditor :: GlobalAppState Task
editSelectedTaskInEditor = Helpers.saveForUndo $ do
  ctx <- get
  let ct = preview currentTaskLens ctx
  let maybePtr = view currentTaskPtr ctx
  case (ct, maybePtr) of
    (Just task, Just ptr) -> suspendAndResume $ do
      editResult <- editWithEditor (write task)
      case editResult of
        Left err -> do
          writeBChan (view (appState . eventChannel) ctx) $ Error err
          return ctx
        Right Nothing -> return ctx -- User cancelled
        Right (Just editedStr) -> do
          let (l, _, result) = runParser (view (system . taskParser) ctx) editedStr
          case result of
            ParserSuccess t -> return $ set (fileStateLens . taskBy ptr) t ctx
            ParserFailure e -> do
              _ <- writeBChan (view (appState . eventChannel) ctx) $ Error (e ++ " at " ++ show (line l) ++ ":" ++ show (column l))
              return ctx
    _ -> showError "No task selected. Please select a task to edit."
