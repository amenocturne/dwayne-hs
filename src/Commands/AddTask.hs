{-# LANGUAGE OverloadedStrings #-}

module Commands.AddTask (addTaskCommand, addNewTask) where

import Brick (get, modify, suspendAndResume)
import Brick.BChan (writeBChan)
import Commands.Command (Command (..), TuiBinding (..))
import Commands.ErrorDialog (showError)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Core.Operations as Ops
import Core.Types (FileState)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (getZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode
  ( Task (..),
    closed,
    content,
    createdProp,
    deadline,
    description,
    level,
    orgCreatedProperty,
    orgDayTimeFormat,
    orgInboxKeyword,
    plainToRichText,
    priority,
    properties,
    scheduled,
    tags,
    title,
    todoKeyword,
  )
import Parser.Parser (Location (..), ParserResult (..), runParser)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdout)
import TextUtils (editWithEditor)
import qualified Tui.Contexts as Ctx
import Tui.Helpers (refreshTuiView)
import qualified Tui.Helpers as Helpers
import Tui.Keybindings (toKeySeq)
import Tui.Types
  ( AppEvent (..),
    AppMode (..),
    GlobalAppState,
    KeyEvent (..),
    KeyPress (..),
    appState,
    config,
    cursorLens,
    eventChannel,
    fileLens,
    inboxFile,
    system,
    taskParser,
  )
import Writer.OrgWriter ()
import Writer.Writer (write)

-- | Add new task command
addTaskCommand :: Command Task
addTaskCommand =
  Command
    { cmdName = "Add Task",
      cmdAlias = "addTask",
      cmdDescription = "Add a new task to the inbox file",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = AddTask,
              tuiKeybinding = toKeySeq "at",
              tuiDescription = "Add new task",
              tuiAction = addNewTask,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Action: Add a new task by editing a template in an external editor
addNewTask :: GlobalAppState Task
addNewTask = Helpers.saveForUndo $ do
  ctx <- get
  let fp = view (config . inboxFile) ctx
  case preview (fileLens fp) ctx of
    Nothing -> showError $ "Inbox file not found: " <> T.pack fp <> "\n\nPlease check your configuration and ensure the inbox file exists."
    Just tf -> do
      now <- liftIO getZonedTime
      let createdStr = T.pack $ "[" ++ formatTime defaultTimeLocale orgDayTimeFormat now ++ "]"
          dummyTask =
            Task
              { _level = 1,
                _todoKeyword = orgInboxKeyword,
                _priority = Nothing,
                _title = plainToRichText "{{Title}}",
                _tags = S.empty,
                _scheduled = Nothing,
                _deadline = Nothing,
                _createdProp = Nothing,
                _closed = Nothing,
                _properties = [(orgCreatedProperty, createdStr)],
                _description = plainToRichText ""
              }
          initialContent = write dummyTask

      _ <- suspendAndResume $ do
        editResult <- editWithEditor initialContent
        case editResult of
          Left err -> do
            writeBChan (view (appState . eventChannel) ctx) $ Error err
            return ctx
          Right Nothing -> return ctx -- User cancelled
          Right (Just editedStr) ->
            let (l, _, result) =
                  runParser (view (system . taskParser) ctx) editedStr
             in case result of
                  ParserFailure err -> do
                    writeBChan (view (appState . eventChannel) ctx) $
                      Error (err ++ " at " ++ show (line l) ++ ":" ++ show (column l))
                    return ctx
                  ParserSuccess newTask
                    | newTask == dummyTask -> do
                        writeBChan (view (appState . eventChannel) ctx) $
                          Error "No changes detected; task not added."
                        return ctx
                    | otherwise -> do
                        let oldTasks = view content tf
                            idx = V.length oldTasks
                            updatedTf = tf & content .~ V.snoc oldTasks newTask
                            ctx1 = set (fileLens fp) updatedTf ctx
                            ctx2 = set cursorLens (Just idx) ctx1
                        return ctx2
      refreshTuiView
      return ()
