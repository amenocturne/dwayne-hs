{-# LANGUAGE OverloadedStrings #-}

module Commands.Capture (captureCommand) where

import Commands.CliHelpers (loadFileState)
import Commands.Command (Command (..))
import Commands.UrlEnrich (enrichText)
import Control.Lens (view, (&), (.~))
import DB.TaskStore (DatabaseStore (..), TaskStore (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import Model.OrgMode
  ( Task (..),
    TaskFile (..),
    content,
    orgDayTimeFormat,
    orgInboxKeyword,
    plainToRichText,
  )
import Options.Applicative (argument, help, long, metavar, str, switch)
import Parser.OrgParser (dateTimeParserReimplemented)
import Parser.Parser (ParserResult (..), runParser)
import Tui.Types (AppConfig (..))
import Writer.OrgWriter ()

captureCommand :: Command Task
captureCommand =
  Command
    { cmdName = "Capture",
      cmdAlias = "capture",
      cmdDescription = "Capture a new task to inbox with URL enrichment",
      cmdTui = Nothing,
      cmdCli =
        Just $
          runCapture
            <$> switch (long "no-enrich" <> help "Skip URL title fetching")
            <*> argument str (metavar "TEXT" <> help "Task text (first line = title, rest = body)"),
      cmdApi = Nothing
    }

runCapture :: Bool -> String -> IO ()
runCapture noEnrich input = do
  (conf, fState) <- loadFileState
  let fp = _inboxFile conf
      dbFile = _database conf
      inputText = T.pack input
      (titleLine, bodyText) = case T.breakOn "\n" inputText of
        (t, rest)
          | T.null rest -> (t, T.empty)
          | otherwise -> (t, T.drop 1 rest)

  now <- getZonedTime
  let createdStr = T.pack $ formatTime defaultTimeLocale orgDayTimeFormat now
      (_, _, createdResult) = runParser dateTimeParserReimplemented createdStr
      createdTime = case createdResult of
        ParserSuccess t -> Just t
        _ -> Nothing

  titleRich <-
    if noEnrich
      then return $ plainToRichText titleLine
      else enrichText titleLine
  descRich <-
    if noEnrich || T.null bodyText
      then return $ plainToRichText bodyText
      else enrichText bodyText

  let task =
        Task
          { _level = 1,
            _todoKeyword = orgInboxKeyword,
            _priority = Nothing,
            _title = titleRich,
            _tags = S.empty,
            _scheduled = Nothing,
            _deadline = Nothing,
            _createdProp = createdTime,
            _closed = Nothing,
            _properties = [],
            _description = descRich
          }

  case M.lookup fp fState of
    Just (ParserSuccess tf) -> do
      let updatedTf = tf & content .~ V.snoc (view content tf) task
      saveTasks (DatabaseStore dbFile) (M.singleton fp (ParserSuccess updatedTf))
      TIO.putStrLn $ "Captured: " <> titleLine
    Just (ParserFailure e) ->
      TIO.putStrLn $ "Error: inbox file has parse errors: " <> T.pack e
    Nothing -> do
      let newTf = TaskFile Nothing (V.singleton task)
      saveTasks (DatabaseStore dbFile) (M.singleton fp (ParserSuccess newTf))
      TIO.putStrLn $ "Captured: " <> titleLine
