{-# LANGUAGE OverloadedStrings #-}

module Commands.Capture (captureCommand) where

import Commands.CliHelpers (loadConfig)
import Commands.Command (Command (..))
import Commands.UrlEnrich (enrichText)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Model.OrgMode
  ( Task (..),
    orgDayTimeFormat,
    orgInboxKeyword,
    plainToRichText,
  )
import Options.Applicative (argument, help, long, metavar, str, switch)
import Parser.OrgParser (dateTimeParserReimplemented)
import Parser.Parser (ParserResult (..), runParser)
import Writer.OrgWriter ()
import Writer.Writer (write)
import Tui.Types (AppConfig (..))

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
  conf <- loadConfig
  let fp = _inboxFile conf
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
      taskText = write task

  existing <- TIO.readFile fp
  let separator = if T.null existing || "\n\n" `T.isSuffixOf` existing then "" else "\n\n"
  TIO.appendFile fp (separator <> taskText <> "\n")

  TIO.putStrLn $ "Captured: " <> titleLine
