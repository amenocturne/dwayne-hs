{-# LANGUAGE OverloadedStrings #-}

module Commands.Capture (captureCommand) where

import Commands.CliHelpers (loadFileState)
import Commands.Command (Command (..))
import Commands.UrlEnrich (enrichText)
import Control.Exception (IOException, try)
import Control.Lens (view, (&), (.~))
import DB.Connection (withDatabase)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime, getZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import Events.Store (insertEvent)
import Events.Types (genesisEvent)
import Model.OrgMode
  ( Task (..),
    TaskFile (..),
    content,
    orgDayTimeFormat,
    orgInboxKeyword,
    plainToRichText,
  )
import System.IO (hPutStrLn, stderr)
import Writer.OrgWriter ()
import Writer.Writer (Writer (..))
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

  -- Emit a genesis event — the canonical write to the events log. The
  -- @events_to_state@ trigger will project it into @task_current_state@
  -- automatically; downstream views read from there. The org file is
  -- mirrored as a best-effort secondary side effect so external tools
  -- (grep, editor) see the new task without a separate @dwayne dbExport@.
  utcNow <- getCurrentTime
  case M.lookup fp fState of
    Just (ParserSuccess tf) -> do
      let oldTasks = view content tf
          idx = V.length oldTasks
          updatedTf = tf & content .~ V.snoc oldTasks task
      withDatabase dbFile $ \conn ->
        insertEvent conn (genesisEvent fp idx utcNow task)
      mirrorOrgFile fp updatedTf
      TIO.putStrLn $ "Captured: " <> titleLine
    Just (ParserFailure e) ->
      TIO.putStrLn $ "Error: inbox file has parse errors: " <> T.pack e
    Nothing -> do
      let newTf = TaskFile Nothing (V.singleton task)
      withDatabase dbFile $ \conn ->
        insertEvent conn (genesisEvent fp 0 utcNow task)
      mirrorOrgFile fp newTf
      TIO.putStrLn $ "Captured: " <> titleLine
  where
    mirrorOrgFile :: FilePath -> TaskFile Task -> IO ()
    mirrorOrgFile path tf = do
      r <- try (TIO.writeFile path (write tf)) :: IO (Either IOException ())
      case r of
        Right () -> pure ()
        Left e ->
          hPutStrLn stderr $
            "warning: capture failed to mirror to org file " ++ path ++ ": " ++ show e
