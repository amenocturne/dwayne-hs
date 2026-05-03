{-# LANGUAGE OverloadedStrings #-}

-- | One-shot migration: parse all configured org files, emit one genesis
-- 'Event' per task into the events table.
--
-- Idempotent: events use (file_path, task_index, occurred_at) as their
-- primary key, and we use INSERT OR IGNORE. Re-running this command on a
-- DB that already has events will silently skip duplicates.
--
-- The occurred_at choice prefers the task's :CREATED: org property when
-- present, falls back to the file's mtime, and finally to current time.
module Commands.MigrateToEvents
  ( migrateToEventsCommand,
  )
where

import Commands.CliHelpers (loadConfig)
import Commands.Command (Command (..))
import qualified Data.Vector as V
import Data.Time
  ( LocalTime (..),
    TimeOfDay (..),
    UTCTime,
    getCurrentTime,
    localTimeToUTC,
    utc,
  )
import DB.Connection (initDatabase, withDatabase)
import Events.Store (insertEvents)
import Events.Types (Event, genesisEvent)
import Model.OrgMode (OrgTime (..), Task (..), TaskFile (..))
import Parser.OrgParser (orgFileParser)
import Parser.Parser (ParserResult (..), runParser)
import System.Directory (doesFileExist, getModificationTime)
import TextUtils (readFileExample)
import Tui.Types (AppConfig (..), getAllFiles)

migrateToEventsCommand :: Command Task
migrateToEventsCommand =
  Command
    { cmdName = "Migrate To Events",
      cmdAlias = "migrateToEvents",
      cmdDescription = "Emit one genesis event per task, populating the events table from org files",
      cmdTui = Nothing,
      cmdCli = Just $ pure $ do
        conf <- loadConfig
        let dbFile = _database conf
            files = getAllFiles conf
        initDatabase dbFile
        now <- getCurrentTime
        events <- collectEvents now files
        n <- withDatabase dbFile $ \conn -> insertEvents conn events
        putStrLn $
          "migrateToEvents: emitted "
            <> show n
            <> " genesis events from "
            <> show (length files)
            <> " files",
      cmdApi = Nothing
    }

collectEvents :: UTCTime -> [FilePath] -> IO [Event]
collectEvents now files = do
  evss <- mapM (collectFileEvents now) files
  pure (concat evss)

collectFileEvents :: UTCTime -> FilePath -> IO [Event]
collectFileEvents now fp = do
  exists <- doesFileExist fp
  if not exists
    then pure []
    else do
      txt <- readFileExample fp
      mtime <- getModificationTime fp
      let (_, _, result) = runParser orgFileParser txt
      pure (eventsFromResult (Just mtime) now fp result)

eventsFromResult ::
  Maybe UTCTime ->
  UTCTime ->
  FilePath ->
  ParserResult (TaskFile Task) ->
  [Event]
eventsFromResult _ _ _ (ParserFailure _) = []
eventsFromResult mtime now fp (ParserSuccess (TaskFile _ tasks)) =
  [ genesisEvent fp idx (occurredAt mtime now task) task
  | (idx, task) <- zip [0 ..] (V.toList tasks)
  ]

-- | Pick a reasonable occurred_at for the genesis event. Order of preference:
--   1. The task's :CREATED: property if it carries a usable timestamp.
--   2. The org file's modification time.
--   3. The current wall clock as a last resort.
occurredAt :: Maybe UTCTime -> UTCTime -> Task -> UTCTime
occurredAt mtime fallback task =
  case _createdProp task of
    Just (OrgTime (Right lt) _ _) -> localTimeToUTC utc lt
    Just (OrgTime (Left day) _ _) ->
      localTimeToUTC utc (LocalTime day (TimeOfDay 0 0 0))
    _ -> case mtime of
      Just t -> t
      Nothing -> fallback
