{-# LANGUAGE OverloadedStrings #-}

-- | Replay events into per-file org documents on disk.
--
-- Reads all events from the DB, projects them to current task state, and
-- writes each file's TaskFile out as org-mode text. Idempotent.
module Commands.ExportOrg
  ( exportOrgCommand,
  )
where

import Commands.CliHelpers (loadConfig)
import Commands.Command (Command (..))
import Control.Exception (IOException, try)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import DB.Connection (withDatabase)
import Events.Projection (fileStateFromEvents)
import Events.Store (selectAllEvents)
import Model.OrgMode (Task, TaskFile (..))
import Parser.Parser (ParserResult (..))
import System.IO (hPutStrLn, stderr)
import Tui.Types (AppConfig (..))
import Writer.OrgWriter ()
import Writer.Writer (Writer (..))

exportOrgCommand :: Command Task
exportOrgCommand =
  Command
    { cmdName = "Export Org",
      cmdAlias = "exportOrg",
      cmdDescription = "Replay events and write each file's tasks back to its org file",
      cmdTui = Nothing,
      cmdCli = Just $ pure $ do
        conf <- loadConfig
        let dbFile = _database conf
        events <- withDatabase dbFile $ \conn -> selectAllEvents conn
        let fs = fileStateFromEvents events
            fileCount = M.size fs
            taskCount = sum (map countTasks (M.elems fs))
        mapM_ writeOne (M.toList fs)
        putStrLn $
          "exportOrg: wrote "
            <> show taskCount
            <> " tasks to "
            <> show fileCount
            <> " files",
      cmdApi = Nothing
    }
  where
    countTasks (ParserSuccess (TaskFile _ tasks)) = V.length tasks
    countTasks (ParserFailure _) = 0

    writeOne :: (FilePath, ParserResult (TaskFile Task)) -> IO ()
    writeOne (fp, ParserSuccess tf) = do
      r <- try (TIO.writeFile fp (write tf)) :: IO (Either IOException ())
      case r of
        Right () -> pure ()
        Left e ->
          hPutStrLn stderr $
            "exportOrg: failed to write " ++ fp ++ ": " ++ show e
    writeOne (_, ParserFailure _) = pure ()
