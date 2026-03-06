{-# LANGUAGE OverloadedStrings #-}

module DB.TaskStore
  ( TaskStore (..),
    OrgFileStore (..),
    DatabaseStore (..),
    mkTaskStoreOps,
  )
where

import Core.Types (FileState, TaskStoreOps (..))
import DB.Connection (withDatabase)
import DB.Export (loadTasksFromDB)
import DB.Import (importFileState)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import Model.OrgMode (Task, TaskFile)
import Parser.Parser (Parser, ParserResult (..), runParser)
import TextUtils (readFileExample)
import Writer.OrgWriter ()
import Writer.Writer (Writer (..))

class TaskStore s where
  loadTasks :: s -> IO (FileState Task)
  saveTasks :: s -> FileState Task -> IO ()

-- | Org file backend: reads/writes org files directly
data OrgFileStore = OrgFileStore
  { orgFiles :: [FilePath],
    orgParser :: Parser (TaskFile Task)
  }

instance TaskStore OrgFileStore where
  loadTasks store = do
    pairs <- mapM readOne (orgFiles store)
    pure $ M.fromList pairs
    where
      readOne fp = do
        txt <- readFileExample fp
        let (_, _, result) = runParser (orgParser store) txt
        pure (fp, result)

  -- \| Writes every entry in the FileState to disk.
  -- The caller is responsible for passing only changed entries
  -- if incremental save is desired (see TUI SaveAllFiles handler).
  saveTasks store fs =
    mapM_ writeOne (M.toList fs)
    where
      writeOne (fp, ParserSuccess taskFile) =
        TIO.writeFile fp (write taskFile)
      writeOne (_, ParserFailure _) =
        pure ()

-- | SQLite backend: uses import/export functions
data DatabaseStore = DatabaseStore
  { dbPath :: FilePath
  }

instance TaskStore DatabaseStore where
  loadTasks store =
    withDatabase (dbPath store) $ \conn ->
      loadTasksFromDB conn

  saveTasks store fs =
    withDatabase (dbPath store) $ \conn -> do
      _ <- importFileState conn fs
      pure ()

-- | Create TaskStoreOps closures from any TaskStore instance
mkTaskStoreOps :: (TaskStore s) => s -> TaskStoreOps Task
mkTaskStoreOps s = TaskStoreOps {storeLoad = loadTasks s, storeSave = saveTasks s}
