{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the 'TaskStore' instances exposed by 'DB.TaskStore'.
--
-- After Phase 3 cleanup the events log is the canonical write model.
-- 'DatabaseStore.saveTasks' no longer writes to the dropped @tasks@
-- table; it just mirrors the FileState to org files. Loads still go
-- through 'loadTasksFromDB', which projects the events log into a
-- 'FileState'. The DB roundtrip therefore exercises the events log,
-- not a row-shaped table — see 'Repo.EventStoreRepoSpec' for the
-- full event-driven coverage.
module DB.TaskStoreSpec (spec) where

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import DB.Connection (initDatabase, withDatabase)
import DB.TaskStore (DatabaseStore (..), OrgFileStore (..), TaskStore (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Events.Store (insertEvents)
import Events.Types (genesisEvent)
import Model.OrgMode (RichText (..), Task (..), TaskFile (..), TextNode (..), richTextToPlain)
import Parser.OrgParser (orgFileParser)
import Parser.Parser (ParserResult (..))
import System.Directory (doesFileExist, removeFile)
import System.IO.Temp (emptySystemTempFile, withSystemTempDirectory)
import Test.Hspec

sampleTask :: Task
sampleTask =
  Task
    { _level = 1,
      _todoKeyword = "TODO",
      _priority = Just 1,
      _title = RichText [PlainText "Buy groceries"],
      _tags = S.fromList ["errands", "shopping"],
      _scheduled = Nothing,
      _deadline = Nothing,
      _createdProp = Nothing,
      _closed = Nothing,
      _properties = [],
      _description = RichText [PlainText "Get milk and eggs"]
    }

minimalTask :: Task
minimalTask =
  Task
    { _level = 2,
      _todoKeyword = "DONE",
      _priority = Nothing,
      _title = RichText [PlainText "Simple task"],
      _tags = S.empty,
      _scheduled = Nothing,
      _deadline = Nothing,
      _createdProp = Nothing,
      _closed = Nothing,
      _properties = [],
      _description = RichText []
    }

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 5 1) (secondsToDiffTime 0)

spec :: Spec
spec = do
  describe "DatabaseStore" $ do
    it "loadTasks projects events into a FileState" $ do
      dbPath <- emptySystemTempFile "dwayne-taskstore-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        _ <-
          insertEvents
            conn
            [ genesisEvent "/inbox.org" 0 t0 sampleTask,
              genesisEvent "/inbox.org" 1 t0 minimalTask,
              genesisEvent "/projects.org" 0 t0 minimalTask
            ]
        pure ()
      let store = DatabaseStore dbPath
      fs <- loadTasks store
      M.size fs `shouldBe` 2
      case M.lookup "/inbox.org" fs of
        Just (ParserSuccess (TaskFile _ tasks)) -> do
          V.length tasks `shouldBe` 2
          _todoKeyword (V.head tasks) `shouldBe` "TODO"
          richTextToPlain (_title (V.head tasks)) `shouldBe` "Buy groceries"
        _ -> expectationFailure "Expected ParserSuccess for /inbox.org"
      case M.lookup "/projects.org" fs of
        Just (ParserSuccess (TaskFile _ tasks)) ->
          V.length tasks `shouldBe` 1
        _ -> expectationFailure "Expected ParserSuccess for /projects.org"
      removeFile dbPath

    it "saveTasks mirrors the FileState to org files (DB writes go via events)" $ do
      withSystemTempDirectory "dwayne-taskstore-test" $ \tmpDir -> do
        let dbPath = tmpDir ++ "/dwayne.db"
            file1 = tmpDir ++ "/inbox.org"
        initDatabase dbPath
        let store = DatabaseStore dbPath
            fs =
              M.fromList
                [(file1, ParserSuccess (TaskFile Nothing (V.fromList [minimalTask])))]
        saveTasks store fs
        exists <- doesFileExist file1
        exists `shouldBe` True
        contents <- TIO.readFile file1
        T.isInfixOf "Simple task" contents `shouldBe` True

  describe "OrgFileStore" $ do
    it "loads tasks from org files" $ do
      withSystemTempDirectory "dwayne-orgstore-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/inbox.org"
            orgContent =
              T.unlines
                [ "* TODO Buy groceries :errands:",
                  "SCHEDULED: <2026-03-06 Fri>",
                  "",
                  "* DONE Simple task"
                ]
        TIO.writeFile file1 orgContent

        let store = OrgFileStore [file1] orgFileParser
        fs <- loadTasks store

        M.size fs `shouldBe` 1
        case M.lookup file1 fs of
          Just (ParserSuccess (TaskFile _ tasks)) -> do
            V.length tasks `shouldBe` 2
            _todoKeyword (V.head tasks) `shouldBe` "TODO"
            richTextToPlain (_title (V.head tasks)) `shouldBe` "Buy groceries"
            _todoKeyword (tasks V.! 1) `shouldBe` "DONE"
          _ -> expectationFailure "Expected ParserSuccess"

    it "writes tasks back to their correct files without mixing" $ do
      withSystemTempDirectory "dwayne-orgstore-test" $ \tmpDir -> do
        let fileA = tmpDir ++ "/inbox.org"
            fileB = tmpDir ++ "/projects.org"
            contentA = "* TODO Task A\n"
            contentB = "* DONE Task B\n"
        TIO.writeFile fileA contentA
        TIO.writeFile fileB contentB

        let store = OrgFileStore [fileA, fileB] orgFileParser

        fs <- loadTasks store
        saveTasks store fs

        fs' <- loadTasks store
        case M.lookup fileA fs' of
          Just (ParserSuccess (TaskFile _ tasks)) -> do
            V.length tasks `shouldBe` 1
            richTextToPlain (_title (V.head tasks)) `shouldBe` "Task A"
          _ -> expectationFailure "Expected ParserSuccess for fileA"

        case M.lookup fileB fs' of
          Just (ParserSuccess (TaskFile _ tasks)) -> do
            V.length tasks `shouldBe` 1
            richTextToPlain (_title (V.head tasks)) `shouldBe` "Task B"
          _ -> expectationFailure "Expected ParserSuccess for fileB"

    it "saves only passed entries, leaving other files untouched" $ do
      withSystemTempDirectory "dwayne-orgstore-test" $ \tmpDir -> do
        let fileA = tmpDir ++ "/inbox.org"
            fileB = tmpDir ++ "/projects.org"
            contentA = "* TODO Task A\n"
            contentB = "* DONE Task B\n"
        TIO.writeFile fileA contentA
        TIO.writeFile fileB contentB

        let store = OrgFileStore [fileA, fileB] orgFileParser

        let modifiedFs =
              M.fromList
                [(fileA, ParserSuccess (TaskFile Nothing (V.fromList [minimalTask])))]
        saveTasks store modifiedFs

        fs <- loadTasks store
        case M.lookup fileA fs of
          Just (ParserSuccess (TaskFile _ tasks)) -> do
            V.length tasks `shouldBe` 1
            richTextToPlain (_title (V.head tasks)) `shouldBe` "Simple task"
          _ -> expectationFailure "Expected ParserSuccess for fileA"

        case M.lookup fileB fs of
          Just (ParserSuccess (TaskFile _ tasks)) -> do
            V.length tasks `shouldBe` 1
            richTextToPlain (_title (V.head tasks)) `shouldBe` "Task B"
          _ -> expectationFailure "Expected ParserSuccess for fileB"

    it "saves and re-reads modified tasks" $ do
      withSystemTempDirectory "dwayne-orgstore-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/inbox.org"
            orgContent = "* TODO Original task\n"
        TIO.writeFile file1 orgContent

        let store = OrgFileStore [file1] orgFileParser

        fs <- loadTasks store
        case M.lookup file1 fs of
          Just (ParserSuccess (TaskFile _ tasks)) ->
            richTextToPlain (_title (V.head tasks)) `shouldBe` "Original task"
          _ -> expectationFailure "Expected ParserSuccess on first load"

        let newFs =
              M.fromList
                [(file1, ParserSuccess (TaskFile Nothing (V.fromList [minimalTask])))]
        saveTasks store newFs

        fs' <- loadTasks store
        case M.lookup file1 fs' of
          Just (ParserSuccess (TaskFile _ tasks)) -> do
            V.length tasks `shouldBe` 1
            _todoKeyword (V.head tasks) `shouldBe` "DONE"
            richTextToPlain (_title (V.head tasks)) `shouldBe` "Simple task"
          _ -> expectationFailure "Expected ParserSuccess after save"
