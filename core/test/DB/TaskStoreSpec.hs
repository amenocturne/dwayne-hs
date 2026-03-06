{-# LANGUAGE OverloadedStrings #-}

module DB.TaskStoreSpec (spec) where

import DB.Connection (initDatabase)
import DB.TaskStore (DatabaseStore (..), OrgFileStore (..), TaskStore (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import qualified Data.Vector as V
import Model.OrgMode (OrgTime (..), RichText (..), Task (..), TaskFile (..), TextNode (..), richTextToPlain)
import Parser.OrgParser (orgFileParser)
import Parser.Parser (ParserResult (..))
import System.Directory (removeFile)
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
      _scheduled = Just (OrgTime (Left (fromGregorian 2026 3 6)) Nothing Nothing),
      _deadline = Just (OrgTime (Right (LocalTime (fromGregorian 2026 3 10) (TimeOfDay 14 30 0))) Nothing Nothing),
      _createdProp = Just (OrgTime (Left (fromGregorian 2026 3 1)) Nothing Nothing),
      _closed = Nothing,
      _properties = [("EFFORT", "30min"), ("CATEGORY", "personal")],
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

spec :: Spec
spec = do
  describe "DatabaseStore" $ do
    it "roundtrips a FileState through SQLite" $ do
      dbPath <- emptySystemTempFile "dwayne-taskstore-test.db"
      initDatabase dbPath
      let store = DatabaseStore dbPath
          inputFs =
            M.fromList
              [ ("/inbox.org", ParserSuccess (TaskFile (Just "inbox") (V.fromList [sampleTask, minimalTask]))),
                ("/projects.org", ParserSuccess (TaskFile (Just "projects") (V.fromList [minimalTask])))
              ]
      saveTasks store inputFs
      fs <- loadTasks store
      M.size fs `shouldBe` 2

      case M.lookup "/inbox.org" fs of
        Just (ParserSuccess (TaskFile _ tasks)) -> do
          V.length tasks `shouldBe` 2
          let t = V.head tasks
          _level t `shouldBe` 1
          _todoKeyword t `shouldBe` "TODO"
          _priority t `shouldBe` Just 1
          richTextToPlain (_title t) `shouldBe` "Buy groceries"
          _tags t `shouldBe` S.fromList ["errands", "shopping"]
        _ -> expectationFailure "Expected ParserSuccess for /inbox.org"

      case M.lookup "/projects.org" fs of
        Just (ParserSuccess (TaskFile _ tasks)) ->
          V.length tasks `shouldBe` 1
        _ -> expectationFailure "Expected ParserSuccess for /projects.org"

      removeFile dbPath

    it "save overwrites previous data" $ do
      dbPath <- emptySystemTempFile "dwayne-taskstore-test.db"
      initDatabase dbPath
      let store = DatabaseStore dbPath
          fs1 =
            M.fromList
              [("/inbox.org", ParserSuccess (TaskFile Nothing (V.fromList [sampleTask, minimalTask])))]
          fs2 =
            M.fromList
              [("/inbox.org", ParserSuccess (TaskFile Nothing (V.fromList [minimalTask])))]
      saveTasks store fs1
      saveTasks store fs2
      fs <- loadTasks store

      case M.lookup "/inbox.org" fs of
        Just (ParserSuccess (TaskFile _ tasks)) ->
          V.length tasks `shouldBe` 1
        _ -> expectationFailure "Expected ParserSuccess for /inbox.org"

      removeFile dbPath

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

        -- Load, then save with swapped content would be wrong,
        -- but saving back unchanged should preserve file identity.
        fs <- loadTasks store
        saveTasks store fs

        -- Reload and verify each file still has its own task
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

        -- Save only fileA with modified content
        let modifiedFs =
              M.fromList
                [(fileA, ParserSuccess (TaskFile Nothing (V.fromList [minimalTask])))]
        saveTasks store modifiedFs

        -- fileA should have the new task
        fs <- loadTasks store
        case M.lookup fileA fs of
          Just (ParserSuccess (TaskFile _ tasks)) -> do
            V.length tasks `shouldBe` 1
            richTextToPlain (_title (V.head tasks)) `shouldBe` "Simple task"
          _ -> expectationFailure "Expected ParserSuccess for fileA"

        -- fileB should be untouched
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

        -- Replace with a new task and save
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
