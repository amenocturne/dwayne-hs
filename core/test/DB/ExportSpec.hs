{-# LANGUAGE OverloadedStrings #-}

module DB.ExportSpec (spec) where

import DB.Connection (withDatabase)
import DB.Export (loadTasksFromDB)
import DB.Import (importFileState)
import DB.TaskRow (deserializeOrgTime, deserializeProperties, deserializeTags)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import qualified Data.Vector as V
import Model.OrgMode (OrgTime (..), RichText (..), Task (..), TaskFile (..), TextNode (..), plainToRichText, richTextToPlain)
import Parser.Parser (ParserResult (..))
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
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
  describe "deserialization helpers" $ do
    it "deserializeTags parses JSON array" $ do
      deserializeTags "[\"errands\",\"shopping\"]" `shouldBe` S.fromList ["errands", "shopping"]

    it "deserializeTags with empty array returns empty set" $ do
      deserializeTags "[]" `shouldBe` S.empty

    it "deserializeTags with invalid JSON returns empty set" $ do
      deserializeTags "not json" `shouldBe` S.empty

    it "deserializeProperties parses JSON array of pairs" $ do
      deserializeProperties "[[\"key\",\"value\"]]" `shouldBe` [("key", "value")]

    it "deserializeProperties with empty array returns empty list" $ do
      deserializeProperties "[]" `shouldBe` []

    it "deserializeOrgTime parses date-only" $ do
      deserializeOrgTime "2026-03-06"
        `shouldBe` Just (OrgTime (Left (fromGregorian 2026 3 6)) Nothing Nothing)

    it "deserializeOrgTime parses datetime" $ do
      deserializeOrgTime "2026-03-06 14:30:00"
        `shouldBe` Just (OrgTime (Right (LocalTime (fromGregorian 2026 3 6) (TimeOfDay 14 30 0))) Nothing Nothing)

    it "deserializeOrgTime returns Nothing for invalid input" $ do
      deserializeOrgTime "not a date" `shouldBe` Nothing

  describe "loadTasksFromDB" $ do
    it "returns empty FileState for empty DB" $ do
      dbPath <- emptySystemTempFile "dwayne-export-test.db"
      withDatabase dbPath $ \conn -> do
        fs <- loadTasksFromDB conn
        M.size fs `shouldBe` 0
      removeFile dbPath

    it "roundtrips a single task" $ do
      dbPath <- emptySystemTempFile "dwayne-export-test.db"
      withDatabase dbPath $ \conn -> do
        let inputFs =
              M.fromList
                [("/inbox.org", ParserSuccess (TaskFile Nothing (V.fromList [sampleTask])))]
        _ <- importFileState conn inputFs
        fs <- loadTasksFromDB conn
        M.size fs `shouldBe` 1
        case M.lookup "/inbox.org" fs of
          Just (ParserSuccess (TaskFile _ tasks)) -> do
            V.length tasks `shouldBe` 1
            let t = V.head tasks
            _level t `shouldBe` 1
            _todoKeyword t `shouldBe` "TODO"
            _priority t `shouldBe` Just 1
            richTextToPlain (_title t) `shouldBe` "Buy groceries"
            _tags t `shouldBe` S.fromList ["errands", "shopping"]
            _scheduled t `shouldBe` Just (OrgTime (Left (fromGregorian 2026 3 6)) Nothing Nothing)
            _deadline t `shouldBe` Just (OrgTime (Right (LocalTime (fromGregorian 2026 3 10) (TimeOfDay 14 30 0))) Nothing Nothing)
            _createdProp t `shouldBe` Just (OrgTime (Left (fromGregorian 2026 3 1)) Nothing Nothing)
            _closed t `shouldBe` Nothing
            _properties t `shouldBe` [("EFFORT", "30min"), ("CATEGORY", "personal")]
            richTextToPlain (_description t) `shouldBe` "Get milk and eggs"
          _ -> expectationFailure "Expected ParserSuccess for /inbox.org"
      removeFile dbPath

    it "roundtrips a minimal task" $ do
      dbPath <- emptySystemTempFile "dwayne-export-test.db"
      withDatabase dbPath $ \conn -> do
        let inputFs =
              M.fromList
                [("/minimal.org", ParserSuccess (TaskFile Nothing (V.fromList [minimalTask])))]
        _ <- importFileState conn inputFs
        fs <- loadTasksFromDB conn
        case M.lookup "/minimal.org" fs of
          Just (ParserSuccess (TaskFile _ tasks)) -> do
            V.length tasks `shouldBe` 1
            let t = V.head tasks
            _level t `shouldBe` 2
            _todoKeyword t `shouldBe` "DONE"
            _priority t `shouldBe` Nothing
            richTextToPlain (_title t) `shouldBe` "Simple task"
            _tags t `shouldBe` S.empty
            _scheduled t `shouldBe` Nothing
            _deadline t `shouldBe` Nothing
            _createdProp t `shouldBe` Nothing
            _closed t `shouldBe` Nothing
            _properties t `shouldBe` []
            richTextToPlain (_description t) `shouldBe` ""
          _ -> expectationFailure "Expected ParserSuccess for /minimal.org"
      removeFile dbPath

    it "groups tasks by file_path" $ do
      dbPath <- emptySystemTempFile "dwayne-export-test.db"
      withDatabase dbPath $ \conn -> do
        let inputFs =
              M.fromList
                [ ("/inbox.org", ParserSuccess (TaskFile Nothing (V.fromList [sampleTask, minimalTask]))),
                  ("/projects.org", ParserSuccess (TaskFile Nothing (V.fromList [minimalTask])))
                ]
        _ <- importFileState conn inputFs
        fs <- loadTasksFromDB conn
        M.size fs `shouldBe` 2
        case M.lookup "/inbox.org" fs of
          Just (ParserSuccess (TaskFile _ tasks)) -> V.length tasks `shouldBe` 2
          _ -> expectationFailure "Expected 2 tasks for /inbox.org"
        case M.lookup "/projects.org" fs of
          Just (ParserSuccess (TaskFile _ tasks)) -> V.length tasks `shouldBe` 1
          _ -> expectationFailure "Expected 1 task for /projects.org"
      removeFile dbPath

    it "preserves task order within a file" $ do
      dbPath <- emptySystemTempFile "dwayne-export-test.db"
      withDatabase dbPath $ \conn -> do
        let inputFs =
              M.fromList
                [("/inbox.org", ParserSuccess (TaskFile Nothing (V.fromList [sampleTask, minimalTask])))]
        _ <- importFileState conn inputFs
        fs <- loadTasksFromDB conn
        case M.lookup "/inbox.org" fs of
          Just (ParserSuccess (TaskFile _ tasks)) -> do
            richTextToPlain (_title (tasks V.! 0)) `shouldBe` "Buy groceries"
            richTextToPlain (_title (tasks V.! 1)) `shouldBe` "Simple task"
          _ -> expectationFailure "Expected ParserSuccess for /inbox.org"
      removeFile dbPath
