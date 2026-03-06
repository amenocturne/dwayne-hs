{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module DB.ImportSpec (spec) where

import DB.Connection (withDatabase)
import DB.Import (importFileState, importTask)
import DB.TaskRow (serializeOrgTime, serializeProperties, serializeTags)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import qualified Data.Vector as V
import Database.SQLite.Simple
import Database.SQLite.Simple.Types ((:.) (..))
import Model.OrgMode (OrgTime (..), RichText (..), Task (..), TaskFile (..), TextNode (..))
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
  describe "serialization helpers" $ do
    it "serializeTags produces JSON array" $ do
      serializeTags (S.fromList ["music", "cool"]) `shouldSatisfy` \t ->
        T.isInfixOf "music" t && T.isInfixOf "cool" t

    it "serializeTags with empty set produces []" $ do
      serializeTags S.empty `shouldBe` "[]"

    it "serializeProperties produces JSON array of pairs" $ do
      let result = serializeProperties [("key", "value")]
      result `shouldBe` "[[\"key\",\"value\"]]"

    it "serializeOrgTime formats date-only" $ do
      serializeOrgTime (OrgTime (Left (fromGregorian 2026 3 6)) Nothing Nothing)
        `shouldBe` "2026-03-06"

    it "serializeOrgTime formats datetime" $ do
      serializeOrgTime (OrgTime (Right (LocalTime (fromGregorian 2026 3 6) (TimeOfDay 14 30 0))) Nothing Nothing)
        `shouldBe` "2026-03-06 14:30:00"

  describe "importTask" $ do
    it "inserts a task into the database" $ do
      dbPath <- emptySystemTempFile "dwayne-import-test.db"
      withDatabase dbPath $ \conn -> do
        importTask conn "/test/inbox.org" 0 sampleTask
        rows <-
          query_ conn "SELECT file_path, task_index, level, todo_keyword, priority, title, tags, scheduled, deadline, created, closed, properties, description FROM tasks" ::
            IO [(T.Text, Int, Int, T.Text, Maybe Int, T.Text, T.Text) :. (Maybe T.Text, Maybe T.Text, Maybe T.Text, Maybe T.Text, T.Text, T.Text)]
        length rows `shouldBe` 1
        let (fp, idx, lvl, kw, pri, ttl, tgs) :. (sched, dl, crt, cls, props, desc) = head rows
        fp `shouldBe` "/test/inbox.org"
        idx `shouldBe` 0
        lvl `shouldBe` 1
        kw `shouldBe` "TODO"
        pri `shouldBe` Just 1
        ttl `shouldBe` "Buy groceries"
        tgs `shouldSatisfy` T.isInfixOf "errands"
        tgs `shouldSatisfy` T.isInfixOf "shopping"
        sched `shouldBe` Just "2026-03-06"
        dl `shouldBe` Just "2026-03-10 14:30:00"
        crt `shouldBe` Just "2026-03-01"
        cls `shouldBe` Nothing
        props `shouldSatisfy` T.isInfixOf "EFFORT"
        desc `shouldBe` "Get milk and eggs"
      removeFile dbPath

  describe "importFileState" $ do
    it "imports tasks from multiple files" $ do
      dbPath <- emptySystemTempFile "dwayne-import-test.db"
      withDatabase dbPath $ \conn -> do
        let fs =
              M.fromList
                [ ("/inbox.org", ParserSuccess (TaskFile (Just "inbox") (V.fromList [sampleTask, minimalTask]))),
                  ("/projects.org", ParserSuccess (TaskFile (Just "projects") (V.fromList [minimalTask])))
                ]
        count <- importFileState conn fs
        count `shouldBe` 3
        [Only dbCount] <- query_ conn "SELECT count(*) FROM tasks" :: IO [Only Int]
        dbCount `shouldBe` 3
      removeFile dbPath

    it "skips files with parser errors" $ do
      dbPath <- emptySystemTempFile "dwayne-import-test.db"
      withDatabase dbPath $ \conn -> do
        let fs =
              M.fromList
                [ ("/good.org", ParserSuccess (TaskFile Nothing (V.fromList [sampleTask]))),
                  ("/bad.org", ParserFailure "parse error")
                ]
        count <- importFileState conn fs
        count `shouldBe` 1
      removeFile dbPath

    it "clears old data on re-import" $ do
      dbPath <- emptySystemTempFile "dwayne-import-test.db"
      withDatabase dbPath $ \conn -> do
        let fs1 =
              M.fromList
                [("/inbox.org", ParserSuccess (TaskFile Nothing (V.fromList [sampleTask, minimalTask])))]
        _ <- importFileState conn fs1
        [Only count1] <- query_ conn "SELECT count(*) FROM tasks" :: IO [Only Int]
        count1 `shouldBe` 2

        let fs2 =
              M.fromList
                [("/inbox.org", ParserSuccess (TaskFile Nothing (V.fromList [minimalTask])))]
        _ <- importFileState conn fs2
        [Only count2] <- query_ conn "SELECT count(*) FROM tasks" :: IO [Only Int]
        count2 `shouldBe` 1
      removeFile dbPath
