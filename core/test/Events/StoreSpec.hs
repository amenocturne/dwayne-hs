{-# LANGUAGE OverloadedStrings #-}

module Events.StoreSpec (spec) where

import DB.Connection (initDatabase, withDatabase)
import qualified Data.Set as S
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Events.Store (insertEvent, insertEvents, nextTaskIndex, selectAllEvents, selectEventsForFiles, selectEventsSince)
import Events.Types (Event (..), emptyEvent, genesisEvent)
import Model.OrgMode (RichText (..), Task (..), TextNode (..))
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 0)

laterTime :: UTCTime
laterTime = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 3600)

mkTask :: Task
mkTask =
  Task
    { _level = 1,
      _todoKeyword = "INBOX",
      _priority = Nothing,
      _title = RichText [PlainText "test"],
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
  describe "insertEvent" $ do
    it "round-trips a single genesis event through SQLite" $ do
      dbPath <- emptySystemTempFile "dwayne-events-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        let e = genesisEvent "/inbox.org" 0 baseTime mkTask
        insertEvent conn e
        events <- selectAllEvents conn
        length events `shouldBe` 1
        evFilePath (head events) `shouldBe` "/inbox.org"
        evTaskIndex (head events) `shouldBe` 0
        evTodoKeyword (head events) `shouldBe` Just "INBOX"
      removeFile dbPath

    it "INSERT OR IGNORE silently skips duplicates on PK" $ do
      dbPath <- emptySystemTempFile "dwayne-events-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        let e = genesisEvent "/inbox.org" 0 baseTime mkTask
        insertEvent conn e
        insertEvent conn e
        insertEvent conn e
        events <- selectAllEvents conn
        length events `shouldBe` 1
      removeFile dbPath

    it "permits two events at different occurred_at for the same task" $ do
      dbPath <- emptySystemTempFile "dwayne-events-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        let g = genesisEvent "/inbox.org" 0 baseTime mkTask
            d = (emptyEvent "/inbox.org" 0 laterTime) {evTodoKeyword = Just "DONE"}
        insertEvent conn g
        insertEvent conn d
        events <- selectAllEvents conn
        length events `shouldBe` 2
      removeFile dbPath

  describe "insertEvents (bulk transactional)" $ do
    it "inserts a list and reports the count" $ do
      dbPath <- emptySystemTempFile "dwayne-events-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        let evs =
              [ genesisEvent "/a.org" 0 baseTime mkTask,
                genesisEvent "/a.org" 1 baseTime mkTask,
                genesisEvent "/b.org" 0 baseTime mkTask
              ]
        n <- insertEvents conn evs
        n `shouldBe` 3
        events <- selectAllEvents conn
        length events `shouldBe` 3
      removeFile dbPath

  describe "selectEventsSince" $ do
    it "returns only events strictly after the timestamp" $ do
      dbPath <- emptySystemTempFile "dwayne-events-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        let g = genesisEvent "/inbox.org" 0 baseTime mkTask
            d = (emptyEvent "/inbox.org" 0 laterTime) {evTodoKeyword = Just "DONE"}
        _ <- insertEvents conn [g, d]
        let halfHour =
              UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 1800)
        recent <- selectEventsSince conn halfHour
        length recent `shouldBe` 1
        evTodoKeyword (head recent) `shouldBe` Just "DONE"
      removeFile dbPath

  describe "selectEventsForFiles" $ do
    it "filters by ownership set" $ do
      dbPath <- emptySystemTempFile "dwayne-events-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        _ <-
          insertEvents
            conn
            [ genesisEvent "/a.org" 0 baseTime mkTask,
              genesisEvent "/b.org" 0 baseTime mkTask
            ]
        owned <- selectEventsForFiles conn (S.singleton "/a.org")
        length owned `shouldBe` 1
        evFilePath (head owned) `shouldBe` "/a.org"
      removeFile dbPath

  describe "nextTaskIndex" $ do
    it "returns 0 when no events exist for a file" $ do
      dbPath <- emptySystemTempFile "dwayne-events-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        n <- nextTaskIndex conn "/never-seen.org"
        n `shouldBe` 0
      removeFile dbPath

    it "returns max + 1 when events exist" $ do
      dbPath <- emptySystemTempFile "dwayne-events-test.db"
      initDatabase dbPath
      withDatabase dbPath $ \conn -> do
        _ <-
          insertEvents
            conn
            [ genesisEvent "/a.org" 0 baseTime mkTask,
              genesisEvent "/a.org" 3 baseTime mkTask
            ]
        n <- nextTaskIndex conn "/a.org"
        n `shouldBe` 4
      removeFile dbPath
