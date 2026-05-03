{-# LANGUAGE OverloadedStrings #-}

module Events.DiffSpec (spec) where

import qualified Data.Set as S
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Events.Diff (diffTaskAsEvent, diffTasks)
import Events.Types (Event (..))
import Model.OrgMode (RichText (..), Task (..), TextNode (..))
import Test.Hspec

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 0)

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
  describe "diffTaskAsEvent" $ do
    it "no changes → all delta fields Nothing" $ do
      let e = diffTaskAsEvent "/a.org" 0 baseTime mkTask mkTask
      evLevel e `shouldBe` Nothing
      evTodoKeyword e `shouldBe` Nothing
      evPriority e `shouldBe` Nothing
      evTitle e `shouldBe` Nothing
      evTags e `shouldBe` Nothing
      evScheduled e `shouldBe` Nothing
      evDeadline e `shouldBe` Nothing
      evCreated e `shouldBe` Nothing
      evClosed e `shouldBe` Nothing
      evProperties e `shouldBe` Nothing
      evDescription e `shouldBe` Nothing

    it "keyword changed → only evTodoKeyword Just-set" $ do
      let new = mkTask {_todoKeyword = "TODO"}
          e = diffTaskAsEvent "/a.org" 0 baseTime mkTask new
      evTodoKeyword e `shouldBe` Just "TODO"
      evLevel e `shouldBe` Nothing
      evTitle e `shouldBe` Nothing

    it "priority cleared → encodes the Nullable sentinel" $ do
      let withPri = mkTask {_priority = Just 1}
          cleared = mkTask {_priority = Nothing}
          e = diffTaskAsEvent "/a.org" 0 baseTime withPri cleared
      evPriority e `shouldBe` Just (-1)

    it "priority set → encodes the literal value" $ do
      let cleared = mkTask {_priority = Nothing}
          withPri = mkTask {_priority = Just 2}
          e = diffTaskAsEvent "/a.org" 0 baseTime cleared withPri
      evPriority e `shouldBe` Just 2

    it "row identity is preserved" $ do
      let e = diffTaskAsEvent "/some.org" 5 baseTime mkTask mkTask
      evFilePath e `shouldBe` "/some.org"
      evTaskIndex e `shouldBe` 5
      evOccurredAt e `shouldBe` baseTime

  describe "diffTasks (field-name list)" $ do
    it "no changes → empty" $
      diffTasks mkTask mkTask `shouldBe` []

    it "lists changed fields" $ do
      let new = mkTask {_todoKeyword = "DONE", _level = 2}
      diffTasks mkTask new `shouldMatchList` ["level", "todoKeyword"]
