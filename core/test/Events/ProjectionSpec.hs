{-# LANGUAGE OverloadedStrings #-}

module Events.ProjectionSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Events.Projection (currentState, foldTaskEvents)
import Events.Types (Event (..), emptyEvent, genesisEvent)
import Model.OrgMode (RichText (..), Task (..), TextNode (..))
import Test.Hspec

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 0)

laterTime :: UTCTime
laterTime = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 3600)

evenLaterTime :: UTCTime
evenLaterTime = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 7200)

initialTask :: Task
initialTask =
  Task
    { _level = 1,
      _todoKeyword = "INBOX",
      _priority = Nothing,
      _title = RichText [PlainText "buy milk"],
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
  describe "foldTaskEvents" $ do
    it "genesis only → reproduces the original Task" $ do
      let g = genesisEvent "/inbox.org" 0 baseTime initialTask
      foldTaskEvents [g] `shouldBe` Just initialTask

    it "no events → Nothing" $ do
      foldTaskEvents [] `shouldBe` Nothing

    it "delta only (no genesis) → Nothing because required fields are missing" $ do
      let delta = (emptyEvent "/inbox.org" 0 baseTime) {evTodoKeyword = Just "TODO"}
      foldTaskEvents [delta] `shouldBe` Nothing

    it "genesis + delta → keyword changed, other fields preserved" $ do
      let g = genesisEvent "/inbox.org" 0 baseTime initialTask
          delta = (emptyEvent "/inbox.org" 0 laterTime) {evTodoKeyword = Just "TODO"}
          Just t = foldTaskEvents [g, delta]
      _todoKeyword t `shouldBe` "TODO"
      _title t `shouldBe` _title initialTask
      _level t `shouldBe` _level initialTask

    it "two deltas → latest-Just-wins for the same field" $ do
      let g = genesisEvent "/inbox.org" 0 baseTime initialTask
          d1 = (emptyEvent "/inbox.org" 0 laterTime) {evTodoKeyword = Just "TODO"}
          d2 = (emptyEvent "/inbox.org" 0 evenLaterTime) {evTodoKeyword = Just "DONE"}
          Just t = foldTaskEvents [g, d1, d2]
      _todoKeyword t `shouldBe` "DONE"

    it "out-of-order events are sorted by occurredAt" $ do
      let g = genesisEvent "/inbox.org" 0 baseTime initialTask
          d1 = (emptyEvent "/inbox.org" 0 laterTime) {evTodoKeyword = Just "TODO"}
          d2 = (emptyEvent "/inbox.org" 0 evenLaterTime) {evTodoKeyword = Just "DONE"}
          Just t = foldTaskEvents [d2, g, d1]
      _todoKeyword t `shouldBe` "DONE"

    it "tags update is replacement, not merge" $ do
      let g0 = genesisEvent "/inbox.org" 0 baseTime (initialTask {_tags = S.fromList ["a"]})
          delta = (emptyEvent "/inbox.org" 0 laterTime) {evTags = Just ["b", "c"]}
          Just t = foldTaskEvents [g0, delta]
      _tags t `shouldBe` S.fromList ["b", "c"]

  describe "currentState" $ do
    it "groups by (file, taskIndex)" $ do
      let e0 = genesisEvent "/inbox.org" 0 baseTime initialTask
          e1 = genesisEvent "/inbox.org" 1 baseTime (initialTask {_title = RichText [PlainText "task 1"]})
          e2 = genesisEvent "/projects.org" 0 baseTime initialTask
          state = currentState [e0, e1, e2]
      M.size state `shouldBe` 3
      M.member ("/inbox.org", 0) state `shouldBe` True
      M.member ("/inbox.org", 1) state `shouldBe` True
      M.member ("/projects.org", 0) state `shouldBe` True
