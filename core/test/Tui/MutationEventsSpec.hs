{-# LANGUAGE OverloadedStrings #-}

module Tui.MutationEventsSpec (spec) where

import Core.Types (FileState)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import qualified Data.Vector as V
import Events.Types (Event (..))
import Model.OrgMode (RichText (..), Task (..), TaskFile (..), TextNode (..))
import Parser.Parser (ParserResult (..))
import Test.Hspec
import Tui.MutationEvents (computeMutationEvents)

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

-- | Build a single-file FileState with the given list of tasks.
mkFs :: FilePath -> [Task] -> FileState Task
mkFs fp tasks =
  M.singleton fp (ParserSuccess (TaskFile Nothing (V.fromList tasks)))

spec :: Spec
spec = do
  describe "computeMutationEvents" $ do
    it "no changes → no events" $ do
      let fs = mkFs "/inbox.org" [mkTask]
      computeMutationEvents baseTime fs fs `shouldBe` []

    it "single field change → one delta event with that field set" $ do
      let oldFs = mkFs "/inbox.org" [mkTask]
          newTask = mkTask {_todoKeyword = "DONE"}
          newFs = mkFs "/inbox.org" [newTask]
          events = computeMutationEvents baseTime oldFs newFs
      length events `shouldBe` 1
      let e = head events
      evFilePath e `shouldBe` "/inbox.org"
      evTaskIndex e `shouldBe` 0
      evOccurredAt e `shouldBe` baseTime
      evTodoKeyword e `shouldBe` Just "DONE"
      -- untouched field stays Nothing
      evTitle e `shouldBe` Nothing
      evLevel e `shouldBe` Nothing

    it "appended task → genesis event for the new index" $ do
      let oldFs = mkFs "/inbox.org" [mkTask]
          extra = mkTask {_title = RichText [PlainText "added"]}
          newFs = mkFs "/inbox.org" [mkTask, extra]
          events = computeMutationEvents baseTime oldFs newFs
      length events `shouldBe` 1
      let e = head events
      evTaskIndex e `shouldBe` 1
      -- genesis events populate every field
      evLevel e `shouldBe` Just 1
      evTodoKeyword e `shouldBe` Just "INBOX"
      evTitle e `shouldBe` Just (RichText [PlainText "added"])

    it "brand-new file → genesis event per task" $ do
      let oldFs = M.empty
          newFs = mkFs "/inbox.org" [mkTask, mkTask {_title = RichText [PlainText "second"]}]
          events = computeMutationEvents baseTime oldFs newFs
      length events `shouldBe` 2
      map evTaskIndex events `shouldBe` [0, 1]
      mapM_ (\e -> evLevel e `shouldBe` Just 1) events

    it "tag set changed on one task only → exactly one event with evTags set" $ do
      let t0 = mkTask
          t1 = mkTask {_title = RichText [PlainText "second"]}
          oldFs = mkFs "/inbox.org" [t0, t1]
          t1' = t1 {_tags = S.fromList ["music"]}
          newFs = mkFs "/inbox.org" [t0, t1']
          events = computeMutationEvents baseTime oldFs newFs
      length events `shouldBe` 1
      let e = head events
      evTaskIndex e `shouldBe` 1
      evTags e `shouldBe` Just ["music"]

    it "deleted (TRASH-marked) task → delta event for keyword" $ do
      let oldFs = mkFs "/inbox.org" [mkTask]
          newFs = mkFs "/inbox.org" [mkTask {_todoKeyword = "TRASH"}]
          events = computeMutationEvents baseTime oldFs newFs
      length events `shouldBe` 1
      evTodoKeyword (head events) `shouldBe` Just "TRASH"

    it "refile-shaped change (mark old TRASH, add new under project) → two events" $ do
      let inboxOld = mkFs "/inbox.org" [mkTask]
          projOld = mkFs "/projects.org" []
          oldFs = M.union inboxOld projOld

          inboxNew = mkFs "/inbox.org" [mkTask {_todoKeyword = "TRASH"}]
          projNew = mkFs "/projects.org" [mkTask]
          newFs = M.union inboxNew projNew

          events = computeMutationEvents baseTime oldFs newFs
      length events `shouldBe` 2
      -- one delta on inbox (TRASH), one genesis on projects
      let inboxEv = head [e | e <- events, evFilePath e == "/inbox.org"]
          projEv = head [e | e <- events, evFilePath e == "/projects.org"]
      evTodoKeyword inboxEv `shouldBe` Just "TRASH"
      evLevel projEv `shouldBe` Just 1 -- genesis populates everything

    it "ParserFailure entries are ignored (no events emitted for them)" $ do
      let oldFs = mkFs "/inbox.org" [mkTask]
          newFs =
            M.insert "/broken.org" (ParserFailure "boom") oldFs
      computeMutationEvents baseTime oldFs newFs `shouldBe` []
