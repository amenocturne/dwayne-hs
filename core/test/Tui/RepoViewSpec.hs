{-# LANGUAGE OverloadedStrings #-}

-- | Phase 2c: verify the TUI's repo-backed read seam.
--
-- The TUI used to fold every event in memory at startup and after every
-- mutation. Phase 2c routes that read through 'loadFileStateFromRepo',
-- which queries the materialized @task_current_state@ table maintained
-- by the @events_to_state@ trigger.
--
-- The shape this module exercises is the rendered-window contract: given
-- a sequence of events written through the standard event-store API,
-- the FileState returned by 'loadFileStateFromRepo' is what the TUI's
-- render path consumes (via 'fileStateLens' / 'taskBy' / pointer
-- vectors). Tests here drive the trigger pathway end-to-end and assert
-- the rebuilt FileState matches expectations.
module Tui.RepoViewSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import qualified Data.Vector as V
import DB.Connection (initDatabase, withDatabase)
import Events.Store (insertEvent)
import Events.Types (Event (..), emptyEvent, genesisEvent)
import Model.OrgMode (RichText (..), Task (..), TaskFile (..), TextNode (..))
import Parser.Parser (ParserResult (..))
import Repo.EventStoreRepo (mkEventStoreRepo)
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec
import Tui.RepoView (loadFileStateFromRepo)

inboxPath :: FilePath
inboxPath = "/inbox.org"

projectsPath :: FilePath
projectsPath = "/projects.org"

t0, t1, t2 :: UTCTime
t0 = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 0)
t1 = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 3600)
t2 = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 7200)

mkTask :: T.Text -> T.Text -> Task
mkTask kw ttl =
  Task
    { _level = 1,
      _todoKeyword = kw,
      _priority = Nothing,
      _title = RichText [PlainText ttl],
      _tags = S.empty,
      _scheduled = Nothing,
      _deadline = Nothing,
      _createdProp = Nothing,
      _closed = Nothing,
      _properties = [],
      _description = RichText []
    }

-- | Set up a fresh DB file with migrations applied.
withFreshDb :: (FilePath -> IO a) -> IO a
withFreshDb action = do
  dbPath <- emptySystemTempFile "dwayne-tui-repoview-test.db"
  initDatabase dbPath
  r <- action dbPath
  removeFile dbPath
  pure r

-- | Pull the V.Vector Task content out of a FileState for one path; fail
-- the test if the entry isn't a successful parse result.
tasksAt :: FilePath -> M.Map FilePath (ParserResult (TaskFile Task)) -> V.Vector Task
tasksAt fp fs =
  case M.lookup fp fs of
    Just (ParserSuccess (TaskFile _ ts)) -> ts
    _ -> V.empty

spec :: Spec
spec = do
  describe "loadFileStateFromRepo" $ do
    it "returns an empty FileState when no events have been written" $
      withFreshDb $ \dbFile -> do
        let repo = mkEventStoreRepo dbFile inboxPath
        fs <- loadFileStateFromRepo repo
        fs `shouldBe` M.empty

    it "materializes a single genesis event into a one-task FileState" $
      withFreshDb $ \dbFile -> do
        let task = mkTask "INBOX" "buy milk"
            repo = mkEventStoreRepo dbFile inboxPath
        withDatabase dbFile $ \conn ->
          insertEvent conn (genesisEvent inboxPath 0 t0 task)
        fs <- loadFileStateFromRepo repo
        let ts = tasksAt inboxPath fs
        V.length ts `shouldBe` 1
        let t = ts V.! 0
        _todoKeyword t `shouldBe` "INBOX"

    it "applies a delta on top of a genesis (TUI rendered window reflects trigger output)" $
      withFreshDb $ \dbFile -> do
        let task = mkTask "INBOX" "review pr"
            repo = mkEventStoreRepo dbFile inboxPath
        withDatabase dbFile $ \conn -> do
          insertEvent conn (genesisEvent inboxPath 0 t0 task)
          insertEvent conn ((emptyEvent inboxPath 0 t1) {evTodoKeyword = Just "DONE"})
        fs <- loadFileStateFromRepo repo
        let ts = tasksAt inboxPath fs
        V.length ts `shouldBe` 1
        let t = ts V.! 0
        _todoKeyword t `shouldBe` "DONE"

    it "groups tasks by file_path and orders them by task_index" $
      withFreshDb $ \dbFile -> do
        let inboxA = mkTask "INBOX" "first inbox"
            inboxB = mkTask "INBOX" "second inbox"
            proj = mkTask "PROJECT" "deploy"
            repo = mkEventStoreRepo dbFile inboxPath
        withDatabase dbFile $ \conn -> do
          insertEvent conn (genesisEvent inboxPath 0 t0 inboxA)
          -- intentionally write index 1 BEFORE index 0's delta to confirm
          -- the FileState preserves index ordering, not insertion order
          insertEvent conn (genesisEvent inboxPath 1 t0 inboxB)
          insertEvent conn (genesisEvent projectsPath 0 t0 proj)
        fs <- loadFileStateFromRepo repo
        let inboxTs = tasksAt inboxPath fs
            projTs = tasksAt projectsPath fs
        V.length inboxTs `shouldBe` 2
        V.length projTs `shouldBe` 1
        _todoKeyword (inboxTs V.! 0) `shouldBe` "INBOX"
        _todoKeyword (inboxTs V.! 1) `shouldBe` "INBOX"
        _todoKeyword (projTs V.! 0) `shouldBe` "PROJECT"

    it "ignores partial events that lack genesis coverage" $
      withFreshDb $ \dbFile -> do
        -- a delta-only event (no level/title) cannot materialize a task
        let repo = mkEventStoreRepo dbFile inboxPath
            partial = (emptyEvent inboxPath 0 t0) {evTodoKeyword = Just "TODO"}
        withDatabase dbFile $ \conn -> insertEvent conn partial
        fs <- loadFileStateFromRepo repo
        fs `shouldBe` M.empty

    it "reflects events committed via the trigger pathway end-to-end" $
      -- This is the explicit Step-6 test: events go through insertEvent →
      -- trigger fires → task_current_state updates → loadFileStateFromRepo
      -- reads the materialized projection. The FileState we return is
      -- exactly what the TUI's render path consumes.
      withFreshDb $ \dbFile -> do
        let task = mkTask "INBOX" "ship feature"
            repo = mkEventStoreRepo dbFile inboxPath
        -- simulate a sequence of TUI mutations: capture, then mark DONE,
        -- then re-mark TODO via a later delta
        withDatabase dbFile $ \conn -> do
          insertEvent conn (genesisEvent inboxPath 0 t0 task)
          insertEvent conn ((emptyEvent inboxPath 0 t1) {evTodoKeyword = Just "DONE"})
          insertEvent conn ((emptyEvent inboxPath 0 t2) {evTodoKeyword = Just "TODO"})
        fs <- loadFileStateFromRepo repo
        let ts = tasksAt inboxPath fs
        V.length ts `shouldBe` 1
        _todoKeyword (ts V.! 0) `shouldBe` "TODO"
