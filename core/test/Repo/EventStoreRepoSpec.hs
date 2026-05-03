{-# LANGUAGE OverloadedStrings #-}

module Repo.EventStoreRepoSpec (spec) where

import Control.Monad (forM_)
import DB.Connection (initDatabase, withDatabase)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.SQLite.Simple (Only (..), execute_, query_)
import Events.Projection (foldTaskEvents)
import Events.Store (insertEvent, insertEvents)
import Events.Types (Event (..), emptyEvent, genesisEvent)
import Model.OrgMode (RichText (..), Task (..), TextNode (..))
import Repo.EventStoreRepo
  ( EventStoreRepo (..),
    mkEventStoreRepo,
    rebuildTaskCurrentState,
  )
import Repo.TaskRepo
  ( Query (..),
    SortField (..),
    TaskRepo (..),
    View (..),
    emptyQuery,
  )
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

t0, t1, t2, t3 :: UTCTime
t0 = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 0)
t1 = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 3600)
t2 = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 7200)
t3 = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 10800)

inboxPath :: FilePath
inboxPath = "/inbox.org"

projectsPath :: FilePath
projectsPath = "/projects.org"

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

-- | Set up a fresh DB file with migrations applied. Returns (path, repo).
withFreshRepo :: ((FilePath, EventStoreRepo) -> IO a) -> IO a
withFreshRepo action = do
  dbPath <- emptySystemTempFile "dwayne-repo-test.db"
  initDatabase dbPath
  let repo = mkEventStoreRepo dbPath inboxPath
  r <- action (dbPath, repo)
  removeFile dbPath
  pure r

-- | Project the events for a task using the in-memory algorithm so we
-- have a ground-truth comparison for the trigger.
projectFromEvents :: FilePath -> Int -> [Event] -> Maybe Task
projectFromEvents fp idx evs =
  foldTaskEvents [e | e <- evs, evFilePath e == fp, evTaskIndex e == idx]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "getTask" $ do
    it "returns Nothing for a task that has no events" $
      withFreshRepo $ \(_, repo) -> do
        result <- getTask repo (inboxPath, 0)
        result `shouldBe` Nothing

    it "returns Nothing when only a partial delta exists (no genesis coverage)" $
      withFreshRepo $ \(dbFile, repo) -> do
        -- A bare delta-style event that only sets keyword: no level, no title.
        let partial = (emptyEvent inboxPath 0 t0) {evTodoKeyword = Just "TODO"}
        withDatabase dbFile $ \conn -> insertEvent conn partial
        result <- getTask repo (inboxPath, 0)
        result `shouldBe` Nothing

    it "returns the projected task after a genesis event" $
      withFreshRepo $ \(dbFile, repo) -> do
        let task = mkTask "INBOX" "buy milk"
            gen = genesisEvent inboxPath 0 t0 task
        withDatabase dbFile $ \conn -> insertEvent conn gen
        result <- getTask repo (inboxPath, 0)
        result `shouldBe` Just task

    it "applies a delta on top of the genesis event" $
      withFreshRepo $ \(dbFile, repo) -> do
        let task = mkTask "INBOX" "buy milk"
            gen = genesisEvent inboxPath 0 t0 task
            delta = (emptyEvent inboxPath 0 t1) {evTodoKeyword = Just "DONE"}
        withDatabase dbFile $ \conn -> do
          insertEvent conn gen
          insertEvent conn delta
        result <- getTask repo (inboxPath, 0)
        case result of
          Just t -> _todoKeyword t `shouldBe` "DONE"
          Nothing -> expectationFailure "expected Just but got Nothing"

  describe "trigger correctness vs Events.Projection" $ do
    it "matches the projection for a simple genesis + delta sequence" $
      withFreshRepo $ \(dbFile, repo) -> do
        let task = mkTask "INBOX" "task"
            gen = genesisEvent inboxPath 0 t0 task
            delta = (emptyEvent inboxPath 0 t1) {evTodoKeyword = Just "TODO"}
            evs = [gen, delta]
        withDatabase dbFile $ \conn -> do
          insertEvent conn gen
          insertEvent conn delta
        result <- getTask repo (inboxPath, 0)
        result `shouldBe` projectFromEvents inboxPath 0 evs

    it "matches the projection across many deltas" $
      withFreshRepo $ \(dbFile, repo) -> do
        let task = mkTask "INBOX" "task"
            gen = genesisEvent inboxPath 0 t0 task
            d1 = (emptyEvent inboxPath 0 t1) {evTodoKeyword = Just "TODO"}
            d2 = (emptyEvent inboxPath 0 t2) {evPriority = Just 1}
            d3 = (emptyEvent inboxPath 0 t3) {evTodoKeyword = Just "DONE"}
            evs = [gen, d1, d2, d3]
        withDatabase dbFile $ \conn ->
          forM_ evs (insertEvent conn)
        result <- getTask repo (inboxPath, 0)
        result `shouldBe` projectFromEvents inboxPath 0 evs

  describe "out-of-order events" $ do
    it "later occurred_at wins even when inserted first" $
      withFreshRepo $ \(dbFile, repo) -> do
        let task = mkTask "INBOX" "task"
            gen = genesisEvent inboxPath 0 t0 task
            -- This delta is "newer" than the one below in occurred_at order
            -- but we insert it first; the trigger must still resolve the
            -- final state by occurred_at, not insertion order.
            laterDelta = (emptyEvent inboxPath 0 t2) {evTodoKeyword = Just "DONE"}
            earlierDelta = (emptyEvent inboxPath 0 t1) {evTodoKeyword = Just "TODO"}
        withDatabase dbFile $ \conn -> do
          insertEvent conn gen
          -- intentionally insert later before earlier
          insertEvent conn laterDelta
          insertEvent conn earlierDelta
        result <- getTask repo (inboxPath, 0)
        case result of
          Just t -> _todoKeyword t `shouldBe` "DONE"
          Nothing -> expectationFailure "expected Just but got Nothing"

    it "matches projection when events are inserted in reverse order" $
      withFreshRepo $ \(dbFile, repo) -> do
        let task = mkTask "INBOX" "task"
            gen = genesisEvent inboxPath 0 t0 task
            d1 = (emptyEvent inboxPath 0 t1) {evTodoKeyword = Just "TODO"}
            d2 = (emptyEvent inboxPath 0 t2) {evPriority = Just 2}
            d3 = (emptyEvent inboxPath 0 t3) {evTodoKeyword = Just "DONE"}
            evs = [gen, d1, d2, d3]
            reversed = reverse evs
        withDatabase dbFile $ \conn ->
          forM_ reversed (insertEvent conn)
        result <- getTask repo (inboxPath, 0)
        result `shouldBe` projectFromEvents inboxPath 0 evs

  describe "queryTasks views" $ do
    it "returns all non-TRASH tasks for ViewAll" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath 0 t0 (mkTask "INBOX" "a"),
                genesisEvent inboxPath 1 t0 (mkTask "TODO" "b"),
                genesisEvent inboxPath 2 t0 (mkTask "TRASH" "c")
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        results <- queryTasks repo (emptyQuery {qView = Just ViewAll})
        length results `shouldBe` 2

    it "filters by ViewInbox using the inbox file path" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath 0 t0 (mkTask "INBOX" "a"),
                genesisEvent inboxPath 1 t0 (mkTask "TODO" "b"),
                genesisEvent projectsPath 0 t0 (mkTask "PROJECT" "c")
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        results <- queryTasks repo (emptyQuery {qView = Just ViewInbox})
        length results `shouldBe` 2 -- both tasks under inbox file (excluding none, no TRASH)

    it "filters by ViewToday on todo_keyword" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath 0 t0 (mkTask "TODAY" "a"),
                genesisEvent inboxPath 1 t0 (mkTask "TODO" "b"),
                genesisEvent inboxPath 2 t0 (mkTask "TODAY" "c")
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        results <- queryTasks repo (emptyQuery {qView = Just ViewToday})
        length results `shouldBe` 2

    it "ViewWorkQueue returns TODAY and SOON" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath 0 t0 (mkTask "TODAY" "a"),
                genesisEvent inboxPath 1 t0 (mkTask "SOON" "b"),
                genesisEvent inboxPath 2 t0 (mkTask "TODO" "c"),
                genesisEvent inboxPath 3 t0 (mkTask "DONE" "d")
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        results <- queryTasks repo (emptyQuery {qView = Just ViewWorkQueue})
        length results `shouldBe` 2

    it "default queryTasks excludes TRASH" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath 0 t0 (mkTask "INBOX" "a"),
                genesisEvent inboxPath 1 t0 (mkTask "TRASH" "b"),
                genesisEvent inboxPath 2 t0 (mkTask "TRASH" "c")
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        results <- queryTasks repo emptyQuery
        length results `shouldBe` 1

    it "ViewTrash returns only TRASH" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath 0 t0 (mkTask "INBOX" "a"),
                genesisEvent inboxPath 1 t0 (mkTask "TRASH" "b"),
                genesisEvent inboxPath 2 t0 (mkTask "TRASH" "c")
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        results <- queryTasks repo (emptyQuery {qView = Just ViewTrash})
        length results `shouldBe` 2

  describe "queryTasks pagination" $ do
    it "honors qLimit" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath i t0 (mkTask "TODO" (T.pack ("t" ++ show i)))
              | i <- [0 .. 9]
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        results <- queryTasks repo (emptyQuery {qLimit = Just 3})
        length results `shouldBe` 3

    it "honors qOffset" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath i t0 (mkTask "TODO" (T.pack ("t" ++ show i)))
              | i <- [0 .. 4]
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        results <- queryTasks repo (emptyQuery {qLimit = Just 10, qOffset = Just 2})
        length results `shouldBe` 3

  describe "queryTasks search" $ do
    it "substring-matches title via qSearchTerm" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath 0 t0 (mkTask "TODO" "buy milk"),
                genesisEvent inboxPath 1 t0 (mkTask "TODO" "feed cat"),
                genesisEvent inboxPath 2 t0 (mkTask "TODO" "milk frother")
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        results <- queryTasks repo (emptyQuery {qSearchTerm = Just "milk"})
        length results `shouldBe` 2

  describe "countTasks" $ do
    it "counts rows that match the query" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath 0 t0 (mkTask "TODAY" "a"),
                genesisEvent inboxPath 1 t0 (mkTask "TODO" "b"),
                genesisEvent inboxPath 2 t0 (mkTask "TODAY" "c"),
                genesisEvent inboxPath 3 t0 (mkTask "TRASH" "d")
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        n <- countTasks repo (emptyQuery {qView = Just ViewToday})
        n `shouldBe` 2

  describe "appendEvent / appendEvents" $ do
    it "appendEvent fires the trigger and materializes a row" $
      withFreshRepo $ \(_, repo) -> do
        let task = mkTask "INBOX" "via repo"
            gen = genesisEvent inboxPath 0 t0 task
        appendEvent repo gen
        result <- getTask repo (inboxPath, 0)
        result `shouldBe` Just task

    it "appendEvents inserts a batch and reports its size" $
      withFreshRepo $ \(_, repo) -> do
        let evs =
              [ genesisEvent inboxPath 0 t0 (mkTask "INBOX" "a"),
                genesisEvent inboxPath 1 t0 (mkTask "TODO" "b")
              ]
        n <- appendEvents repo evs
        n `shouldBe` 2
        all_ <- queryTasks repo (emptyQuery {qView = Just ViewAll})
        length all_ `shouldBe` 2

  describe "rebuildTaskCurrentState" $ do
    it "produces the same state as incremental trigger updates" $
      withFreshRepo $ \(dbFile, repo) -> do
        let task1 = mkTask "INBOX" "a"
            task2 = mkTask "TODO" "b"
            evs =
              [ genesisEvent inboxPath 0 t0 task1,
                (emptyEvent inboxPath 0 t1) {evPriority = Just 1},
                genesisEvent inboxPath 1 t0 task2,
                (emptyEvent inboxPath 1 t2) {evTodoKeyword = Just "DONE"}
              ]
        withDatabase dbFile $ \conn -> do
          forM_ evs (insertEvent conn)
        beforeT0 <- getTask repo (inboxPath, 0)
        beforeT1 <- getTask repo (inboxPath, 1)
        -- Drop and rebuild from events
        withDatabase dbFile rebuildTaskCurrentState
        afterT0 <- getTask repo (inboxPath, 0)
        afterT1 <- getTask repo (inboxPath, 1)
        afterT0 `shouldBe` beforeT0
        afterT1 `shouldBe` beforeT1

    it "rebuild from an empty task_current_state recovers all rows" $
      withFreshRepo $ \(dbFile, repo) -> do
        let evs =
              [ genesisEvent inboxPath 0 t0 (mkTask "INBOX" "a"),
                genesisEvent inboxPath 1 t0 (mkTask "TODO" "b"),
                genesisEvent projectsPath 0 t0 (mkTask "PROJECT" "c")
              ]
        withDatabase dbFile $ \conn -> do
          _ <- insertEvents conn evs
          pure ()
        -- Manually clear, bypassing the trigger.
        withDatabase dbFile $ \conn ->
          execute_ conn "DELETE FROM task_current_state"
        emptied <- withDatabase dbFile $ \conn ->
          query_ conn "SELECT COUNT(*) FROM task_current_state" :: IO [Only Int]
        emptied `shouldBe` [Only 0]
        withDatabase dbFile rebuildTaskCurrentState
        afterCount <- withDatabase dbFile $ \conn ->
          query_ conn "SELECT COUNT(*) FROM task_current_state" :: IO [Only Int]
        afterCount `shouldBe` [Only 3]
        -- Spot check one of the rebuilt rows
        result <- getTask repo (projectsPath, 0)
        case result of
          Just t -> _todoKeyword t `shouldBe` "PROJECT"
          Nothing -> expectationFailure "expected projectsPath/0 to be rebuilt"
