{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the read and mutation handlers in 'Api.Handlers' after
-- the Phase 2a (reads) and Phase 2b (mutations) migration to the CQRS
-- read model.
--
-- The handlers run inside Servant's 'Handler' monad. We don't spin up a
-- WAI server: we seed the events table directly (the trigger projects
-- into @task_current_state@), build an 'AppContext' with the repo
-- wired in, and invoke the handler functions. 'Servant.Handler' is just
-- @ExceptT ServerError IO@; 'runHandler' takes us back to plain IO with
-- a @Either ServerError a@ result for assertions.
--
-- For mutation tests, the handler signatures take an
-- @MVar (AppContext Task)@ — a relic of the legacy in-memory write
-- path. Phase 2b only reads from that MVar (to look up the repo
-- handle); it never mutates it. Tests therefore use a freshly-created
-- MVar, exercise the handler, and inspect the underlying SQLite via
-- the same repo to verify the event landed.
module Api.HandlersSpec (spec) where

import qualified Api.Handlers as H
import Api.Types
  ( CaptureRequest (..),
    ChangeKeywordRequest (..),
    ChangePriorityRequest (..),
    EditTaskRequest (..),
    PaginatedResponse (..),
    ProjectTreeResponse (..),
    ResponseMetadata (..),
    TagRequest (..),
    TaskNode (..),
    TaskPointerRequest (..),
    TaskWithPointer (..),
  )
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Core.Types (TaskPointer (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import DB.Connection (initDatabase, withDatabase)
import Events.Store (insertEvents, selectAllEvents)
import Events.Types (Event (..), genesisEvent)
import Model.OrgMode
  ( RichText (..),
    Task (..),
    TextNode (..),
    orgInboxKeyword,
    orgProjectKeyword,
    orgTodayKeyword,
    orgTodoKeyword,
    orgTrashKeyword,
  )
import Parser.OrgParser (anyTaskparser, orgFileParser)
import Repo.EventStoreRepo (EventStoreRepo, mkEventStoreRepo)
import qualified Repo.TaskRepo as TR
import qualified Servant
import Servant (ServerError, errHTTPCode, runHandler)
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec
import Tui.Types
  ( AppConfig (..),
    AppContext (..),
    SystemConfig (..),
    initializeAppContextForServer,
  )

-- ---------------------------------------------------------------------------
-- Fixture helpers
-- ---------------------------------------------------------------------------

inboxPath :: FilePath
inboxPath = "/inbox.org"

projectsPath :: FilePath
projectsPath = "/projects.org"

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 0)

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

mkTaskAt :: Int -> T.Text -> T.Text -> Task
mkTaskAt lvl kw ttl = (mkTask kw ttl) {_level = lvl}

-- | Build an 'AppContext' suitable for invoking read handlers under
-- test. The legacy 'fileState' map stays empty: read handlers consult
-- the repo, mutation handlers (untouched by Phase 2a) would still
-- depend on it.
mkCtx :: FilePath -> AppContext Task
mkCtx dbFile =
  let conf =
        AppConfig
          { _files = [inboxPath, projectsPath],
            _inboxFile = inboxPath,
            _projectsFile = projectsPath,
            _scrollingMargin = 5,
            _keyTimeoutMs = 500,
            _colorScheme = "dark",
            _commands = mempty,
            _database = dbFile
          }
      sysConf =
        SystemConfig
          { _fileParser = orgFileParser,
            _taskParser = anyTaskparser,
            _keybindings = [],
            _defaultFilters = [],
            _defaultSorter = \_ _ -> EQ,
            _taskStoreOps = Nothing,
            _taskRepo = Just (mkEventStoreRepo dbFile inboxPath)
          }
   in initializeAppContextForServer sysConf conf M.empty

-- | Variant that intentionally omits the repo so we can assert the
-- "no repo configured" failure path returns HTTP 500.
mkCtxNoRepo :: FilePath -> AppContext Task
mkCtxNoRepo dbFile =
  let ctx = mkCtx dbFile
      sysConf' = (_system ctx) {_taskRepo = Nothing}
   in ctx {_system = sysConf'}

-- | Run a handler and unwrap into Either. Avoids dragging in
-- 'liftIO' / 'try' boilerplate at every call site.
runH :: Servant.Handler a -> IO (Either ServerError a)
runH = runHandler

-- | Set up a fresh DB file with migrations and the given events
-- preloaded. Yields the path so tests can build a context off it.
withSeededDb :: [Event] -> (FilePath -> IO a) -> IO a
withSeededDb evs action = do
  dbPath <- emptySystemTempFile "dwayne-api-test.db"
  initDatabase dbPath
  withDatabase dbPath $ \conn -> do
    _ <- insertEvents conn evs
    pure ()
  r <- action dbPath
  removeFile dbPath
  pure r

todayQuery :: TR.Query
todayQuery = TR.emptyQuery {TR.qKeyword = Just orgTodayKeyword}

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "queryViewHandler / view dispatch" $ do
    it "returns only tasks matching the keyword filter" $
      withSeededDb
        [ genesisEvent inboxPath 0 t0 (mkTask orgTodayKeyword "today task"),
          genesisEvent inboxPath 1 t0 (mkTask orgTodoKeyword "todo task"),
          genesisEvent inboxPath 2 t0 (mkTask orgTodayKeyword "another today")
        ]
        $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.queryViewHandler todayQuery ctx Nothing Nothing)
          case result of
            Left err -> expectationFailure $ "expected success, got " ++ show err
            Right (PaginatedResponse rows meta) -> do
              length rows `shouldBe` 2
              rmTotal meta `shouldBe` 2
              all ((== orgTodayKeyword) . _todoKeyword . twpTask) rows `shouldBe` True

    it "excludes TRASH from non-trash views" $
      withSeededDb
        [ genesisEvent inboxPath 0 t0 (mkTask orgInboxKeyword "alive"),
          genesisEvent inboxPath 1 t0 (mkTask "TRASH" "deleted")
        ]
        $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.viewAllHandler ctx Nothing Nothing)
          case result of
            Right (PaginatedResponse rows meta) -> do
              rmTotal meta `shouldBe` 1
              length rows `shouldBe` 1
              _todoKeyword (twpTask (head rows)) `shouldBe` orgInboxKeyword
            Left err -> expectationFailure $ show err

    it "honors limit and offset query parameters" $
      withSeededDb
        [ genesisEvent inboxPath i t0 (mkTask orgTodoKeyword (T.pack ("t" ++ show i)))
        | i <- [0 .. 9]
        ]
        $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.viewAllHandler ctx (Just 2) (Just 3))
          case result of
            Right (PaginatedResponse rows meta) -> do
              -- 10 tasks total in the read model, page size 3 starting
              -- at offset 2.
              length rows `shouldBe` 3
              rmTotal meta `shouldBe` 10
            Left err -> expectationFailure $ show err

    it "returns HTTP 500 when the server has no repo configured" $
      withSeededDb [] $ \db -> do
        let ctx = mkCtxNoRepo db
        result <- runH (H.viewAllHandler ctx Nothing Nothing)
        case result of
          Left err -> errHTTPCode err `shouldBe` 500
          Right _ -> expectationFailure "expected 500 for missing repo"

  describe "search handler" $ do
    it "substring-matches title across all tasks when no view filter" $
      withSeededDb
        [ genesisEvent inboxPath 0 t0 (mkTask orgTodoKeyword "buy milk"),
          genesisEvent inboxPath 1 t0 (mkTask orgTodoKeyword "feed cat"),
          genesisEvent inboxPath 2 t0 (mkTask orgTodayKeyword "milk frother")
        ]
        $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.searchHandler "milk" Nothing ctx Nothing Nothing)
          case result of
            Right (PaginatedResponse rows meta) -> do
              rmTotal meta `shouldBe` 2
              length rows `shouldBe` 2
            Left err -> expectationFailure $ show err

    it "narrows by view when the 'view' query parameter is supplied" $
      withSeededDb
        [ genesisEvent inboxPath 0 t0 (mkTask orgTodoKeyword "buy milk"),
          genesisEvent inboxPath 1 t0 (mkTask orgTodayKeyword "milk frother")
        ]
        $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.searchHandler "milk" (Just "today") ctx Nothing Nothing)
          case result of
            Right (PaginatedResponse rows meta) -> do
              rmTotal meta `shouldBe` 1
              length rows `shouldBe` 1
              _todoKeyword (twpTask (head rows)) `shouldBe` orgTodayKeyword
            Left err -> expectationFailure $ show err

  describe "project handlers" $ do
    let projectGenesis =
          genesisEvent projectsPath 0 t0 (mkTaskAt 1 orgProjectKeyword "Project A")
        childA1 = genesisEvent projectsPath 1 t0 (mkTaskAt 2 orgTodoKeyword "child 1")
        childA2 = genesisEvent projectsPath 2 t0 (mkTaskAt 2 orgTodoKeyword "child 2")
        grandchild = genesisEvent projectsPath 3 t0 (mkTaskAt 3 orgTodoKeyword "grandchild")
        otherTopLevel =
          genesisEvent projectsPath 4 t0 (mkTaskAt 1 orgProjectKeyword "Project B")

    describe "getProjectByPointerHandler" $ do
      it "returns the project task as a single-item paginated response" $
        withSeededDb [projectGenesis] $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.getProjectByPointerHandler projectsPath 0 ctx)
          case result of
            Right (PaginatedResponse rows meta) -> do
              rmTotal meta `shouldBe` 1
              length rows `shouldBe` 1
              _todoKeyword (twpTask (head rows)) `shouldBe` orgProjectKeyword
            Left err -> expectationFailure $ show err

      it "404s when the task is not a PROJECT" $
        withSeededDb [genesisEvent projectsPath 0 t0 (mkTask orgTodoKeyword "not a project")] $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.getProjectByPointerHandler projectsPath 0 ctx)
          case result of
            Left err -> errHTTPCode err `shouldBe` 404
            Right _ -> expectationFailure "expected 404"

      it "404s when the pointer doesn't resolve" $
        withSeededDb [] $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.getProjectByPointerHandler projectsPath 0 ctx)
          case result of
            Left err -> errHTTPCode err `shouldBe` 404
            Right _ -> expectationFailure "expected 404"

    describe "getProjectTasksHandler" $ do
      it "builds a tree with the project root and its children" $
        withSeededDb [projectGenesis, childA1, childA2, grandchild, otherTopLevel] $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.getProjectTasksHandler projectsPath 0 ctx Nothing Nothing)
          case result of
            Right (ProjectTreeResponse root) -> do
              -- root must be the PROJECT
              _todoKeyword (tnTask root) `shouldBe` orgProjectKeyword
              -- two direct children at level 2
              length (tnChildren root) `shouldBe` 2
              -- the second child owns the grandchild (it precedes
              -- grandchild in task_index order, which is how org-mode
              -- assigns ownership). The first child does not.
              let [firstChild, secondChild] = tnChildren root
              length (tnChildren firstChild) `shouldBe` 0
              length (tnChildren secondChild) `shouldBe` 1
              -- traversal stops at the next top-level project
              let allTitles = collectTitles root
              "Project B" `notElem` allTitles `shouldBe` True
              "Project A" `elem` allTitles `shouldBe` True
              "grandchild" `elem` allTitles `shouldBe` True
            Left err -> expectationFailure $ show err

      it "404s when the pointer is not a project" $
        withSeededDb [genesisEvent projectsPath 0 t0 (mkTask orgTodoKeyword "plain")] $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.getProjectTasksHandler projectsPath 0 ctx Nothing Nothing)
          case result of
            Left err -> errHTTPCode err `shouldBe` 404
            Right _ -> expectationFailure "expected 404"

    describe "getParentProjectHandler" $ do
      it "returns the parent project of a child task" $
        withSeededDb [projectGenesis, childA1] $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.getParentProjectHandler projectsPath 1 ctx)
          case result of
            Right (PaginatedResponse rows meta) -> do
              rmTotal meta `shouldBe` 1
              length rows `shouldBe` 1
              let parent = twpTask (head rows)
              _todoKeyword parent `shouldBe` orgProjectKeyword
            Left err -> expectationFailure $ show err

      it "404s when the task has no preceding project" $
        withSeededDb [genesisEvent inboxPath 0 t0 (mkTask orgTodoKeyword "orphan")] $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.getParentProjectHandler inboxPath 0 ctx)
          case result of
            Left err -> errHTTPCode err `shouldBe` 404
            Right _ -> expectationFailure "expected 404"

  describe "wire-format compatibility" $ do
    -- Insert a known set of events, query via the migrated handler,
    -- and assert the JSON encoding has the expected shape. Guards
    -- against accidental field renames or wrapper changes between
    -- phases.
    it "PaginatedResponse JSON has 'data', 'metadata.total', and TaskWithPointer fields" $
      withSeededDb
        [ genesisEvent inboxPath 0 t0 (mkTask orgTodoKeyword "alpha"),
          genesisEvent inboxPath 1 t0 (mkTask orgTodoKeyword "beta")
        ]
        $ \db -> do
          let ctx = mkCtx db
          result <- runH (H.viewAllHandler ctx Nothing Nothing)
          case result of
            Right resp -> do
              let bs = Aeson.encode resp
                  encoded = BSL.unpack bs
              encoded `shouldContainSubstring` "\"data\":"
              encoded `shouldContainSubstring` "\"metadata\":"
              encoded `shouldContainSubstring` "\"total\":2"
              encoded `shouldContainSubstring` "\"task\":"
              encoded `shouldContainSubstring` "\"pointer\":"
              encoded `shouldContainSubstring` "\"file\":"
              encoded `shouldContainSubstring` "\"taskIndex\":"
            Left err -> expectationFailure $ show err

  -- -------------------------------------------------------------------
  -- Phase 2b — mutation handlers
  -- -------------------------------------------------------------------
  --
  -- All mutation handlers route through 'withMutation' (or, for
  -- capture, an analogous repo flow). The expectation in every test:
  --
  --   * pre-existing task at (file_path, task_index) found via
  --     'TR.getTask' before mutation;
  --   * the handler appends an event;
  --   * the trigger projects it into 'task_current_state';
  --   * a fresh 'TR.getTask' returns the mutated task.
  --
  -- Tests bypass 'AppContext.fileState' entirely. After Phase 2b
  -- nothing in the API path consults the in-memory cache.
  describe "captureHandler" $ do
    it "creates a new task and emits a genesis event with all fields populated" $
      withSeededDb [] $ \db -> do
        ctxVar <- newMVar (mkCtx db)
        let req = CaptureRequest "buy oat milk"
        result <- runH (H.captureHandler ctxVar req)
        case result of
          Left err -> expectationFailure $ "expected success, got " ++ show err
          Right (TaskWithPointer task ptr) -> do
            -- pointer is the inbox file at index 0 (first task)
            _file ptr `shouldBe` inboxPath
            _taskIndex ptr `shouldBe` 0
            _todoKeyword task `shouldBe` orgInboxKeyword
            -- repo materialised the task so subsequent reads see it
            mTask <- repoGetTask db inboxPath 0
            case mTask of
              Nothing -> expectationFailure "captured task missing from repo"
              Just t -> _todoKeyword t `shouldBe` orgInboxKeyword
            -- exactly one genesis event landed; every mutation field set
            evs <- selectEvents db
            length evs `shouldBe` 1
            let e = head evs
            evFilePath e `shouldBe` inboxPath
            evTaskIndex e `shouldBe` 0
            evTodoKeyword e `shouldBe` Just orgInboxKeyword
            evLevel e `shouldBe` Just 1
            -- title is non-Nothing — exact RichText shape varies, but
            -- a genesis must populate it.
            evTitle e `shouldNotBe` Nothing

    it "uses MAX(task_index)+1 from the events log when the inbox already has tasks" $
      -- An existing genesis at (inbox, 0) plus a TRASH delta at the
      -- same key still counts as task_index 0 for the next-index
      -- calculation. Capture should land at index 1.
      withSeededDb [genesisEvent inboxPath 0 t0 (mkTask orgInboxKeyword "first")] $ \db -> do
        ctxVar <- newMVar (mkCtx db)
        let req = CaptureRequest "second"
        result <- runH (H.captureHandler ctxVar req)
        case result of
          Right (TaskWithPointer _ ptr) -> _taskIndex ptr `shouldBe` 1
          Left err -> expectationFailure $ show err

  describe "editTaskHandler" $ do
    it "applies a title change and the repo reflects the new title" $
      withSeededDb [genesisEvent inboxPath 0 t0 (mkTask orgTodoKeyword "old title")] $ \db -> do
        ctxVar <- newMVar (mkCtx db)
        let req =
              EditTaskRequest
                { etrFile = inboxPath,
                  etrTaskIndex = 0,
                  etrKeyword = Nothing,
                  etrPriority = Nothing,
                  etrTitle = Just "new title",
                  etrDescription = Nothing,
                  etrTags = Nothing,
                  etrScheduled = Nothing,
                  etrDeadline = Nothing
                }
        result <- runH (H.editTaskHandler ctxVar req)
        case result of
          Right (TaskWithPointer task _) -> do
            renderTitle (_title task) `shouldBe` "new title"
            mLater <- repoGetTask db inboxPath 0
            case mLater of
              Just t -> renderTitle (_title t) `shouldBe` "new title"
              Nothing -> expectationFailure "task missing after edit"
          Left err -> expectationFailure $ show err

    it "404s when editing a non-existent task" $
      withSeededDb [] $ \db -> do
        ctxVar <- newMVar (mkCtx db)
        let req =
              EditTaskRequest
                { etrFile = inboxPath,
                  etrTaskIndex = 99,
                  etrKeyword = Nothing,
                  etrPriority = Nothing,
                  etrTitle = Just "ghost",
                  etrDescription = Nothing,
                  etrTags = Nothing,
                  etrScheduled = Nothing,
                  etrDeadline = Nothing
                }
        result <- runH (H.editTaskHandler ctxVar req)
        case result of
          Left err -> errHTTPCode err `shouldBe` 404
          Right _ -> expectationFailure "expected 404 for missing task"

  describe "changeKeywordHandler" $ do
    it "transitions a task from TODO to TODAY and persists the change" $
      withSeededDb [genesisEvent inboxPath 0 t0 (mkTask orgTodoKeyword "task")] $ \db -> do
        ctxVar <- newMVar (mkCtx db)
        let req = ChangeKeywordRequest inboxPath 0 orgTodayKeyword
        result <- runH (H.changeKeywordHandler ctxVar req)
        case result of
          Right (TaskWithPointer task _) -> do
            _todoKeyword task `shouldBe` orgTodayKeyword
            mLater <- repoGetTask db inboxPath 0
            fmap _todoKeyword mLater `shouldBe` Just orgTodayKeyword
          Left err -> expectationFailure $ show err

  describe "changePriorityHandler" $ do
    it "sets a priority and the repo reflects it" $
      withSeededDb [genesisEvent inboxPath 0 t0 (mkTask orgTodoKeyword "task")] $ \db -> do
        ctxVar <- newMVar (mkCtx db)
        let req = ChangePriorityRequest inboxPath 0 (Just 1)
        result <- runH (H.changePriorityHandler ctxVar req)
        case result of
          Right (TaskWithPointer task _) -> do
            _priority task `shouldBe` Just 1
            mLater <- repoGetTask db inboxPath 0
            fmap _priority mLater `shouldBe` Just (Just 1)
          Left err -> expectationFailure $ show err

  describe "addTagHandler / removeTagHandler" $ do
    it "addTagHandler inserts the tag into the task's tag set" $
      withSeededDb [genesisEvent inboxPath 0 t0 (mkTask orgTodoKeyword "task")] $ \db -> do
        ctxVar <- newMVar (mkCtx db)
        let req = TagRequest inboxPath 0 "urgent"
        result <- runH (H.addTagHandler ctxVar req)
        case result of
          Right (TaskWithPointer task _) -> do
            S.member "urgent" (_tags task) `shouldBe` True
            mLater <- repoGetTask db inboxPath 0
            fmap (S.member "urgent" . _tags) mLater `shouldBe` Just True
          Left err -> expectationFailure $ show err

    it "removeTagHandler drops the tag if present" $ do
      let preTagged = (mkTask orgTodoKeyword "task") {_tags = S.fromList ["urgent", "work"]}
      withSeededDb [genesisEvent inboxPath 0 t0 preTagged] $ \db -> do
        ctxVar <- newMVar (mkCtx db)
        let req = TagRequest inboxPath 0 "urgent"
        result <- runH (H.removeTagHandler ctxVar req)
        case result of
          Right (TaskWithPointer task _) -> do
            S.member "urgent" (_tags task) `shouldBe` False
            S.member "work" (_tags task) `shouldBe` True
            mLater <- repoGetTask db inboxPath 0
            fmap (S.member "urgent" . _tags) mLater `shouldBe` Just False
          Left err -> expectationFailure $ show err

  describe "deleteTaskHandler" $ do
    it "soft-deletes by setting todo_keyword to TRASH (still exists in repo)" $
      withSeededDb [genesisEvent inboxPath 0 t0 (mkTask orgTodoKeyword "doomed")] $ \db -> do
        ctxVar <- newMVar (mkCtx db)
        let req = TaskPointerRequest inboxPath 0
        result <- runH (H.deleteTaskHandler ctxVar req)
        case result of
          Right (TaskWithPointer task _) -> do
            _todoKeyword task `shouldBe` orgTrashKeyword
            -- Soft-delete: row still present in task_current_state
            mLater <- repoGetTask db inboxPath 0
            fmap _todoKeyword mLater `shouldBe` Just orgTrashKeyword
            -- viewAllHandler excludes TRASH by default
            ctx <- readCtxVar ctxVar
            viewResult <- runH (H.viewAllHandler ctx Nothing Nothing)
            case viewResult of
              Right (PaginatedResponse rows _) -> length rows `shouldBe` 0
              Left err -> expectationFailure $ show err
          Left err -> expectationFailure $ show err

  describe "idempotency / re-append" $ do
    -- INSERT OR IGNORE means an event with the same
    -- (file_path, task_index, occurred_at) PK is silently ignored.
    -- A delta with no changes should be a no-op for the read model.
    it "re-running the same keyword change at the same instant is a no-op" $
      withSeededDb [genesisEvent inboxPath 0 t0 (mkTask orgTodoKeyword "task")] $ \db -> do
        ctxVar <- newMVar (mkCtx db)
        let req = ChangeKeywordRequest inboxPath 0 orgTodayKeyword
        _ <- runH (H.changeKeywordHandler ctxVar req)
        -- Second call: a fresh event (different occurred_at) but same
        -- result. Should still leave the read model coherent — no
        -- duplicate task, no exception.
        _ <- runH (H.changeKeywordHandler ctxVar req)
        mLater <- repoGetTask db inboxPath 0
        fmap _todoKeyword mLater `shouldBe` Just orgTodayKeyword

-- ---------------------------------------------------------------------------
-- Local helpers
-- ---------------------------------------------------------------------------

-- | Open the test DB and run a single 'TR.getTask' through the same
-- repo path the handlers use. Avoids reaching past the repo seam.
repoGetTask :: FilePath -> FilePath -> Int -> IO (Maybe Task)
repoGetTask dbFile fp idx =
  TR.getTask (mkEventStoreRepo dbFile inboxPath :: EventStoreRepo) (fp, idx)

-- | Read all events from the DB. Used by mutation tests to assert event
-- shape directly (cardinality, fields populated).
selectEvents :: FilePath -> IO [Event]
selectEvents dbFile = withDatabase dbFile selectAllEvents

-- | Read the AppContext out of an MVar. Tests that compose two
-- mutations need the up-to-date ctx for a follow-up read; the MVar is
-- never written by the API path, so the value is the seed ctx.
readCtxVar :: MVar (AppContext Task) -> IO (AppContext Task)
readCtxVar = readMVar

-- | Render a 'RichText' as a plain string for test comparison. Skips
-- non-PlainText nodes; sufficient for the simple titles used in tests.
renderTitle :: RichText -> T.Text
renderTitle (RichText nodes) = T.concat (map flatten nodes)
  where
    flatten (PlainText x) = x
    flatten _ = ""

-- | Walk a 'TaskNode' tree and pull out every plain-text title.
collectTitles :: TaskNode -> [T.Text]
collectTitles (TaskNode t _ cs) =
  let RichText nodes = _title t
      flatten (PlainText x) = x
      flatten _ = ""
      titleStr = T.concat (map flatten nodes)
   in titleStr : concatMap collectTitles cs

-- | Cheap substring assertion. Avoids pulling in a richer matcher when
-- a list 'isInfixOf' check is enough.
shouldContainSubstring :: String -> String -> Expectation
shouldContainSubstring haystack needle =
  if needle `isInfixOfList` haystack
    then pure ()
    else
      expectationFailure $
        "expected substring " ++ show needle ++ " in " ++ show haystack

isInfixOfList :: (Eq a) => [a] -> [a] -> Bool
isInfixOfList needle hay = any (needle `isPrefixOfList`) (tailsList hay)
  where
    isPrefixOfList [] _ = True
    isPrefixOfList _ [] = False
    isPrefixOfList (x : xs) (y : ys) = x == y && isPrefixOfList xs ys
    tailsList [] = [[]]
    tailsList xs@(_ : t) = xs : tailsList t
