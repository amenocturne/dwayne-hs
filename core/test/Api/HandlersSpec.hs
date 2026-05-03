{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the read handlers in 'Api.Handlers' after the Phase 2a
-- migration to the CQRS read model.
--
-- The handlers run inside Servant's 'Handler' monad. We don't spin up a
-- WAI server: we seed the events table directly (the trigger projects
-- into @task_current_state@), build an 'AppContext' with the repo
-- wired in, and invoke the handler functions. 'Servant.Handler' is just
-- @ExceptT ServerError IO@; 'runHandler' takes us back to plain IO with
-- a @Either ServerError a@ result for assertions.
module Api.HandlersSpec (spec) where

import qualified Api.Handlers as H
import Api.Types
  ( PaginatedResponse (..),
    ProjectTreeResponse (..),
    ResponseMetadata (..),
    TaskNode (..),
    TaskWithPointer (..),
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import DB.Connection (initDatabase, withDatabase)
import Events.Store (insertEvents)
import Events.Types (Event, genesisEvent)
import Model.OrgMode
  ( RichText (..),
    Task (..),
    TextNode (..),
    orgInboxKeyword,
    orgProjectKeyword,
    orgTodayKeyword,
    orgTodoKeyword,
  )
import Parser.OrgParser (anyTaskparser, orgFileParser)
import Repo.EventStoreRepo (mkEventStoreRepo)
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

-- ---------------------------------------------------------------------------
-- Local helpers
-- ---------------------------------------------------------------------------

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
