{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the per-mode WAI 'Application' wiring exposed by
-- 'Api.Server'. Each server variant should mount only its intended
-- subset of routes:
--
--   * 'webApp' serves UI reads, capture, mutations, and static assets.
--     '/api/events' is /not/ mounted; sync clients see HTTP 404.
--   * 'syncApp' serves only the events transport. UI clients see 404
--     for every read/mutation/capture path.
--   * 'combinedApp' (back-compat for @--serve@) serves both.
--
-- The tests run the Application in-process via 'Network.Wai.Test' — no
-- Warp/network is involved. Each scenario seeds a fresh ephemeral
-- SQLite DB so the events log is non-empty when /api/views/today
-- queries the read model.
module Api.ServerSpec (spec) where

import qualified Api.Server as Server
import Api.Types (HealthResponse (..))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import DB.Connection (initDatabase, withDatabase)
import Events.Store (insertEvents)
import Events.Types (genesisEvent)
import Model.OrgMode (RichText (..), Task (..), TextNode (..), orgTodayKeyword)
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WaiTest
import Parser.OrgParser (anyTaskparser, orgFileParser)
import Repo.EventStoreRepo (mkEventStoreRepo)
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec
import Tui.Types
  ( AppConfig (..),
    AppContext (..),
    SystemConfig (..),
    initializeAppContextForServer,
  )

inboxPath :: FilePath
inboxPath = "/inbox.org"

projectsPath :: FilePath
projectsPath = "/projects.org"

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 5 3) (secondsToDiffTime 0)

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

withSeededDb :: (FilePath -> IO a) -> IO a
withSeededDb action = do
  dbPath <- emptySystemTempFile "dwayne-server-test.db"
  initDatabase dbPath
  _ <-
    withDatabase dbPath $ \conn ->
      insertEvents
        conn
        [genesisEvent inboxPath 0 t0 (mkTask orgTodayKeyword "today task")]
  r <- action dbPath
  removeFile dbPath
  pure r

-- | Run a single fake request through an Application and read the
-- status code back. 'WaiTest.runSession' invokes the Application
-- in-process; no Warp socket is opened.
statusOf :: Wai.Application -> HT.Method -> [T.Text] -> IO Int
statusOf app method pathSegs = do
  resp <- responseOf app method pathSegs
  pure (HT.statusCode (WaiTest.simpleStatus resp))

responseOf :: Wai.Application -> HT.Method -> [T.Text] -> IO WaiTest.SResponse
responseOf app method pathSegs = do
  let req =
        WaiTest.setPath
          (WaiTest.defaultRequest {Wai.requestMethod = method})
          ("/" <> TE.encodeUtf8 (T.intercalate "/" pathSegs))
  WaiTest.runSession
    (WaiTest.request req)
    app

healthOf :: Wai.Application -> IO HealthResponse
healthOf app = do
  resp <- responseOf app HT.methodGet ["api", "health"]
  HT.statusCode (WaiTest.simpleStatus resp) `shouldBe` 200
  case Aeson.eitherDecode (WaiTest.simpleBody resp) of
    Left err -> expectationFailure err >> pure (HealthResponse "" "" "" 0)
    Right health -> pure health

spec :: Spec
spec = do
  describe "Api.Server route mounting per --mode flag" $ do
    it "webApp serves /api/views/today" $
      withSeededDb $ \db -> do
        let ctx = mkCtx db
        st <- Server.initServerState ctx
        code <- statusOf (Server.webApp st) HT.methodGet ["api", "views", "today"]
        code `shouldBe` 200

    it "webApp serves /api/health with web mode and task count" $
      withSeededDb $ \db -> do
        let ctx = mkCtx db
        st <- Server.initServerState ctx
        health <- healthOf (Server.webApp st)
        hrStatus health `shouldBe` "ok"
        hrVersion health `shouldSatisfy` (not . T.null)
        hrMode health `shouldBe` "web"
        hrTasks health `shouldBe` 1

    it "webApp returns 404 for /api/events (sync routes excluded)" $
      withSeededDb $ \db -> do
        let ctx = mkCtx db
        st <- Server.initServerState ctx
        code <- statusOf (Server.webApp st) HT.methodGet ["api", "events"]
        code `shouldBe` 404

    it "syncApp serves /api/events" $
      withSeededDb $ \db -> do
        let ctx = mkCtx db
        st <- Server.initServerState ctx
        code <- statusOf (Server.syncApp st) HT.methodGet ["api", "events"]
        code `shouldBe` 200

    it "syncApp serves /api/health with sync mode and task count" $
      withSeededDb $ \db -> do
        let ctx = mkCtx db
        st <- Server.initServerState ctx
        health <- healthOf (Server.syncApp st)
        hrStatus health `shouldBe` "ok"
        hrVersion health `shouldSatisfy` (not . T.null)
        hrMode health `shouldBe` "sync"
        hrTasks health `shouldBe` 1

    it "syncApp returns 404 for /api/views/today (web routes excluded)" $
      withSeededDb $ \db -> do
        let ctx = mkCtx db
        st <- Server.initServerState ctx
        code <- statusOf (Server.syncApp st) HT.methodGet ["api", "views", "today"]
        code `shouldBe` 404

    it "combinedApp serves both /api/views/today and /api/events" $
      withSeededDb $ \db -> do
        let ctx = mkCtx db
        st <- Server.initServerState ctx
        viewCode <- statusOf (Server.combinedApp st) HT.methodGet ["api", "views", "today"]
        eventsCode <- statusOf (Server.combinedApp st) HT.methodGet ["api", "events"]
        viewCode `shouldBe` 200
        eventsCode `shouldBe` 200

    it "combinedApp serves /api/health with combined mode and task count" $
      withSeededDb $ \db -> do
        let ctx = mkCtx db
        st <- Server.initServerState ctx
        health <- healthOf (Server.combinedApp st)
        hrStatus health `shouldBe` "ok"
        hrVersion health `shouldSatisfy` (not . T.null)
        hrMode health `shouldBe` "combined"
        hrTasks health `shouldBe` 1
