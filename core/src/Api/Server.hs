{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | HTTP server for the REST API and static file serving.
--
-- The route surface is split into two independent Servant API trees:
--
--   * 'WebAPI' — UI-facing reads (views, search, projects), capture
--     mutations, and the static front-end. This is what the web UI and
--     Raycast extension talk to.
--   * 'SyncAPI' — '/api/events' (push + pull) only. This is the
--     transport for cross-device sync; it carries no UI logic.
--
-- A combined deployment ('runCombinedServer' / 'app') mounts both — the
-- legacy default for local dev so a single @dwayne --serve@ keeps the
-- web UI working alongside @dwayne sync@. Production deployments can
-- run @dwayne --web@ and @dwayne --sync-server@ as separate processes
-- and only expose the routes each environment actually needs.
module Api.Server
  ( runServer,
    runWebServer,
    runSyncServer,
    -- Internals exposed for tests
    ServerState,
    webApp,
    syncApp,
    combinedApp,
    initServerState,
  )
where

import qualified Api.EventHandlers as EH
import qualified Api.Handlers
import Api.Types (ApiBinding (..), ApiMethod (..), CaptureRequest, ChangeKeywordRequest, ChangePriorityRequest, EditTaskRequest, HealthResponse (..), PaginatedResponse (..), ProjectTreeResponse, ResponseMetadata (..), TagRequest, TaskPointerRequest, TaskWithPointer)
import qualified Api.WebSocket as WS
import Commands.Command (Command (..), getEnabledCommands)
import Commands.Registry (allCommands)
import Control.Concurrent.MVar
import Control.Lens (view)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Version (showVersion)
import Database.SQLite.Simple (Only (..), query_)
import DB.Connection (withDatabase)
import qualified FileWatcher as FW
import Model.OrgMode (Task)
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Network.Wai.Middleware.Cors (cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy)
import qualified Network.WebSockets as WebSockets
import Servant
  ( Application,
    Capture,
    Get,
    Handler,
    JSON,
    Post,
    Proxy (..),
    Put,
    QueryParam,
    QueryParam',
    Raw,
    ReqBody,
    Required,
    Server,
    err404,
    serve,
    serveDirectoryWebApp,
    throwError,
    type (:<|>) (..),
    type (:>),
  )
import Paths_dwayne_hs (version)
import Tui.Types (AppContext, commands, config, getAllFiles)
import qualified Tui.Types as TT

type HealthAPI = "health" :> Get '[JSON] HealthResponse

-- | API type for view endpoints — catch-all route that dispatches at runtime
type ViewsAPI =
  "views"
    :> Capture "view" T.Text
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> Get '[JSON] (PaginatedResponse TaskWithPointer)

-- | Search API for full-text search across tasks
type SearchAPI =
  "search"
    :> QueryParam' '[Required] "query" T.Text
    :> QueryParam "view" T.Text
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> Get '[JSON] (PaginatedResponse TaskWithPointer)

-- | Projects API for project-specific queries
type ProjectsAPI =
  "projects"
    :> "by-pointer"
    :> QueryParam' '[Required] "file" FilePath
    :> QueryParam' '[Required] "taskIndex" Int
    :> Get '[JSON] (PaginatedResponse TaskWithPointer)
    :<|> "projects"
      :> "tasks"
      :> QueryParam' '[Required] "file" FilePath
      :> QueryParam' '[Required] "taskIndex" Int
      :> QueryParam "offset" Int
      :> QueryParam "limit" Int
      :> Get '[JSON] ProjectTreeResponse
    :<|> "projects"
      :> "parent"
      :> QueryParam' '[Required] "file" FilePath
      :> QueryParam' '[Required] "taskIndex" Int
      :> Get '[JSON] (PaginatedResponse TaskWithPointer)

-- | Capture API for quick task creation
type CaptureAPI =
  "tasks"
    :> "capture"
    :> ReqBody '[JSON] CaptureRequest
    :> Post '[JSON] TaskWithPointer

-- | Mutation API for task modifications
type MutationAPI =
  "tasks" :> "edit" :> ReqBody '[JSON] EditTaskRequest :> Put '[JSON] TaskWithPointer
    :<|> "tasks" :> "keyword" :> ReqBody '[JSON] ChangeKeywordRequest :> Put '[JSON] TaskWithPointer
    :<|> "tasks" :> "priority" :> ReqBody '[JSON] ChangePriorityRequest :> Put '[JSON] TaskWithPointer
    :<|> "tasks" :> "tags" :> "add" :> ReqBody '[JSON] TagRequest :> Post '[JSON] TaskWithPointer
    :<|> "tasks" :> "tags" :> "remove" :> ReqBody '[JSON] TagRequest :> Post '[JSON] TaskWithPointer
    :<|> "tasks" :> "delete" :> ReqBody '[JSON] TaskPointerRequest :> Post '[JSON] TaskWithPointer

-- | Events API: event-sourced sync. Replaces the legacy /api/sync/* surface.
type EventsAPI =
  "events" :> QueryParam "since" T.Text :> Get '[JSON] EH.EventsResponse
    :<|> "events" :> ReqBody '[JSON] EH.PostEventsRequest :> Post '[JSON] EH.PostEventsResponse

-- | UI / read-side surface plus capture, exposed at @\/api\/*@ alongside
-- the static web bundle at @\/*@. No sync routes; deploy this when the
-- only client is the web UI / Raycast extension.
type WebAPI =
  "api" :> (HealthAPI :<|> ViewsAPI :<|> SearchAPI :<|> ProjectsAPI :<|> CaptureAPI :<|> MutationAPI)
    :<|> Raw

-- | Sync transport surface — push + pull of events. Deploy this when
-- the host is just a sync hub for other clients (Android / TUI).
type SyncAPI = "api" :> (HealthAPI :<|> EventsAPI)

-- | Combined surface for local dev convenience: WebAPI plus sync.
-- Static file serving stays attached to the web side so a single
-- @dwayne --serve@ still drives the UI.
type CombinedAPI =
  "api" :> (HealthAPI :<|> ViewsAPI :<|> SearchAPI :<|> ProjectsAPI :<|> CaptureAPI :<|> MutationAPI :<|> EventsAPI)
    :<|> Raw

-- | Server state containing cached context and WebSocket registry
data ServerState = ServerState
  { stateCache :: MVar (AppContext Task),
    stateRegistry :: WS.ClientRegistry,
    stateWatcher :: Maybe FW.WatcherHandle
  }

-- | Find the API handler for a given endpoint in the command list
findViewHandler :: T.Text -> [Command Task] -> Maybe (AppContext Task -> Maybe Int -> Maybe Int -> Handler (PaginatedResponse TaskWithPointer))
findViewHandler endpoint cmds =
  case filter matchesEndpoint cmds of
    (cmd : _) -> apiHandler <$> cmdApi cmd
    [] -> Nothing
  where
    matchesEndpoint cmd = case cmdApi cmd of
      Just binding -> apiEndpoint binding == endpoint && apiMethod binding == GET
      Nothing -> False

healthServer :: T.Text -> ServerState -> Server HealthAPI
healthServer mode serverState = do
  ctx <- liftIO $ readMVar (stateCache serverState)
  let dbFile = TT._database (view config ctx)
  taskCount <- liftIO $ countCurrentTasks dbFile
  pure $
    HealthResponse
      { hrStatus = "ok",
        hrVersion = T.pack (showVersion version),
        hrMode = mode,
        hrTasks = taskCount
      }

countCurrentTasks :: FilePath -> IO Int
countCurrentTasks dbFile =
  withDatabase dbFile $ \conn -> do
    rows <- query_ conn "SELECT COUNT(*) FROM task_current_state" :: IO [Only Int]
    pure $ case rows of
      (Only n : _) -> n
      [] -> 0

-- | Build the Views API server — dispatches to the matching command at runtime
viewsServer :: ServerState -> Server ViewsAPI
viewsServer serverState viewName mOffset mLimit = do
  ctx <- liftIO $ readMVar (stateCache serverState)
  let enabledCommands = getEnabledCommands (view (config . commands) ctx) allCommands
      endpoint = "views/" <> viewName
  case findViewHandler endpoint enabledCommands of
    Just handler -> handler ctx mOffset mLimit
    Nothing -> throwError err404

-- | Search server implementation
searchServer :: ServerState -> Server SearchAPI
searchServer serverState query mView mOffset mLimit = do
  ctx <- liftIO $ readMVar (stateCache serverState)
  Api.Handlers.searchHandler query mView ctx mOffset mLimit

-- | Projects server implementation
projectsServer :: ServerState -> Server ProjectsAPI
projectsServer serverState =
  getProjectByPointer :<|> getProjectTasks :<|> getParentProject
  where
    getProjectByPointer filePath taskIdx = do
      ctx <- liftIO $ readMVar (stateCache serverState)
      Api.Handlers.getProjectByPointerHandler filePath taskIdx ctx

    getProjectTasks filePath taskIdx mOffset mLimit = do
      ctx <- liftIO $ readMVar (stateCache serverState)
      Api.Handlers.getProjectTasksHandler filePath taskIdx ctx mOffset mLimit

    getParentProject filePath taskIdx = do
      ctx <- liftIO $ readMVar (stateCache serverState)
      Api.Handlers.getParentProjectHandler filePath taskIdx ctx

-- | Capture server implementation
captureServer :: ServerState -> Server CaptureAPI
captureServer serverState = Api.Handlers.captureHandler (stateCache serverState)

-- | Mutation server implementation
mutationServer :: ServerState -> Server MutationAPI
mutationServer serverState =
  Api.Handlers.editTaskHandler (stateCache serverState)
    :<|> Api.Handlers.changeKeywordHandler (stateCache serverState)
    :<|> Api.Handlers.changePriorityHandler (stateCache serverState)
    :<|> Api.Handlers.addTagHandler (stateCache serverState)
    :<|> Api.Handlers.removeTagHandler (stateCache serverState)
    :<|> Api.Handlers.deleteTaskHandler (stateCache serverState)

-- | Events server implementation
eventsServer :: ServerState -> Server EventsAPI
eventsServer serverState =
  EH.getEventsHandler (stateCache serverState)
    :<|> EH.postEventsHandler (stateCache serverState)

-- | Web-only server: UI reads + capture + mutations + static UI assets.
webServer :: ServerState -> Server WebAPI
webServer serverState =
  ( healthServer "web" serverState
      :<|> viewsServer serverState
      :<|> searchServer serverState
      :<|> projectsServer serverState
      :<|> captureServer serverState
      :<|> mutationServer serverState
  )
    :<|> serveDirectoryWebApp "web/dist"

-- | Sync-only server: just the events push/pull endpoints.
syncServer :: ServerState -> Server SyncAPI
syncServer serverState =
  healthServer "sync" serverState
    :<|> eventsServer serverState

-- | Combined server: every web route plus the events sync surface.
combinedServer :: ServerState -> Server CombinedAPI
combinedServer serverState =
  ( healthServer "combined" serverState
      :<|> viewsServer serverState
      :<|> searchServer serverState
      :<|> projectsServer serverState
      :<|> captureServer serverState
      :<|> mutationServer serverState
      :<|> eventsServer serverState
  )
    :<|> serveDirectoryWebApp "web/dist"

-- | CORS policy shared by every variant.
corsMiddleware :: Application -> Application
corsMiddleware =
  cors $
    const $
      Just
        simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type"],
            corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
          }

-- | WAI Application for the web-only surface.
webApp :: ServerState -> Application
webApp st = corsMiddleware $ serve (Proxy :: Proxy WebAPI) (webServer st)

-- | WAI Application for the sync-only surface.
syncApp :: ServerState -> Application
syncApp st = corsMiddleware $ serve (Proxy :: Proxy SyncAPI) (syncServer st)

-- | WAI Application combining web + sync (back-compat for @--serve@).
combinedApp :: ServerState -> Application
combinedApp st = corsMiddleware $ serve (Proxy :: Proxy CombinedAPI) (combinedServer st)

-- | Build a 'ServerState' once for any of the run* entry points.
-- Watches org files and warns on external edits — DB is canonical, so
-- those edits will not propagate until the operator runs an explicit
-- @dwayne dbExport@ / @dwayne migrateToEvents@ depending on intent.
initServerState :: AppContext Task -> IO ServerState
initServerState initialCtx = do
  cacheVar <- newMVar initialCtx
  registry <- WS.newClientRegistry
  let files = getAllFiles (view config initialCtx)
  watcher <- FW.startWatcher files $
    putStrLn "warning: external org edit detected. DB is canonical; run 'dwayne migrateToEvents' or 'dwayne dbExport' as appropriate."
  pure $ ServerState cacheVar registry (Just watcher)

-- | Run the combined web + sync server (default for local dev).
-- Serves REST API at /api/* and static files from web/dist/ at /*.
runServer :: Int -> AppContext Task -> IO ()
runServer port initialCtx = do
  st <- initServerState initialCtx
  let wsApp =
        WaiWS.websocketsOr
          WebSockets.defaultConnectionOptions
          (WS.wsApp (stateRegistry st))
          (combinedApp st)
  putStrLn $ "Starting combined server on http://localhost:" ++ show port
  run port wsApp

-- | Run the UI-only server. No /api/events route — sync clients pointed
-- at this process will get HTTP 404.
runWebServer :: Int -> AppContext Task -> IO ()
runWebServer port initialCtx = do
  st <- initServerState initialCtx
  let wsApp =
        WaiWS.websocketsOr
          WebSockets.defaultConnectionOptions
          (WS.wsApp (stateRegistry st))
          (webApp st)
  putStrLn $ "Starting web server on http://localhost:" ++ show port
  run port wsApp

-- | Run the sync-only server. UI/Raycast clients pointed at this
-- process will get HTTP 404 for /api/views and friends.
runSyncServer :: Int -> AppContext Task -> IO ()
runSyncServer port initialCtx = do
  st <- initServerState initialCtx
  putStrLn $ "Starting sync server on http://localhost:" ++ show port
  run port (syncApp st)
