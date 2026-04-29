{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | HTTP server for the REST API and static file serving
module Api.Server
  ( runServer,
  )
where

import qualified Api.Handlers
import Api.Types (ApiBinding (..), ApiMethod (..), CaptureRequest, ChangeKeywordRequest, ChangePriorityRequest, EditTaskRequest, PaginatedResponse (..), ProjectTreeResponse, ResponseMetadata (..), TagRequest, TaskPointerRequest, TaskWithPointer)
import qualified Api.WebSocket as WS
import Commands.Command (Command (..), getEnabledCommands)
import Commands.Registry (allCommands)
import Control.Concurrent.MVar
import Control.Lens (view)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
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
import Tui.Types (AppContext, commands, config, getAllFiles)

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

-- | Combined API: /api/* for REST endpoints, /* for static files
type API = "api" :> (ViewsAPI :<|> SearchAPI :<|> ProjectsAPI :<|> CaptureAPI :<|> MutationAPI) :<|> Raw

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

-- | Main API server combining views and static file serving
apiServer :: ServerState -> Server API
apiServer serverState =
  (viewsServer serverState :<|> searchServer serverState :<|> projectsServer serverState :<|> captureServer serverState :<|> mutationServer serverState)
    :<|> serveDirectoryWebApp "web/dist" -- Assumes run from project root

-- | Convert the API server to a WAI Application with CORS support
app :: ServerState -> Application
app serverState = corsMiddleware $ serve (Proxy :: Proxy API) (apiServer serverState)
  where
    corsMiddleware =
      cors $
        const $
          Just
            simpleCorsResourcePolicy
              { corsRequestHeaders = ["Content-Type"],
                corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
              }

-- | Run the web server on the specified port
-- Serves REST API at /api/* and static files from web/dist/ at /*
-- Watches org files and warns on external edits (DB is canonical, so
-- those edits will not propagate until the user runs `dwayne dbImport`).
runServer :: Int -> AppContext Task -> IO ()
runServer port initialCtx = do
  -- Create server state with cached context
  cacheVar <- newMVar initialCtx
  registry <- WS.newClientRegistry

  -- Watch org files for external edits. DB is canonical now, so we don't
  -- reload from disk — we just warn the operator that the change won't
  -- propagate without an explicit dbImport.
  let files = getAllFiles (view config initialCtx)
  watcher <- FW.startWatcher files $
    putStrLn "warning: external org edit detected. DB is canonical; run `dwayne dbImport` to sync if intended."

  let serverState = ServerState cacheVar registry (Just watcher)

  -- Create WAI application with WebSocket support
  let wsApp =
        WaiWS.websocketsOr
          WebSockets.defaultConnectionOptions
          (WS.wsApp registry)
          (app serverState)

  putStrLn $ "Starting server on http://localhost:" ++ show port
  run port wsApp
