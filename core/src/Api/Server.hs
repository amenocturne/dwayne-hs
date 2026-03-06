{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | HTTP server for the REST API and static file serving
module Api.Server
  ( runServer,
  )
where

import qualified Api.Handlers
import Api.Types (ApiBinding (..), ApiMethod (..), PaginatedResponse (..), ProjectTreeResponse, ResponseMetadata (..), TaskWithPointer)
import qualified Api.WebSocket as WS
import Commands.Command (Command (..), getEnabledCommands)
import Commands.Registry (allCommands)
import Control.Concurrent.MVar
import Control.Lens (set, view)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified FileWatcher as FW
import Model.OrgMode (Task, TaskFile)
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Network.WebSockets as WebSockets
import Parser.Parser (runParser)
import Servant
  ( Application,
    Capture,
    Get,
    Handler,
    JSON,
    Proxy (..),
    QueryParam,
    QueryParam',
    Raw,
    Required,
    Server,
    err404,
    serve,
    serveDirectoryWebApp,
    throwError,
    type (:<|>) (..),
    type (:>),
  )
import TextUtils (readFileExample)
import Tui.Types (AppContext, commands, config, fileParser, fileStateLens, getAllFiles, system)

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

-- | Combined API: /api/* for REST endpoints, /* for static files
type API = "api" :> (ViewsAPI :<|> SearchAPI :<|> ProjectsAPI) :<|> Raw

-- | Server state containing cached context and WebSocket registry
data ServerState = ServerState
  { stateCache :: MVar (AppContext Task),
    stateRegistry :: WS.ClientRegistry,
    stateWatcher :: Maybe FW.WatcherHandle
  }

-- | Reload all org files from disk and update the context
-- This ensures the API always returns the most current data
reloadContext :: AppContext Task -> IO (AppContext Task)
reloadContext ctx = do
  let fps = getAllFiles (view config ctx)
      parser = view (system . fileParser) ctx
  results <- forM fps $ \fp -> do
    txt <- readFileExample fp
    let (_, _, parsed) = runParser parser txt
    return (fp, parsed)
  let newFileState = M.fromList results
  return $ set fileStateLens newFileState ctx

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

-- | Main API server combining views and static file serving
apiServer :: ServerState -> Server API
apiServer serverState =
  (viewsServer serverState :<|> searchServer serverState :<|> projectsServer serverState)
    :<|> serveDirectoryWebApp "web/dist" -- Assumes run from project root

-- | Convert the API server to a WAI Application with CORS support
app :: ServerState -> Application
app serverState = simpleCors $ serve (Proxy :: Proxy API) (apiServer serverState)

-- | Run the web server on the specified port
-- Serves REST API at /api/* and static files from web/dist/ at /*
-- Starts file watcher to automatically reload and broadcast changes
runServer :: Int -> AppContext Task -> IO ()
runServer port initialCtx = do
  -- Create server state with cached context
  cacheVar <- newMVar initialCtx
  registry <- WS.newClientRegistry

  -- Start file watcher that invalidates cache and broadcasts to clients
  let files = getAllFiles (view config initialCtx)
  watcher <- FW.startWatcher files $ do
    putStrLn "Files changed, reloading..."
    newCtx <- reloadContext initialCtx
    modifyMVar_ cacheVar (const $ return newCtx)
    WS.broadcast registry "reload"

  let serverState = ServerState cacheVar registry (Just watcher)

  -- Create WAI application with WebSocket support
  let wsApp =
        WaiWS.websocketsOr
          WebSockets.defaultConnectionOptions
          (WS.wsApp registry)
          (app serverState)

  putStrLn $ "Starting server on http://localhost:" ++ show port
  putStrLn "  API endpoints:"
  putStrLn "    - Views: http://localhost:8080/api/views/*"
  putStrLn "    - Search: http://localhost:8080/api/search?query=<text>"
  putStrLn "    - Projects (by pointer): http://localhost:8080/api/projects/by-pointer?file=<path>&taskIndex=<idx>"
  putStrLn "    - Projects (tree): http://localhost:8080/api/projects/tasks?file=<path>&taskIndex=<idx>"
  putStrLn "    - Projects (parent): http://localhost:8080/api/projects/parent?file=<path>&taskIndex=<idx>"
  putStrLn "  WebSocket: ws://localhost:8080/ws"
  putStrLn "  Static files: http://localhost:8080/"
  run port wsApp
