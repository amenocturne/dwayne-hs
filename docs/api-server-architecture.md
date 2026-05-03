# API Server Architecture

## Module Overview

The API server is built using Servant, providing both REST endpoints and static file serving in a single application.

## Component Stack

```
runServer (Int -> AppContext Task -> IO ())
    ↓
Network.Wai.Handler.Warp.run
    ↓
Servant Application (WAI)
    ↓
    ├── /api/* → REST API Endpoints
    │       ↓
    │   ViewsAPI (11 endpoints)
    │       ↓
    │   viewsServer (dynamic handler resolution)
    │       ↓
    │   Commands.Registry.getEnabledCommands
    │       ↓
    │   Filter by config.commands field
    │       ↓
    │   Extract apiHandler from Command.cmdApi
    │       ↓
    │   Api.Handlers (makeKeywordViewHandler, viewAllHandler)
    │       ↓
    │   Core.Filters.computeFilteredSortedView
    │
    └── /* → Static Files
            ↓
        serveDirectoryWebApp "web/dist"
```

## Type Flow

```haskell
-- Top level
runServer :: Int -> AppContext Task -> IO ()

-- API type definition
type API = "api" :> ViewsAPI :<|> Raw

-- Views API (11 endpoints)
type ViewsAPI = 
     "views" :> "all" :> Get '[JSON] [TaskWithPointer]
:<|> "views" :> "inbox" :> Get '[JSON] [TaskWithPointer]
:<|> ... (9 more endpoints)

-- Server implementation
viewsServer :: AppContext Task -> Server ViewsAPI
viewsServer ctx = handler1 :<|> handler2 :<|> ... :<|> handler11
  where
    handler1 = handlerFor "views/all"
    handler2 = handlerFor "views/inbox"
    ...
```

## Command Resolution Flow

1. **Get Enabled Commands**
   ```haskell
   enabledCommands = getEnabledCommands (config ^. commands)
   ```

2. **Find Matching Handler**
   ```haskell
   findHandler :: T.Text -> [Command Task] 
               -> Maybe (AppContext Task -> Handler [TaskWithPointer])
   ```

3. **Execute Handler**
   ```haskell
   case findHandler "views/inbox" enabledCommands of
     Just handler -> handler ctx
     Nothing -> return []  -- Disabled command
   ```

## Example Request Flow

```
HTTP GET /api/views/inbox
    ↓
Servant routes to inbox endpoint
    ↓
viewsServer calls handlerFor "views/inbox"
    ↓
Finds viewInboxCommand in enabledCommands
    ↓
Extracts apiHandler from cmdApi field
    ↓
makeKeywordViewHandler "INBOX" (Just sortByCreatedDesc)
    ↓
makeViewHandler [todoKeywordFilter "INBOX"] sortByCreatedDesc
    ↓
computeFilteredSortedView filters sorter fileState
    ↓
Returns Vector (Task, TaskPointer)
    ↓
Convert to [TaskWithPointer]
    ↓
Servant serializes to JSON
    ↓
HTTP Response 200 OK
```

## Configuration Integration

The server respects the config file's `commands` field:

```yaml
# config.yml
commands:
  viewInbox: true      # Endpoint enabled
  viewTrash: false     # Endpoint disabled (returns [])
  # Not listed = enabled by default
```

## Error Handling Strategy

- **Disabled Command**: Returns empty list `[]` (graceful degradation)
- **Missing Handler**: Returns empty list `[]`
- **Parser Errors**: Handled at AppContext level (not server's concern)
- **HTTP Errors**: Servant handles 404, 500 automatically

## Static File Serving

```
GET /                → web/dist/index.html
GET /assets/main.js  → web/dist/assets/main.js
GET /favicon.ico     → web/dist/favicon.ico
```

Uses `serveDirectoryWebApp` which:
- Serves `index.html` for directory requests
- Sets proper MIME types
- Handles 404 for missing files

## Future Enhancements

1. **Health Check Endpoint**: `GET /api/health`
2. **Logging**: Request/response logging
3. **CORS**: Enable for web frontend
4. **Path Resolution**: Find `web/dist` relative to executable
5. **Dynamic Port**: Read from config instead of hardcoded
6. **Graceful Shutdown**: Handle SIGTERM properly
7. **TLS/HTTPS**: For production deployments

## Testing Strategy

- **Unit Tests**: Test handler resolution logic
- **Integration Tests**: Test actual HTTP requests
- **Property Tests**: Verify all enabled commands have handlers
- **Manual Tests**: Use `curl` or browser to verify endpoints

## Dependencies

- `servant-server >= 0.20`: Core Servant functionality
- `warp >= 3.3`: Production HTTP server
- Existing: `aeson`, `text`, `lens`, `containers`
