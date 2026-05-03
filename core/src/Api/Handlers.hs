{-# LANGUAGE OverloadedStrings #-}

-- | Handler implementations for view endpoints.
--
-- Read handlers (views, search, projects/*) consult the CQRS read model
-- via 'Repo.TaskRepo' (a 'Repo.EventStoreRepo' wired in at server boot).
-- Mutation handlers still own the legacy in-memory 'FileState' write
-- path; Phase 2b switches them over.
--
-- The repo is kept in 'SystemConfig._taskRepo'. Read handlers fetch it
-- via 'taskRepoLens'; if the field is 'Nothing' (server bootstrapped
-- without a DB-backed repo) the handler returns HTTP 500. The
-- production 'Main.initializeAppContext' always wires a repo, so
-- 'Nothing' indicates either a misconfigured fixture or a regression in
-- bootstrap.
module Api.Handlers
  ( -- * View Handler Builders
    queryViewHandler,
    viewAllHandler,
    workQueueQueryHandler,

    -- * Search Handler
    searchHandler,

    -- * Project Handlers
    getProjectByPointerHandler,
    getProjectTasksHandler,
    getParentProjectHandler,

    -- * Project Tree Handler
    buildProjectTreeFromList,

    -- * Capture Handler
    captureHandler,

    -- * Mutation Handlers
    withMutation,
    editTaskHandler,
    changeKeywordHandler,
    changePriorityHandler,
    addTagHandler,
    removeTagHandler,
    deleteTaskHandler,
  )
where

import Api.Types (CaptureRequest (..), ChangeKeywordRequest (..), ChangePriorityRequest (..), EditTaskRequest (..), PaginatedResponse (..), ProjectTreeResponse (..), ResponseMetadata (..), TagRequest (..), TaskNode (..), TaskPointerRequest (..), TaskWithPointer (..), requestToTransform)
import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Lens (set, view, (&), (.~))
import Control.Monad.IO.Class (liftIO)
import qualified Core.Operations as Ops
import Core.Types (FileState, TaskPointer (..), level, taskIndex)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (getCurrentTime, getZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import DB.Connection (withDatabase)
import Events.Diff (diffTaskAsEvent, fullEvent)
import Events.Store (insertEvent)
import Model.OrgMode (Task (..), content, orgDayTimeFormat, orgInboxKeyword, orgProjectKeyword, plainToRichText)
import Parser.OrgParser (dateTimeParserReimplemented)
import Parser.Parser (ParserResult (..), runParser)
import qualified Repo.EventStoreRepo as Repo
import Repo.TaskRepo (Query (..), SortField (..), View (..), emptyQuery)
import qualified Repo.TaskRepo as TR
import Searcher.OrgSearcher ()
import Servant (Handler, err400, err404, err500, errBody, throwError)
import Tui.Types (AppContext, config, fileStateLens, inboxFile, taskRepoLens)
import qualified Tui.Types as TT
import Writer.OrgWriter ()

-- ---------------------------------------------------------------------------
-- Repo access
-- ---------------------------------------------------------------------------

-- | Fetch the read-model repo from the request context, or fail fast
-- with HTTP 500 when the server was bootstrapped without one. Read
-- handlers must not silently fall back to a stale in-memory projection.
requireRepo :: AppContext Task -> Handler Repo.EventStoreRepo
requireRepo ctx = case view taskRepoLens ctx of
  Just r -> pure r
  Nothing ->
    throwError $
      err500 {errBody = BSL.pack "TaskRepo not configured: server bootstrap is missing the CQRS read model wiring."}

-- | Apply optional 'offset' / 'limit' query params on top of a base
-- query. Defaults match the legacy handler: offset = 0, limit = 100.
withPagination :: Maybe Int -> Maybe Int -> Query -> Query
withPagination mOffset mLimit q =
  q
    { qOffset = Just (fromMaybe 0 mOffset),
      qLimit = Just (fromMaybe 100 mLimit)
    }

-- | Run a paginated query and assemble the wire response. The total
-- count comes from a separate 'countTasks' call so 'metadata.total'
-- reflects the unpaginated row count, matching the legacy semantics.
runPaginatedQuery :: Repo.EventStoreRepo -> Maybe Int -> Maybe Int -> Query -> Handler (PaginatedResponse TaskWithPointer)
runPaginatedQuery repo mOffset mLimit baseQuery = do
  let paged = withPagination mOffset mLimit baseQuery
  rows <- liftIO $ TR.queryTasksWithPointers repo paged
  total <- liftIO $ TR.countTasks repo baseQuery
  pure $
    PaginatedResponse
      (map (\(t, ptr) -> TaskWithPointer t ptr) rows)
      (ResponseMetadata total)

-- | Build a view handler from a 'Query' template. Pagination is
-- composed in at request time (so the same template can serve different
-- offset/limit pairs).
--
-- Each call site in 'Commands.Views' supplies a 'Query' that captures
-- its keyword filter and sort intent. The handler runs that query
-- against the read model and shapes the result into 'PaginatedResponse'.
queryViewHandler ::
  Query ->
  AppContext Task ->
  Maybe Int ->
  Maybe Int ->
  Handler (PaginatedResponse TaskWithPointer)
queryViewHandler baseQuery ctx mOffset mLimit = do
  repo <- requireRepo ctx
  runPaginatedQuery repo mOffset mLimit baseQuery

-- | Handler for "view all" (no keyword filter, no sort, TRASH excluded
-- by the repo default). Pagination defaults match the legacy handler.
viewAllHandler :: AppContext Task -> Maybe Int -> Maybe Int -> Handler (PaginatedResponse TaskWithPointer)
viewAllHandler = queryViewHandler emptyQuery

-- | Handler for the work-queue view (TODAY ∪ SOON ordered by priority,
-- then deadline). Modeled separately from 'queryViewHandler' because
-- the WHERE clause is a multi-keyword OR and the existing repo expresses
-- that as 'ViewWorkQueue' rather than 'qKeyword'.
workQueueQueryHandler ::
  AppContext Task ->
  Maybe Int ->
  Maybe Int ->
  Handler (PaginatedResponse TaskWithPointer)
workQueueQueryHandler =
  queryViewHandler (emptyQuery {qView = Just ViewWorkQueue, qSortBy = SortPriorityThenDeadline})

-- | Search handler that scans titles + descriptions via SQL @LIKE@.
-- Optionally restricts to a view (a TODO keyword, or "all"), preserving
-- the legacy behavior where the @view@ query parameter narrows the
-- candidate set before the substring filter.
searchHandler ::
  T.Text ->
  Maybe T.Text ->
  AppContext Task ->
  Maybe Int ->
  Maybe Int ->
  Handler (PaginatedResponse TaskWithPointer)
searchHandler queryText mView ctx mOffset mLimit = do
  repo <- requireRepo ctx
  let baseQuery = (searchQueryFor mView) {qSearchTerm = Just queryText}
  runPaginatedQuery repo mOffset mLimit baseQuery
  where
    searchQueryFor :: Maybe T.Text -> Query
    searchQueryFor Nothing = emptyQuery
    searchQueryFor (Just v)
      | T.toLower v == "all" = emptyQuery
      | otherwise = emptyQuery {qKeyword = Just (T.toUpper v)}

-- | Get a project by its pointer (file path and task index)
-- Returns the project task itself as a single-item paginated response
-- Throws 404 if the task doesn't exist or is not a project
getProjectByPointerHandler ::
  FilePath ->
  Int ->
  AppContext Task ->
  Handler (PaginatedResponse TaskWithPointer)
getProjectByPointerHandler filePath taskIdx ctx = do
  repo <- requireRepo ctx
  let ptr = TaskPointer filePath taskIdx
  mTask <- liftIO $ TR.getTask repo (filePath, taskIdx)
  case mTask of
    Nothing -> throwError err404
    Just task
      | _todoKeyword task == orgProjectKeyword ->
          return $ PaginatedResponse [TaskWithPointer task ptr] (ResponseMetadata 1)
      | otherwise -> throwError err404

-- | Get all tasks inside a project as a hierarchical tree structure
-- Returns the project task as the root with its subtasks nested according to org-mode hierarchy
-- No pagination is applied as the tree structure is naturally bounded by project scope
getProjectTasksHandler ::
  FilePath ->
  Int ->
  AppContext Task ->
  Maybe Int ->
  Maybe Int ->
  Handler ProjectTreeResponse
getProjectTasksHandler filePath taskIdx ctx _mOffset _mLimit = do
  repo <- requireRepo ctx
  mProjectTask <- liftIO $ TR.getTask repo (filePath, taskIdx)
  case mProjectTask of
    Nothing -> throwError err404
    Just projectTask
      | _todoKeyword projectTask /= orgProjectKeyword -> throwError err404
      | otherwise -> do
          -- Pull every (Task, TaskPointer) for the project's file in
          -- index order, including TRASH children — the legacy
          -- 'getProjectSubtasks' relied on contiguous in-file traversal,
          -- not keyword filtering, to determine subtree membership.
          fileRows <-
            liftIO $
              TR.queryTasksWithPointers
                repo
                ( emptyQuery
                    { qFilePath = Just filePath,
                      qSortBy = SortTaskIndex,
                      qIncludeTrash = True
                    }
                )
          let projectPtr = TaskPointer filePath taskIdx
          case buildProjectTreeFromList projectPtr projectTask fileRows of
            Nothing -> throwError err404
            Just tree -> return $ ProjectTreeResponse tree

-- | Get the parent project of a task
-- Returns the parent PROJECT task as a single-item response
-- Throws 404 if the task doesn't exist or has no parent project
getParentProjectHandler ::
  FilePath ->
  Int ->
  AppContext Task ->
  Handler (PaginatedResponse TaskWithPointer)
getParentProjectHandler filePath taskIdx ctx = do
  repo <- requireRepo ctx
  mTask <- liftIO $ TR.getTask repo (filePath, taskIdx)
  case mTask of
    Nothing -> throwError err404
    Just task -> do
      -- Walk the file's tasks in index order to find the closest
      -- preceding entry whose level is strictly smaller than this task's
      -- and whose keyword is PROJECT. Mirrors 'Ops.findProjectForTask'.
      fileRows <-
        liftIO $
          TR.queryTasksWithPointers
            repo
            ( emptyQuery
                { qFilePath = Just filePath,
                  qSortBy = SortTaskIndex,
                  qIncludeTrash = True
                }
            )
      case findParentProject taskIdx (_level task) fileRows of
        Nothing -> throwError err404
        Just (projectTask, projectPtr) ->
          return $ PaginatedResponse [TaskWithPointer projectTask projectPtr] (ResponseMetadata 1)

-- | Walk a list of (Task, TaskPointer) rows ordered by task_index for
-- one file and return the closest preceding PROJECT row whose level is
-- strictly smaller than 'targetLevel'. Mirrors the index-walk in
-- 'Core.Operations.findProjectForTask'.
findParentProject :: Int -> Int -> [(Task, TaskPointer)] -> Maybe (Task, TaskPointer)
findParentProject targetIdx targetLevel rows =
  let candidates =
        [ (t, p)
        | (t, p) <- rows,
          view taskIndex p < targetIdx,
          _level t < targetLevel,
          _todoKeyword t == orgProjectKeyword
        ]
   in case reverse candidates of
        (last_ : _) -> Just last_
        [] -> Nothing

-- | Build a hierarchical tree structure from a project and a list of
-- @(Task, TaskPointer)@ rows for the same file in @task_index@ order.
-- Subtasks are determined by org-mode indentation: a task at level N is
-- a child of the closest preceding task at level N-1.
--
-- The list is expected to include the project itself (it is filtered
-- out by index). Tree shape mirrors the legacy 'buildProjectTree' but
-- works off a flat list returned by the repo rather than a 'FileState'.
buildProjectTreeFromList :: TaskPointer -> Task -> [(Task, TaskPointer)] -> Maybe TaskNode
buildProjectTreeFromList projectPtr projectTask rows =
  let projectIdx = view taskIndex projectPtr
      projectLevel = view level projectTask
      -- All rows with task_index > projectIdx, but only the contiguous
      -- run with level > projectLevel — once we hit a sibling/ancestor
      -- the project has ended.
      tail_ =
        takeWhile (\(t, _) -> _level t > projectLevel) $
          dropWhile (\(_, p) -> view taskIndex p <= projectIdx) rows
      children = buildChildren projectLevel tail_
   in Just (TaskNode projectTask projectPtr children)
  where
    buildChildren :: Int -> [(Task, TaskPointer)] -> [TaskNode]
    buildChildren parentLevel tasks =
      let (directChildren, _rest) = collectDirectChildren parentLevel tasks
       in map (buildNode tasks) directChildren

    collectDirectChildren :: Int -> [(Task, TaskPointer)] -> ([(Task, TaskPointer)], [(Task, TaskPointer)])
    collectDirectChildren _ [] = ([], [])
    collectDirectChildren parentLevel ((task, ptr) : rest)
      | view level task == parentLevel + 1 =
          let (_descendants, afterDescendants) = takeDescendants (parentLevel + 1) rest
              (siblings, remaining) = collectDirectChildren parentLevel afterDescendants
           in ((task, ptr) : siblings, remaining)
      | otherwise = ([], (task, ptr) : rest)

    takeDescendants :: Int -> [(Task, TaskPointer)] -> ([(Task, TaskPointer)], [(Task, TaskPointer)])
    takeDescendants parentLevel tasks =
      span (\(task, _) -> view level task > parentLevel) tasks

    buildNode :: [(Task, TaskPointer)] -> (Task, TaskPointer) -> TaskNode
    buildNode allTasks (task, ptr) =
      let taskLevel = view level task
          taskIdx_ = view taskIndex ptr
          afterMe = dropWhile (\(_, p) -> view taskIndex p <= taskIdx_) allTasks
          (descendants, _) = takeDescendants taskLevel afterMe
          children = buildChildren taskLevel descendants
       in TaskNode task ptr children

captureHandler ::
  MVar (AppContext Task) ->
  CaptureRequest ->
  Handler TaskWithPointer
captureHandler cacheVar req = do
  now <- liftIO getZonedTime
  utcNow <- liftIO getCurrentTime
  let titleText = captureTitle req
      createdStr = T.pack $ formatTime defaultTimeLocale orgDayTimeFormat now
      (_, _, createdResult) = runParser dateTimeParserReimplemented createdStr
      createdTime = case createdResult of
        ParserSuccess t -> Just t
        _ -> Nothing
      newTask =
        Task
          { _level = 1,
            _todoKeyword = orgInboxKeyword,
            _priority = Nothing,
            _title = plainToRichText titleText,
            _tags = S.empty,
            _scheduled = Nothing,
            _deadline = Nothing,
            _createdProp = createdTime,
            _closed = Nothing,
            _properties = [],
            _description = plainToRichText ""
          }
  result <- liftIO $ modifyMVar cacheVar $ \ctx -> do
    let fp = view (config . inboxFile) ctx
        fs = view fileStateLens ctx
        dbFile = TT._database (view config ctx)
    case M.lookup fp fs of
      Nothing -> return (ctx, Left "Inbox file not found")
      Just (ParserFailure _) -> return (ctx, Left "Inbox file has parse errors")
      Just (ParserSuccess tf) -> do
        let idx = V.length (view content tf)
            updatedTf = tf & content .~ V.snoc (view content tf) newTask
            newCtx = set fileStateLens (M.insert fp (ParserSuccess updatedTf) fs) ctx
            ptr = TaskPointer fp idx
        withDatabase dbFile $ \conn ->
          insertEvent conn (fullEvent fp idx utcNow newTask)
        return (newCtx, Right (TaskWithPointer newTask ptr))
  case result of
    Left err -> throwError $ err400 {errBody = BSL.pack err}
    Right twp -> return twp

withMutation ::
  MVar (AppContext Task) ->
  TaskPointer ->
  (TaskPointer -> FileState Task -> Either String (FileState Task)) ->
  Handler TaskWithPointer
withMutation cacheVar ptr operation = do
  utcNow <- liftIO getCurrentTime
  result <- liftIO $ modifyMVar cacheVar $ \ctx -> do
    let fs = view fileStateLens ctx
        dbFile = TT._database (view config ctx)
    case Ops.getTask ptr fs of
      Nothing -> return (ctx, Left ("Task not found at: " ++ show ptr))
      Just oldTask ->
        case operation ptr fs of
          Left err -> return (ctx, Left err)
          Right newFs -> do
            let newCtx = set fileStateLens newFs ctx
            case Ops.getTask ptr newFs of
              Nothing -> return (ctx, Left "Task not found after update")
              Just task -> do
                withDatabase dbFile $ \conn ->
                  insertEvent conn (diffTaskAsEvent (_file ptr) (_taskIndex ptr) utcNow oldTask task)
                return (newCtx, Right (TaskWithPointer task ptr))
  case result of
    Left err -> throwError $ err400 {errBody = BSL.pack err}
    Right twp -> return twp

editTaskHandler :: MVar (AppContext Task) -> EditTaskRequest -> Handler TaskWithPointer
editTaskHandler cacheVar req = do
  let ptr = TaskPointer (etrFile req) (etrTaskIndex req)
      transform = requestToTransform req
  utcNow <- liftIO getCurrentTime
  result <- liftIO $ modifyMVar cacheVar $ \ctx -> do
    let fs = view fileStateLens ctx
        dbFile = TT._database (view config ctx)
    case Ops.getTask ptr fs of
      Nothing -> return (ctx, Left ("Task not found at: " ++ show ptr))
      Just oldTask ->
        case Ops.editTask ptr transform fs of
          Left err -> return (ctx, Left err)
          Right (newFs, updatedTask) -> do
            let newCtx = set fileStateLens newFs ctx
            withDatabase dbFile $ \conn ->
              insertEvent conn (diffTaskAsEvent (etrFile req) (etrTaskIndex req) utcNow oldTask updatedTask)
            return (newCtx, Right (TaskWithPointer updatedTask ptr))
  case result of
    Left err -> throwError $ err400 {errBody = BSL.pack err}
    Right twp -> return twp

changeKeywordHandler :: MVar (AppContext Task) -> ChangeKeywordRequest -> Handler TaskWithPointer
changeKeywordHandler cacheVar req =
  withMutation cacheVar (TaskPointer (ckrFile req) (ckrTaskIndex req)) (Ops.changeTodoKeyword (ckrKeyword req))

changePriorityHandler :: MVar (AppContext Task) -> ChangePriorityRequest -> Handler TaskWithPointer
changePriorityHandler cacheVar req =
  withMutation cacheVar (TaskPointer (cprFile req) (cprTaskIndex req)) (Ops.changeTaskPriority (cprPriority req))

addTagHandler :: MVar (AppContext Task) -> TagRequest -> Handler TaskWithPointer
addTagHandler cacheVar req =
  withMutation cacheVar (TaskPointer (trFile req) (trTaskIndex req)) (Ops.addTaskTag (trTag req))

removeTagHandler :: MVar (AppContext Task) -> TagRequest -> Handler TaskWithPointer
removeTagHandler cacheVar req =
  withMutation cacheVar (TaskPointer (trFile req) (trTaskIndex req)) (Ops.deleteTaskTag (trTag req))

deleteTaskHandler :: MVar (AppContext Task) -> TaskPointerRequest -> Handler TaskWithPointer
deleteTaskHandler cacheVar req =
  withMutation cacheVar (TaskPointer (tprFile req) (tprTaskIndex req)) Ops.deleteTask
