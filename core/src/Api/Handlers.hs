{-# LANGUAGE OverloadedStrings #-}

-- | Handler implementations for view endpoints
module Api.Handlers
  ( -- * View Handler Builders
    makeViewHandler,
    makeKeywordViewHandler,
    viewAllHandler,

    -- * Search Handler
    searchHandler,

    -- * Project Handlers
    getProjectByPointerHandler,
    getProjectTasksHandler,
    getParentProjectHandler,

    -- * Project Tree Handler
    buildProjectTree,

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
import Control.Lens (preview, set, view, (&), (.~))
import Control.Monad.IO.Class (liftIO)
import Core.Filters (computeFilteredSortedView)
import qualified Core.Operations as Ops
import Core.Types (FileState, TaskPointer (..), TaskStoreOps (..), level)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (getZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import Model.OrgMode (Task (..), content, orgDayTimeFormat, orgInboxKeyword, plainToRichText)
import Parser.OrgParser (dateTimeParserReimplemented)
import Parser.Parser (ParserResult (..), runParser)
import Searcher.OrgSearcher ()
import Searcher.Searcher (matches)
import Servant (Handler, err400, err404, errBody, throwError)
import Tui.Keybindings (todoKeywordFilter)
import Tui.Types (AppContext, config, fileStateLens, inboxFile, system, taskBy, taskStoreOps)
import Writer.OrgWriter ()

-- | Generic view handler builder
-- Takes filter predicates and sorter, returns a handler function
-- Applies pagination using offset (default 0) and limit (default 100) parameters
makeViewHandler ::
  [Task -> Bool] ->
  (Task -> Task -> Ordering) ->
  (AppContext Task -> Maybe Int -> Maybe Int -> Handler (PaginatedResponse TaskWithPointer))
makeViewHandler filters sorter = \ctx mOffset mLimit ->
  let fs = view fileStateLens ctx
      allResults = computeFilteredSortedView filters sorter fs
      totalCount = V.length allResults
      offset = fromMaybe 0 mOffset
      limit = fromMaybe 100 mLimit
      paginated = V.take limit $ V.drop offset allResults
      taskList = V.toList paginated
      taskPointers = map (uncurry TaskWithPointer) taskList
   in return $ PaginatedResponse taskPointers (ResponseMetadata totalCount)

-- | Specialized handler builder for TODO keyword filtering
-- Takes a keyword to filter by (e.g., "INBOX") and optional sorter
-- Applies pagination using offset (default 0) and limit (default 100) parameters
makeKeywordViewHandler ::
  T.Text ->
  Maybe (Task -> Task -> Ordering) ->
  (AppContext Task -> Maybe Int -> Maybe Int -> Handler (PaginatedResponse TaskWithPointer))
makeKeywordViewHandler keyword maybeSorter =
  let filters = [todoKeywordFilter keyword]
      sorter = case maybeSorter of
        Nothing -> \_ _ -> EQ
        Just s -> s
   in makeViewHandler filters sorter

-- | Handler for "view all" (no filtering)
-- Applies pagination using offset (default 0) and limit (default 100) parameters
viewAllHandler :: AppContext Task -> Maybe Int -> Maybe Int -> Handler (PaginatedResponse TaskWithPointer)
viewAllHandler = makeViewHandler [] (\_ _ -> EQ)

-- | Search handler that searches across all tasks using the Searcher typeclass
-- Optionally filters by view (TODO keyword) before searching
-- Applies pagination using offset (default 0) and limit (default 100) parameters
searchHandler ::
  T.Text ->
  Maybe T.Text ->
  AppContext Task ->
  Maybe Int ->
  Maybe Int ->
  Handler (PaginatedResponse TaskWithPointer)
searchHandler query mView ctx mOffset mLimit = do
  let fs = view fileStateLens ctx
      allTasks = computeFilteredSortedView [] (\_ _ -> EQ) fs
      viewFiltered = case mView of
        Nothing -> allTasks
        Just viewName -> V.filter (\(task, _) -> matchesView viewName task) allTasks
      searchFiltered = V.filter (\(task, _) -> matches query task) viewFiltered
      totalCount = V.length searchFiltered
      offset = fromMaybe 0 mOffset
      limit = fromMaybe 100 mLimit
      paginated = V.toList $ V.take limit $ V.drop offset searchFiltered
      taskPointers = map (\(task, ptr) -> TaskWithPointer task ptr) paginated
  return $ PaginatedResponse taskPointers (ResponseMetadata totalCount)
  where
    matchesView :: T.Text -> Task -> Bool
    matchesView "all" _ = True
    matchesView viewName task = todoKeywordFilter (T.toUpper viewName) task

-- | Get a project by its pointer (file path and task index)
-- Returns the project task itself as a single-item paginated response
-- Throws 404 if the task doesn't exist or is not a project
getProjectByPointerHandler ::
  FilePath ->
  Int ->
  AppContext Task ->
  Handler (PaginatedResponse TaskWithPointer)
getProjectByPointerHandler filePath taskIdx ctx = do
  let fs = view fileStateLens ctx
      ptr = TaskPointer filePath taskIdx
      maybeTask = Ops.getTask ptr fs
  case maybeTask of
    Nothing -> throwError err404
    Just task
      | Ops.isProjectTask task ->
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
  let fs = view fileStateLens ctx
      ptr = TaskPointer filePath taskIdx
      maybeProjectTask = Ops.getTask ptr fs
  case maybeProjectTask of
    Nothing -> throwError err404
    Just projectTask
      | not (Ops.isProjectTask projectTask) -> throwError err404
      | otherwise ->
          case buildProjectTree ptr fs of
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
  let fs = view fileStateLens ctx
      ptr = TaskPointer filePath taskIdx
      maybeTask = Ops.getTask ptr fs
  case maybeTask of
    Nothing -> throwError err404
    Just _ ->
      case Ops.findProjectForTask ptr fs of
        Nothing -> throwError err404
        Just projectPtr ->
          case Ops.getTask projectPtr fs of
            Nothing -> throwError err404
            Just projectTask ->
              return $ PaginatedResponse [TaskWithPointer projectTask projectPtr] (ResponseMetadata 1)

-- | Build a hierarchical tree structure from a project and its subtasks
-- Groups subtasks by their parent based on org-mode indentation rules:
-- - A task at level N is a child of the closest preceding task at level N-1
-- - The tree is built recursively to support arbitrary nesting depth
buildProjectTree :: TaskPointer -> FileState Task -> Maybe TaskNode
buildProjectTree projectPtr fs = do
  projectTask <- Ops.getTask projectPtr fs
  let subtaskPtrs = Ops.getProjectSubtasks projectPtr fs
      subtaskPairs = [(task, ptr) | ptr <- subtaskPtrs, Just task <- [Ops.getTask ptr fs]]
      projectLevel = view level projectTask
      children = buildChildren projectLevel subtaskPairs
  return $ TaskNode projectTask projectPtr children
  where
    -- Build direct children for a given parent level
    -- Takes all remaining tasks and groups them by their immediate parent
    buildChildren :: Int -> [(Task, TaskPointer)] -> [TaskNode]
    buildChildren parentLevel tasks =
      let (directChildren, rest) = collectDirectChildren parentLevel tasks
       in map (buildNode parentLevel rest) directChildren

    -- Collect tasks that are direct children (level = parentLevel + 1)
    -- Returns (direct children, remaining tasks after those children and their descendants)
    collectDirectChildren :: Int -> [(Task, TaskPointer)] -> ([(Task, TaskPointer)], [(Task, TaskPointer)])
    collectDirectChildren _ [] = ([], [])
    collectDirectChildren parentLevel ((task, ptr) : rest)
      | view level task == parentLevel + 1 =
          let (descendants, afterDescendants) = takeDescendants (parentLevel + 1) rest
           in let (siblings, remaining) = collectDirectChildren parentLevel afterDescendants
               in ((task, ptr) : siblings, remaining)
      | otherwise = ([], (task, ptr) : rest)

    -- Take all descendants of a task (tasks with level > parentLevel)
    -- Stops when encountering a task at same or lower level
    takeDescendants :: Int -> [(Task, TaskPointer)] -> ([(Task, TaskPointer)], [(Task, TaskPointer)])
    takeDescendants parentLevel tasks =
      span (\(task, _) -> view level task > parentLevel) tasks

    -- Build a node with its children from remaining tasks
    buildNode :: Int -> [(Task, TaskPointer)] -> (Task, TaskPointer) -> TaskNode
    buildNode parentLevel allRest (task, ptr) =
      let taskLevel = view level task
          (descendants, _) = takeDescendants taskLevel allRest
          children = buildChildren taskLevel descendants
       in TaskNode task ptr children

captureHandler ::
  MVar (AppContext Task) ->
  CaptureRequest ->
  Handler TaskWithPointer
captureHandler cacheVar req = do
  now <- liftIO getZonedTime
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
    case M.lookup fp fs of
      Nothing -> return (ctx, Left "Inbox file not found")
      Just (ParserFailure _) -> return (ctx, Left "Inbox file has parse errors")
      Just (ParserSuccess tf) -> do
        let idx = V.length (view content tf)
            updatedTf = tf & content .~ V.snoc (view content tf) newTask
            newCtx = set fileStateLens (M.insert fp (ParserSuccess updatedTf) fs) ctx
            ptr = TaskPointer fp idx
        case view (system . taskStoreOps) ctx of
          Just ops -> storeSave ops (view fileStateLens newCtx)
          Nothing -> return ()
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
  result <- liftIO $ modifyMVar cacheVar $ \ctx -> do
    let fs = view fileStateLens ctx
    case operation ptr fs of
      Left err -> return (ctx, Left err)
      Right newFs -> do
        let newCtx = set fileStateLens newFs ctx
        case view (system . taskStoreOps) ctx of
          Just ops -> storeSave ops newFs
          Nothing -> return ()
        case Ops.getTask ptr newFs of
          Nothing -> return (ctx, Left "Task not found after update")
          Just task -> return (newCtx, Right (TaskWithPointer task ptr))
  case result of
    Left err -> throwError $ err400 {errBody = BSL.pack err}
    Right twp -> return twp

editTaskHandler :: MVar (AppContext Task) -> EditTaskRequest -> Handler TaskWithPointer
editTaskHandler cacheVar req = do
  let ptr = TaskPointer (etrFile req) (etrTaskIndex req)
      transform = requestToTransform req
  result <- liftIO $ modifyMVar cacheVar $ \ctx -> do
    let fs = view fileStateLens ctx
    case Ops.editTask ptr transform fs of
      Left err -> return (ctx, Left err)
      Right (newFs, updatedTask) -> do
        let newCtx = set fileStateLens newFs ctx
        case view (system . taskStoreOps) ctx of
          Just ops -> storeSave ops newFs
          Nothing -> return ()
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
