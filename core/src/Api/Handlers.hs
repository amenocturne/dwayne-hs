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
  )
where

import Api.Types (PaginatedResponse (..), ProjectTreeResponse (..), ResponseMetadata (..), TaskNode (..), TaskWithPointer (..))
import Control.Lens (preview, view)
import qualified Core.Operations as Ops
import Core.Filters (computeFilteredSortedView)
import Core.Types (FileState, TaskPointer (..), level)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Model.OrgMode (Task)
import Searcher.OrgSearcher ()
import Searcher.Searcher (matches)
import Servant (Handler, err404, throwError)
import Tui.Keybindings (todoKeywordFilter)
import Tui.Types (AppContext, fileStateLens, taskBy)
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
