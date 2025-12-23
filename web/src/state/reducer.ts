/**
 * Pure Reducer
 *
 * update :: (AppState, Action) -> (AppState, Effect)
 *
 * The heart of the Elm architecture. Takes current state and an action,
 * returns new state and an effect to execute.
 *
 * Must be pure - no side effects, no mutations, no I/O.
 *
 * Phase 4: Enhanced with exhaustiveness checking using assertNever.
 */

import type { AppState } from "../types/state.js";
import { hasMoreTasks } from "../types/state.js";
import type { Action } from "./actions.js";
import type { Effect } from "./effects.js";
import type { TaskWithPointer } from "../types/domain.js";
import { assertNever } from "../types/utils.js";

/**
 * Pure state update function.
 * Returns tuple of [newState, effect].
 */
export function update(state: AppState, action: Action): readonly [AppState, Effect] {
  switch (action.type) {
    case 'ViewChanged':
      return [
        {
          ...state,
          taskList: {
            ...state.taskList,
            loading: true,
            tasks: [],
            offset: 0,
            loadingMore: false,
            pagesLoaded: 0,
          },
          view: {
            ...state.view,
            currentView: action.view,
            searchQuery: '',
            projectPointer: null,
          },
          carousel: {
            rotation: 0,
            targetRotation: 0,
          },
          error: null,
        },
        { type: 'FetchTasks', view: action.view, offset: 0, limit: 100 }
      ];

    case 'SearchQueryChanged':
      return [
        {
          ...state,
          view: { ...state.view, searchQuery: action.query },
        },
        { type: 'None' }
      ];

    case 'SearchCleared':
      if (state.view.projectPointer) {
        return [
          {
            ...state,
            view: { ...state.view, searchQuery: '' },
            taskList: { ...state.taskList, loading: true },
            carousel: { rotation: 0, targetRotation: 0 },
            error: null,
          },
          { type: 'FetchProjectTree', pointer: state.view.projectPointer, requestId: state.detail.projectTreeRequestId + 1, updateTaskList: true }
        ];
      } else {
        return [
          {
            ...state,
            view: { ...state.view, searchQuery: '' },
            taskList: {
              ...state.taskList,
              loading: true,
              tasks: [],
              offset: 0,
              pagesLoaded: 0,
            },
            carousel: { rotation: 0, targetRotation: 0 },
            error: null,
          },
          { type: 'FetchTasks', view: state.view.currentView, offset: 0, limit: 100 }
        ];
      }

    case 'TasksLoadStarted':
      return [
        {
          ...state,
          taskList: {
            ...state.taskList,
            loading: true,
            tasks: [],
            offset: 0,
            loadingMore: false,
          },
          view: { ...state.view, currentView: action.view },
          error: null,
        },
        { type: 'None' }
      ];

    case 'TasksLoaded': {
      const receivedFull = action.tasks.length === 100;
      return [
        {
          ...state,
          taskList: {
            ...state.taskList,
            tasks: action.tasks,
            loading: false,
            offset: action.offset,
            totalCount: action.total,
            pagesLoaded: 1,
            loadingMore: receivedFull,
          },
          error: null,
        },
        receivedFull
          ? { type: 'LoadMoreTasks', view: state.view.currentView, offset: action.offset, limit: 100 }
          : { type: 'None' }
      ];
    }

    case 'TasksLoadFailed':
      return [
        {
          ...state,
          taskList: { ...state.taskList, loading: false },
          error: action.error,
        },
        { type: 'None' }
      ];

    case 'LoadMoreStarted':
      if (state.taskList.loadingMore || !hasMoreTasks(state) || state.taskList.loading) {
        return [state, { type: 'None' }];
      }
      return [
        {
          ...state,
          taskList: { ...state.taskList, loadingMore: true },
        },
        { type: 'LoadMoreTasks', view: state.view.currentView, offset: state.taskList.offset, limit: 100 }
      ];

    case 'LoadMoreCompleted':
      return [
        {
          ...state,
          taskList: {
            ...state.taskList,
            tasks: [...state.taskList.tasks, ...action.tasks] as ReadonlyArray<TaskWithPointer>,
            offset: state.taskList.offset + action.tasks.length,
            loadingMore: false,
            totalCount: action.total,
            pagesLoaded: state.taskList.pagesLoaded + 1,
          },
        },
        { type: 'None' }
      ];

    case 'LoadMoreFailed':
      return [
        {
          ...state,
          taskList: { ...state.taskList, loadingMore: false },
          error: action.error,
        },
        { type: 'None' }
      ];

    case 'TaskClicked': {
      const isProject = action.task.task.todoKeyword === "PROJECT";

      return [
        {
          ...state,
          detail: {
            ...state.detail,
            selectedTask: action.task,
            projectTree: null,
            loadingProjectTree: isProject,
            parentProject: null,
            loadingParentProject: !isProject,
            projectTreeRequestId: isProject ? state.detail.projectTreeRequestId + 1 : state.detail.projectTreeRequestId,
            parentProjectRequestId: !isProject ? state.detail.parentProjectRequestId + 1 : state.detail.parentProjectRequestId,
          },
        },
        {
          type: 'Batch',
          effects: isProject
            ? [{
                type: 'FetchProjectTree',
                pointer: action.task.pointer,
                requestId: state.detail.projectTreeRequestId + 1,
                updateTaskList: false  // Only update detail card, not carousel
              }]
            : [{
                type: 'FetchParentProject',
                pointer: action.task.pointer,
                requestId: state.detail.parentProjectRequestId + 1
              }]
        }
      ];
    }

    case 'DetailCardClosed':
      return [
        {
          ...state,
          detail: { ...state.detail, selectedTask: null },
        },
        { type: 'None' }
      ];

    case 'ProjectTreeLoadStarted':
      return [
        {
          ...state,
          detail: { ...state.detail, loadingProjectTree: true },
        },
        { type: 'None' }
      ];

    case 'ProjectTreeLoaded':
      if (action.requestId !== state.detail.projectTreeRequestId) {
        return [state, { type: 'None' }];
      }
      return [
        {
          ...state,
          detail: {
            ...state.detail,
            projectTree: action.tree,
            loadingProjectTree: false,
          },
        },
        { type: 'None' }
      ];

    case 'ProjectTreeLoadFailed':
      if (action.requestId !== state.detail.projectTreeRequestId) {
        return [state, { type: 'None' }];
      }
      return [
        {
          ...state,
          detail: {
            ...state.detail,
            projectTree: null,
            loadingProjectTree: false,
          },
        },
        { type: 'None' }
      ];

    case 'ParentProjectLoadStarted':
      return [
        {
          ...state,
          detail: { ...state.detail, loadingParentProject: true },
        },
        { type: 'None' }
      ];

    case 'ParentProjectLoaded':
      if (action.requestId !== state.detail.parentProjectRequestId) {
        return [state, { type: 'None' }];
      }
      return [
        {
          ...state,
          detail: {
            ...state.detail,
            parentProject: action.project,
            loadingParentProject: false,
          },
        },
        { type: 'None' }
      ];

    case 'ParentProjectLoadFailed':
      if (action.requestId !== state.detail.parentProjectRequestId) {
        return [state, { type: 'None' }];
      }
      return [
        {
          ...state,
          detail: {
            ...state.detail,
            parentProject: null,
            loadingParentProject: false,
          },
        },
        { type: 'None' }
      ];

    case 'WebSocketReloadReceived':
      if (state.view.searchQuery.trim() !== "") {
        return [
          {
            ...state,
            taskList: { ...state.taskList, loading: true },
            error: null,
          },
          {
            type: 'Batch',
            effects: [
              { type: 'ShowToast', message: 'Files updated, reloading...' },
              state.view.projectPointer
                ? { type: 'SearchProjectLocally', query: state.view.searchQuery, projectPointer: state.view.projectPointer }
                : {
                    type: 'SearchTasks',
                    query: state.view.searchQuery,
                    view: state.view.currentView === "all" ? null : state.view.currentView,
                    offset: 0,
                    limit: 100
                  }
            ]
          }
        ];
      } else {
        return [
          {
            ...state,
            taskList: { ...state.taskList, loading: true },
            error: null,
          },
          {
            type: 'Batch',
            effects: [
              { type: 'ShowToast', message: 'Files updated, reloading...' },
              { type: 'FetchTasks', view: state.view.currentView, offset: 0, limit: 100 }
            ]
          }
        ];
      }

    case 'ProjectViewRequested':
      return [
        {
          ...state,
          taskList: {
            ...state.taskList,
            loading: true,
            tasks: [],
            offset: 0,
          },
          view: { ...state.view, projectPointer: action.pointer },
          detail: {
            ...state.detail,
            selectedTask: null,
            projectTreeRequestId: state.detail.projectTreeRequestId + 1,
          },
          carousel: {
            rotation: 0,
            targetRotation: 0,
          },
          error: null,
        },
        {
          type: 'FetchProjectTree',
          pointer: action.pointer,
          requestId: state.detail.projectTreeRequestId + 1,
          updateTaskList: true  // Update both detail card AND carousel
        }
      ];

    case 'ProjectTasksLoaded':
      return [
        {
          ...state,
          taskList: {
            ...state.taskList,
            tasks: action.tasks,
            loading: false,
            totalCount: action.total,
          },
          error: null,
        },
        { type: 'None' }
      ];

    case 'ProjectTasksLoadFailed':
      return [
        {
          ...state,
          taskList: { ...state.taskList, loading: false },
          error: action.error,
        },
        { type: 'None' }
      ];

    case 'BackToViewRequested':
      return [
        {
          ...state,
          taskList: {
            ...state.taskList,
            loading: true,
            tasks: [],
            offset: 0,
            pagesLoaded: 0,
          },
          view: { ...state.view, projectPointer: null },
          carousel: {
            rotation: 0,
            targetRotation: 0,
          },
          error: null,
        },
        { type: 'FetchTasks', view: state.view.currentView, offset: 0, limit: 100 }
      ];

    case 'GlobalSearchStarted':
      if (state.view.projectPointer) {
        return [
          {
            ...state,
            taskList: { ...state.taskList, loading: true },
            carousel: { rotation: 0, targetRotation: 0 },
            error: null,
          },
          { type: 'SearchProjectLocally', query: action.query, projectPointer: state.view.projectPointer }
        ];
      } else {
        return [
          {
            ...state,
            taskList: {
              ...state.taskList,
              loading: true,
              tasks: [],
              offset: 0,
            },
            carousel: { rotation: 0, targetRotation: 0 },
            error: null,
          },
          {
            type: 'SearchTasks',
            query: action.query,
            view: state.view.currentView === "all" ? null : state.view.currentView,
            offset: 0,
            limit: 100
          }
        ];
      }

    case 'GlobalSearchCompleted':
      return [
        {
          ...state,
          taskList: {
            ...state.taskList,
            tasks: action.tasks,
            loading: false,
            offset: 100,
            totalCount: action.total,
          },
          error: null,
        },
        { type: 'None' }
      ];

    case 'GlobalSearchFailed':
      return [
        {
          ...state,
          taskList: { ...state.taskList, loading: false },
          error: action.error,
        },
        { type: 'None' }
      ];

    case 'ProjectSearchCompleted':
      return [
        {
          ...state,
          taskList: {
            ...state.taskList,
            tasks: action.tasks,
            loading: false,
            totalCount: action.total,
          },
          error: null,
        },
        { type: 'None' }
      ];

    case 'ProjectSearchFailed':
      return [
        {
          ...state,
          taskList: { ...state.taskList, loading: false },
          error: action.error,
        },
        { type: 'None' }
      ];

    case 'CarouselRotate':
      const newTargetRotation = state.carousel.targetRotation + action.delta;

      return [
        {
          ...state,
          carousel: {
            targetRotation: newTargetRotation,
            rotation: newTargetRotation,
          },
        },
        { type: 'None' }
      ];

    case 'CarouselUpdateCurrent':
      return [
        {
          ...state,
          carousel: { ...state.carousel, rotation: action.rotation },
        },
        { type: 'None' }
      ];

    case 'DebugToggled':
      return [
        {
          ...state,
          debug: { ...state.debug, enabled: !state.debug.enabled },
        },
        { type: 'None' }
      ];

    case 'DebugParamChanged':
      return [
        {
          ...state,
          debug: {
            ...state.debug,
            params: { ...state.debug.params, [action.param]: action.value },
          },
        },
        { type: 'None' }
      ];

    default:
      // Exhaustiveness check - ensures all action types are handled at compile time
      // If a new action is added but not handled, TypeScript will error here
      assertNever(action);
  }
}
