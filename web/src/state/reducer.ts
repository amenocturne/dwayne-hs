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
          currentView: action.view,
          loading: true,
          tasks: [],
          offset: 0,
          hasMore: true,
          loadingMore: false,
          projectPointer: null,
          searchQuery: '',
          error: null,
        },
        { type: 'FetchTasks', view: action.view, offset: 0, limit: 100 }
      ];

    case 'SearchQueryChanged':
      return [
        { ...state, searchQuery: action.query },
        { type: 'None' }
      ];

    case 'SearchCleared':
      if (state.projectPointer) {
        return [
          {
            ...state,
            searchQuery: '',
            loading: true,
            error: null,
          },
          { type: 'FetchProjectTree', pointer: state.projectPointer }
        ];
      } else {
        return [
          {
            ...state,
            searchQuery: '',
            loading: true,
            tasks: [],
            offset: 0,
            hasMore: true,
            error: null,
          },
          { type: 'FetchTasks', view: state.currentView, offset: 0, limit: 100 }
        ];
      }

    case 'TasksLoadStarted':
      return [
        {
          ...state,
          loading: true,
          error: null,
          currentView: action.view,
          tasks: [],
          offset: 0,
          hasMore: true,
          loadingMore: false,
        },
        { type: 'None' }
      ];

    case 'TasksLoaded':
      return [
        {
          ...state,
          tasks: action.tasks,
          loading: false,
          error: null,
          offset: action.offset,
          hasMore: action.tasks.length === 100,
          totalCount: action.total,
        },
        { type: 'None' }
      ];

    case 'TasksLoadFailed':
      return [
        { ...state, loading: false, error: action.error },
        { type: 'None' }
      ];

    case 'LoadMoreStarted':
      if (state.loadingMore || !state.hasMore || state.loading) {
        return [state, { type: 'None' }];
      }
      return [
        { ...state, loadingMore: true },
        { type: 'LoadMoreTasks', view: state.currentView, offset: state.offset, limit: 100 }
      ];

    case 'LoadMoreCompleted':
      return [
        {
          ...state,
          tasks: [...state.tasks, ...action.tasks] as ReadonlyArray<TaskWithPointer>,
          offset: state.offset + action.tasks.length,
          hasMore: action.tasks.length === 100,
          loadingMore: false,
          totalCount: action.total,
        },
        { type: 'None' }
      ];

    case 'LoadMoreFailed':
      return [
        { ...state, loadingMore: false, error: action.error },
        { type: 'None' }
      ];

    case 'TaskSelected':
      return [
        {
          ...state,
          selectedTask: action.task,
          projectTree: null,
          loadingProjectTree: false,
          parentProject: null,
          loadingParentProject: false,
        },
        { type: 'None' }
      ];

    case 'SidebarClosed':
      return [
        { ...state, selectedTask: null },
        { type: 'None' }
      ];

    case 'ProjectTreeLoadStarted':
      return [
        { ...state, loadingProjectTree: true },
        { type: 'None' }
      ];

    case 'ProjectTreeLoaded':
      return [
        {
          ...state,
          projectTree: action.tree,
          loadingProjectTree: false,
        },
        { type: 'None' }
      ];

    case 'ProjectTreeLoadFailed':
      return [
        {
          ...state,
          projectTree: null,
          loadingProjectTree: false,
        },
        { type: 'None' }
      ];

    case 'ParentProjectLoadStarted':
      return [
        { ...state, loadingParentProject: true },
        { type: 'None' }
      ];

    case 'ParentProjectLoaded':
      return [
        {
          ...state,
          parentProject: action.project,
          loadingParentProject: false,
        },
        { type: 'None' }
      ];

    case 'ParentProjectLoadFailed':
      return [
        {
          ...state,
          parentProject: null,
          loadingParentProject: false,
        },
        { type: 'None' }
      ];

    case 'WebSocketReloadReceived':
      if (state.searchQuery.trim() !== "") {
        return [
          {
            ...state,
            loading: true,
            error: null,
          },
          {
            type: 'Batch',
            effects: [
              { type: 'ShowToast', message: 'Files updated, reloading...' },
              state.projectPointer
                ? { type: 'SearchProjectLocally', query: state.searchQuery, projectPointer: state.projectPointer }
                : { 
                    type: 'SearchTasks', 
                    query: state.searchQuery, 
                    view: state.currentView === "all" ? null : state.currentView,
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
            loading: true,
            error: null,
          },
          {
            type: 'Batch',
            effects: [
              { type: 'ShowToast', message: 'Files updated, reloading...' },
              { type: 'FetchTasks', view: state.currentView, offset: 0, limit: 100 }
            ]
          }
        ];
      }

    case 'ProjectViewRequested':
      return [
        {
          ...state,
          loading: true,
          error: null,
          tasks: [],
          offset: 0,
          hasMore: false,
          projectPointer: action.pointer,
          selectedTask: null,
        },
        { type: 'FetchProjectTree', pointer: action.pointer }
      ];

    case 'ProjectTasksLoaded':
      return [
        {
          ...state,
          tasks: action.tasks,
          loading: false,
          error: null,
          totalCount: action.total,
        },
        { type: 'None' }
      ];

    case 'ProjectTasksLoadFailed':
      return [
        { ...state, loading: false, error: action.error },
        { type: 'None' }
      ];

    case 'BackToViewRequested':
      return [
        {
          ...state,
          projectPointer: null,
          loading: true,
          tasks: [],
          offset: 0,
          hasMore: true,
          error: null,
        },
        { type: 'FetchTasks', view: state.currentView, offset: 0, limit: 100 }
      ];

    case 'GlobalSearchStarted':
      if (state.projectPointer) {
        return [
          {
            ...state,
            loading: true,
            error: null,
          },
          { type: 'SearchProjectLocally', query: action.query, projectPointer: state.projectPointer }
        ];
      } else {
        return [
          {
            ...state,
            loading: true,
            error: null,
            tasks: [],
            offset: 0,
            hasMore: true,
          },
          {
            type: 'SearchTasks',
            query: action.query,
            view: state.currentView === "all" ? null : state.currentView,
            offset: 0,
            limit: 100
          }
        ];
      }

    case 'GlobalSearchCompleted':
      return [
        {
          ...state,
          tasks: action.tasks,
          loading: false,
          error: null,
          offset: 100,
          hasMore: action.tasks.length === 100,
          totalCount: action.total,
        },
        { type: 'None' }
      ];

    case 'GlobalSearchFailed':
      return [
        { ...state, loading: false, error: action.error },
        { type: 'None' }
      ];

    case 'ProjectSearchCompleted':
      return [
        {
          ...state,
          tasks: action.tasks,
          loading: false,
          error: null,
          totalCount: action.total,
        },
        { type: 'None' }
      ];

    case 'ProjectSearchFailed':
      return [
        { ...state, loading: false, error: action.error },
        { type: 'None' }
      ];

    default:
      // Exhaustiveness check - ensures all action types are handled at compile time
      // If a new action is added but not handled, TypeScript will error here
      assertNever(action);
  }
}
