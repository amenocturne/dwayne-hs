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
        pagesLoaded: 0,
        projectPointer: null,
          searchQuery: '',
          error: null,
          carouselRotation: 0,
          carouselTargetRotation: 0,
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
            carouselRotation: 0,
            carouselTargetRotation: 0,
            projectTreeRequestId: state.projectTreeRequestId + 1,
          },
          { 
            type: 'FetchProjectTree', 
            pointer: state.projectPointer,
            requestId: state.projectTreeRequestId + 1 
          }
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
            pagesLoaded: 0,
            error: null,
            carouselRotation: 0,
            carouselTargetRotation: 0,
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

    case 'TasksLoaded': {
      const hasMore = action.tasks.length === 100;
      return [
        {
          ...state,
          tasks: action.tasks,
          loading: false,
          error: null,
          offset: action.offset,
          hasMore,
          totalCount: action.total,
          pagesLoaded: 1,
          loadingMore: hasMore,
        },
        hasMore 
          ? { type: 'LoadMoreTasks', view: state.currentView, offset: action.offset, limit: 100 }
          : { type: 'None' }
      ];
    }

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
          pagesLoaded: state.pagesLoaded + 1,
        },
        { type: 'None' }
      ];

    case 'LoadMoreFailed':
      return [
        { ...state, loadingMore: false, error: action.error },
        { type: 'None' }
      ];

    case 'TaskClicked': {
      const isProject = action.task.task.todoKeyword === "PROJECT";
      
      return [
        {
          ...state,
          selectedTask: action.task,
          projectTree: null,
          loadingProjectTree: isProject,
          parentProject: null,
          loadingParentProject: !isProject,
          // Increment request IDs to invalidate in-flight requests
          projectTreeRequestId: isProject ? state.projectTreeRequestId + 1 : state.projectTreeRequestId,
          parentProjectRequestId: !isProject ? state.parentProjectRequestId + 1 : state.parentProjectRequestId,
        },
        {
          type: 'Batch',
          effects: isProject
            ? [{ 
                type: 'FetchProjectTree', 
                pointer: action.task.pointer,
                requestId: state.projectTreeRequestId + 1 
              }]
            : [{ 
                type: 'FetchParentProject', 
                pointer: action.task.pointer,
                requestId: state.parentProjectRequestId + 1 
              }]
        }
      ];
    }

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
      // Only update if this response is for the current request
      if (action.requestId !== state.projectTreeRequestId) {
        return [state, { type: 'None' }];
      }
      return [
        {
          ...state,
          projectTree: action.tree,
          loadingProjectTree: false,
        },
        { type: 'None' }
      ];

    case 'ProjectTreeLoadFailed':
      // Only update if this error is for the current request
      if (action.requestId !== state.projectTreeRequestId) {
        return [state, { type: 'None' }];
      }
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
      // Only update if this response is for the current request
      if (action.requestId !== state.parentProjectRequestId) {
        return [state, { type: 'None' }];
      }
      return [
        {
          ...state,
          parentProject: action.project,
          loadingParentProject: false,
        },
        { type: 'None' }
      ];

    case 'ParentProjectLoadFailed':
      // Only update if this error is for the current request
      if (action.requestId !== state.parentProjectRequestId) {
        return [state, { type: 'None' }];
      }
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
          projectTreeRequestId: state.projectTreeRequestId + 1,
          carouselRotation: 0,
          carouselTargetRotation: 0,
        },
        { 
          type: 'FetchProjectTree', 
          pointer: action.pointer,
          requestId: state.projectTreeRequestId + 1 
        }
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
          pagesLoaded: 0,
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
            carouselRotation: 0,
            carouselTargetRotation: 0,
            projectTreeRequestId: state.projectTreeRequestId + 1,
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
            carouselRotation: 0,
            carouselTargetRotation: 0,
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

    // 3D Carousel actions
    case 'CarouselRotate':
      const newTargetRotation = state.carouselTargetRotation + action.delta;
      
      return [
        {
          ...state,
          carouselTargetRotation: newTargetRotation,
          carouselRotation: newTargetRotation,
        },
        { type: 'None' }
      ];

    case 'CarouselUpdateCurrent':
      // Animation loop interpolated to new current rotation
      return [
        {
          ...state,
          carouselRotation: action.rotation,
        },
        { type: 'None' }
      ];

    default:
      // Exhaustiveness check - ensures all action types are handled at compile time
      // If a new action is added but not handled, TypeScript will error here
      assertNever(action);
  }
}
