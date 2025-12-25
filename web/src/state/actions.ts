/**
 * Action Types
 * 
 * Discriminated union representing all possible state transitions.
 * Each action is a pure data structure describing what happened.
 */

import type { ViewName, TaskWithPointer, TaskPointer, TaskNode } from "../types/domain.js";

export type Action =
  | { type: 'ViewChanged'; view: ViewName }
  | { type: 'SearchQueryChanged'; query: string }
  | { type: 'SearchCleared' }
  | { type: 'TasksLoadStarted'; view: ViewName }
  | { type: 'TasksLoaded'; tasks: ReadonlyArray<TaskWithPointer>; total: number; offset: number }
  | { type: 'TasksLoadFailed'; error: string }
  | { type: 'LoadMoreStarted' }
  | { type: 'LoadMoreCompleted'; tasks: ReadonlyArray<TaskWithPointer>; total: number }
  | { type: 'LoadMoreFailed'; error: string }
  | { type: 'TaskClicked'; task: TaskWithPointer }
  | { type: 'DetailCardClosed' }
  | { type: 'ProjectTreeLoadStarted' }
  | { type: 'ProjectTreeLoaded'; tree: TaskNode; requestId: number }
  | { type: 'ProjectTreeLoadFailed'; requestId: number }
  | { type: 'ParentProjectLoadStarted' }
  | { type: 'ParentProjectLoaded'; project: TaskWithPointer | null; requestId: number }
  | { type: 'ParentProjectLoadFailed'; requestId: number }
  | { type: 'WebSocketReloadReceived' }
  | { type: 'ProjectViewRequested'; pointer: TaskPointer }
  | { type: 'ProjectTasksLoaded'; tasks: ReadonlyArray<TaskWithPointer>; total: number }
  | { type: 'ProjectTasksLoadFailed'; error: string }
  | { type: 'BackToViewRequested' }
  | { type: 'GlobalSearchStarted'; query: string }
  | { type: 'GlobalSearchCompleted'; tasks: ReadonlyArray<TaskWithPointer>; total: number }
  | { type: 'GlobalSearchFailed'; error: string }
  | { type: 'ProjectSearchCompleted'; tasks: ReadonlyArray<TaskWithPointer>; total: number }
  | { type: 'ProjectSearchFailed'; error: string }
  // 3D Carousel actions
  | { type: 'CarouselRotate'; delta: number }           // User scrolls wheel, change target rotation
  | { type: 'CarouselUpdateCurrent'; rotation: number }
  // Debug mode actions
  | { type: 'DebugToggled' }
  | { type: 'DebugParamChanged'; param: keyof import("../types/state.js").Carousel3DParams; value: number | boolean }
  // View navigation actions (keyboard-driven)
  | { type: 'ViewNavigateLeft' }
  | { type: 'ViewNavigateRight' };
