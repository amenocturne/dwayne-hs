/**
 * Application State Types
 * 
 * Structured into logical groups for better organization and maintainability.
 */

import type { TaskWithPointer, TaskNode, ViewName, TaskPointer } from "./domain.js";

/**
 * State for the task list (main content area)
 */
export interface TaskListState {
  readonly tasks: ReadonlyArray<TaskWithPointer>;
  readonly loading: boolean;
  readonly loadingMore: boolean;
  readonly offset: number;
  readonly pagesLoaded: number;
  readonly totalCount: number;
}

/**
 * State for view/filter settings
 */
export interface ViewState {
  readonly currentView: ViewName;
  readonly searchQuery: string;
  readonly projectPointer: TaskPointer | null;  // null = normal view, non-null = project view
}

/**
 * State for the detail card (selected task info)
 */
export interface DetailCardState {
  readonly selectedTask: TaskWithPointer | null;
  readonly parentProject: TaskWithPointer | null;
  readonly loadingParentProject: boolean;
  readonly parentProjectRequestId: number;
  readonly projectTree: TaskNode | null;
  readonly loadingProjectTree: boolean;
  readonly projectTreeRequestId: number;
}

/**
 * State for the 3D carousel UI
 */
export interface CarouselState {
  readonly rotation: number;
  readonly targetRotation: number;
}

/**
 * Debug state for 3D carousel parameter tweaking
 */
export interface DebugState {
  readonly enabled: boolean;
  readonly params: Carousel3DParams;
}

export interface Carousel3DParams {
  readonly radius: number;
  readonly perspective: number;
  readonly rotationSpeed: number;
  readonly anglePerCard: number;
  readonly visibleAngleRange: number;
  readonly fadeTransitionAngle: number;
  readonly rotateX: number;  // Base rotation on X axis
  readonly rotateY: number;  // Base rotation on Y axis
  readonly rotateZ: number;  // Base rotation on Z axis (always -90)
  readonly perspectiveOriginY: number;  // Perspective origin Y (percentage)
  readonly originX: number;  // X position offset (px)
  readonly originY: number;  // Y position offset (px)
  readonly originZ: number;  // Z position offset (px)
  readonly cardScale: number;  // Card size scaling factor
  readonly showDebugDots: boolean;
}

/**
 * Root application state
 */
export interface AppState {
  readonly taskList: TaskListState;
  readonly view: ViewState;
  readonly detail: DetailCardState;
  readonly carousel: CarouselState;
  readonly debug: DebugState;
  readonly error: string | null;
}

/**
 * Derived state helpers - compute values rather than storing them
 */
export function hasMoreTasks(state: AppState): boolean {
  return state.taskList.tasks.length < state.taskList.totalCount;
}

export function isProjectView(state: AppState): boolean {
  return state.view.projectPointer !== null;
}

export function isSearching(state: AppState): boolean {
  return state.view.searchQuery.trim() !== "";
}
