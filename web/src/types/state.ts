/**
 * Application State Type
 * 
 * Central immutable state structure for the application.
 */

import type { TaskWithPointer, TaskNode, ViewName, TaskPointer } from "./domain.js";

export interface AppState {
  readonly tasks: ReadonlyArray<TaskWithPointer>;
  readonly loading: boolean;
  readonly error: string | null;
  readonly currentView: ViewName;
  readonly searchQuery: string;
  readonly offset: number;
  readonly hasMore: boolean;
  readonly loadingMore: boolean;
  readonly totalCount: number;
  readonly selectedTask: TaskWithPointer | null;
  readonly projectTree: TaskNode | null;
  readonly loadingProjectTree: boolean;
  readonly projectPointer: TaskPointer | null;
  readonly parentProject: TaskWithPointer | null;
  readonly loadingParentProject: boolean;
}
