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
  readonly pagesLoaded: number;  // How many pages have been loaded (1 = 100 tasks, 2 = 200 tasks, etc.)
  readonly totalCount: number;
  readonly selectedTask: TaskWithPointer | null;
  readonly projectTree: TaskNode | null;
  readonly loadingProjectTree: boolean;
  readonly projectPointer: TaskPointer | null;
  readonly parentProject: TaskWithPointer | null;
  readonly loadingParentProject: boolean;
  
  // 3D Carousel state
  readonly carouselRotation: number;       // Current rotation in degrees (interpolated, smooth)
  readonly carouselTargetRotation: number; // Target rotation in degrees (from user input)
}
