/**
 * API Types
 * 
 * Types for API request/response structures.
 */

import type { TaskWithPointer, TaskNode } from "./domain.js";

export interface ResponseMetadata {
  readonly total: number;
}

export interface PaginatedResponse {
  readonly data: ReadonlyArray<TaskWithPointer>;
  readonly metadata: ResponseMetadata;
}

export interface ProjectTreeResponse {
  readonly root: TaskNode;
}
