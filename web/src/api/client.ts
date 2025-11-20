
/**
 * API Client
 *
 * Pure functions for communicating with the backend API.
 * All functions are side-effect aware (async) but return predictable results.
 *
 * Phase 4: Enhanced with runtime validation and branded types.
 */

import type { ViewName } from "../types/domain.js";
import type { FilePath, TaskIndex } from "../types/branded.js";
import type { PaginatedResponse, ProjectTreeResponse } from "../types/api.js";
import { parsePaginatedResponse, parseProjectTreeResponse } from "./validation.js";
import { unwrapFilePath, unwrapTaskIndex } from "../types/branded.js";

const API_BASE_URL = "http://localhost:8080";

/**
 * Fetches tasks for a specific view with pagination.
 * Validates response at runtime to ensure type safety.
 */
export async function fetchTasks(
  view: ViewName,
  offset: number = 0,
  limit: number = 100,
): Promise<PaginatedResponse> {
  const url = `${API_BASE_URL}/api/views/${view}?offset=${offset}&limit=${limit}`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to fetch tasks: ${response.status} ${response.statusText}`);
  }
  return await parsePaginatedResponse(response);
}

/**
 * Searches for tasks matching a query string.
 * Optionally filters by view.
 * Validates response at runtime to ensure type safety.
 */
export async function fetchSearchResults(
  query: string,
  view: ViewName | null = null,
  offset: number = 0,
  limit: number = 100,
): Promise<PaginatedResponse> {
  const viewParam = view ? `&view=${view}` : "";
  const url = `${API_BASE_URL}/api/search?query=${encodeURIComponent(query)}${viewParam}&offset=${offset}&limit=${limit}`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Search failed: ${response.status} ${response.statusText}`);
  }
  return await parsePaginatedResponse(response);
}

/**
 * Fetches a project by its file pointer.
 * Validates response at runtime to ensure type safety.
 * Now accepts branded FilePath and TaskIndex types.
 */
export async function fetchProjectByPointer(
  file: FilePath,
  taskIndex: TaskIndex,
): Promise<PaginatedResponse> {
  const fileStr = unwrapFilePath(file);
  const indexNum = unwrapTaskIndex(taskIndex);
  const url = `${API_BASE_URL}/api/projects/by-pointer?file=${encodeURIComponent(fileStr)}&taskIndex=${indexNum}`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(
      `Failed to fetch project: ${response.status} ${response.statusText}`,
    );
  }
  return await parsePaginatedResponse(response);
}

/**
 * Fetches the complete tree structure of a project's subtasks.
 * Validates response at runtime to ensure type safety.
 * Now accepts branded FilePath and TaskIndex types.
 */
export async function fetchProjectTree(
  file: FilePath,
  taskIndex: TaskIndex,
): Promise<ProjectTreeResponse> {
  const fileStr = unwrapFilePath(file);
  const indexNum = unwrapTaskIndex(taskIndex);
  const url = `${API_BASE_URL}/api/projects/tasks?file=${encodeURIComponent(fileStr)}&taskIndex=${indexNum}`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(
      `Failed to fetch project tree: ${response.status} ${response.statusText}`,
    );
  }
  return await parseProjectTreeResponse(response);
}
