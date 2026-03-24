
/**
 * API Client
 *
 * Pure functions for communicating with the backend API.
 * All functions are side-effect aware (async) but return predictable results.
 *
 * Phase 4: Enhanced with runtime validation and branded types.
 */

import type { ViewName, TaskWithPointer } from "../types/domain.js";
import type { FilePath, TaskIndex } from "../types/branded.js";
import type { PaginatedResponse, ProjectTreeResponse } from "../types/api.js";
import { parsePaginatedResponse, parseProjectTreeResponse } from "./validation.js";
import { isTaskWithPointer } from "./validation.js";
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

/**
 * Fetches the parent project for a given task.
 * Returns null if the task has no parent project (404 response).
 */
export async function fetchParentProject(
  file: FilePath,
  taskIndex: TaskIndex,
): Promise<TaskWithPointer | null> {
  const fileStr = unwrapFilePath(file);
  const indexNum = unwrapTaskIndex(taskIndex);
  const url = `${API_BASE_URL}/api/projects/parent?file=${encodeURIComponent(fileStr)}&taskIndex=${indexNum}`;

  const response = await fetch(url);

  // 404 means no parent project - this is normal, not an error
  if (response.status === 404) {
    return null;
  }

  if (!response.ok) {
    throw new Error(`Failed to fetch parent project: ${response.status} ${response.statusText}`);
  }

  const result = await parsePaginatedResponse(response);
  // API returns single-item array, extract first item or null
  return result.data[0] ?? null;
}

// --- Edit Task API (unified mutation endpoint) ---

export interface EditTaskParams {
  readonly file: FilePath;
  readonly taskIndex: TaskIndex;
  readonly keyword?: string;
  readonly priority?: number | null;
  readonly title?: string;
  readonly description?: string;
  readonly tags?: ReadonlyArray<string>;
  readonly scheduled?: import("../types/domain.js").OrgTime | null;
  readonly deadline?: import("../types/domain.js").OrgTime | null;
}

/**
 * Unified task edit endpoint. Only includes fields that are being changed.
 * For nullable fields (priority, scheduled, deadline):
 *   - omitted = don't change
 *   - null = clear the field
 *   - value = set to this value
 */
export async function editTask(params: EditTaskParams): Promise<TaskWithPointer> {
  const body: Record<string, unknown> = {
    file: unwrapFilePath(params.file),
    taskIndex: unwrapTaskIndex(params.taskIndex),
  };
  if (params.keyword !== undefined) body.keyword = params.keyword;
  if (params.priority !== undefined) body.priority = params.priority;
  if (params.title !== undefined) body.title = params.title;
  if (params.description !== undefined) body.description = params.description;
  if (params.tags !== undefined) body.tags = params.tags;
  if (params.scheduled !== undefined) body.scheduled = params.scheduled;
  if (params.deadline !== undefined) body.deadline = params.deadline;

  const response = await fetch(`${API_BASE_URL}/api/tasks/edit`, {
    method: "PUT",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  return parseMutationResponse(response);
}

// --- Legacy Mutation API functions ---

async function parseMutationResponse(response: Response): Promise<TaskWithPointer> {
  if (!response.ok) {
    const text = await response.text().catch(() => "");
    throw new Error(`Mutation failed: ${response.status} ${response.statusText}${text ? ` - ${text}` : ""}`);
  }
  const json = await response.json();
  if (!isTaskWithPointer(json)) {
    throw new Error("Invalid TaskWithPointer response from mutation API");
  }
  return json;
}

/**
 * Captures a new task with the given title.
 */
export async function captureTask(title: string): Promise<TaskWithPointer> {
  const response = await fetch(`${API_BASE_URL}/api/tasks/capture`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ captureTitle: title }),
  });
  return parseMutationResponse(response);
}

/**
 * Changes the keyword (TODO state) of a task.
 */
export async function changeKeyword(
  file: FilePath,
  idx: TaskIndex,
  keyword: string,
): Promise<TaskWithPointer> {
  const response = await fetch(`${API_BASE_URL}/api/tasks/keyword`, {
    method: "PUT",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      ckrFile: unwrapFilePath(file),
      ckrTaskIndex: unwrapTaskIndex(idx),
      ckrKeyword: keyword,
    }),
  });
  return parseMutationResponse(response);
}

/**
 * Changes the priority of a task. Pass null to remove priority.
 */
export async function changePriority(
  file: FilePath,
  idx: TaskIndex,
  priority: number | null,
): Promise<TaskWithPointer> {
  const response = await fetch(`${API_BASE_URL}/api/tasks/priority`, {
    method: "PUT",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      cprFile: unwrapFilePath(file),
      cprTaskIndex: unwrapTaskIndex(idx),
      cprPriority: priority,
    }),
  });
  return parseMutationResponse(response);
}

/**
 * Adds a tag to a task.
 */
export async function addTag(
  file: FilePath,
  idx: TaskIndex,
  tag: string,
): Promise<TaskWithPointer> {
  const response = await fetch(`${API_BASE_URL}/api/tasks/tags/add`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      trFile: unwrapFilePath(file),
      trTaskIndex: unwrapTaskIndex(idx),
      trTag: tag,
    }),
  });
  return parseMutationResponse(response);
}

/**
 * Removes a tag from a task.
 */
export async function removeTag(
  file: FilePath,
  idx: TaskIndex,
  tag: string,
): Promise<TaskWithPointer> {
  const response = await fetch(`${API_BASE_URL}/api/tasks/tags/remove`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      trFile: unwrapFilePath(file),
      trTaskIndex: unwrapTaskIndex(idx),
      trTag: tag,
    }),
  });
  return parseMutationResponse(response);
}

/**
 * Deletes a task.
 */
export async function deleteTask(
  file: FilePath,
  idx: TaskIndex,
): Promise<TaskWithPointer> {
  const response = await fetch(`${API_BASE_URL}/api/tasks/delete`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      tprFile: unwrapFilePath(file),
      tprTaskIndex: unwrapTaskIndex(idx),
    }),
  });
  return parseMutationResponse(response);
}
