
/**
 * Runtime validation for API data.
 *
 * These type guards are essential for ensuring that the data we fetch from
 * the backend conforms to our strict domain types, especially our branded types.
 * This is the boundary where the untyped world meets our type-safe domain.
 */

import { PaginatedResponse, ProjectTreeResponse } from '../types/api.js';
import { filePath, taskIndex, urlString } from '../types/branded.js';
import type { FilePath, TaskIndex, UrlString } from '../types/branded.js';
import type { TaskPointer, TextNode, RichText, OrgTime, RepeatInterval, DelayInterval, Task, TaskWithPointer, TaskNode } from '../types/domain.js';

// Helper to check if an input is a plain object
function isObject(input: unknown): input is Record<string, unknown> {
  return typeof input === 'object' && input !== null && !Array.isArray(input);
}

// --- Branded Type Guards ---

export function isFilePath(input: unknown): input is FilePath {
  if (typeof input !== 'string') return false;
  try {
    filePath(input); // Use the smart constructor which might throw
    return true;
  } catch {
    return false;
  }
}

export function isTaskIndex(input: unknown): input is TaskIndex {
  if (typeof input !== 'number') return false;
  try {
    taskIndex(input); // Use the smart constructor which might throw
    return true;
  } catch {
    return false;
  }
}

export function isUrlString(input: unknown): input is UrlString {
  if (typeof input !== 'string') return false;
  try {
    urlString(input); // Use the smart constructor which might throw
    return true;
  } catch {
    return false;
  }
}

// --- Domain Type Guards ---

export function isTaskPointer(input: unknown): input is TaskPointer {
  if (!isObject(input)) return false;
  return isFilePath(input['file']) && isTaskIndex(input['taskIndex']);
}

function isTextNode(input: unknown): input is TextNode {
    if (!isObject(input)) return false;
    if (input['type'] === 'plain' && typeof input['text'] === 'string') {
        return true;
    }
    if (input['type'] === 'link' && isUrlString(input['url']) && (input['title'] === null || typeof input['title'] === 'string')) {
        return true;
    }
    return false;
}

function isRichText(input: unknown): input is RichText {
    return Array.isArray(input) && input.every(isTextNode);
}

function isRepeatInterval(input: unknown): input is RepeatInterval {
    if (!isObject(input)) return false;
    return (
        typeof input['repeatType'] === 'string' &&
        typeof input['repeatValue'] === 'number' &&
        typeof input['repeatTimeUnit'] === 'string'
    );
}

function isDelayInterval(input: unknown): input is DelayInterval {
    if (!isObject(input)) return false;
    return (
        typeof input['delayType'] === 'string' &&
        typeof input['delayValue'] === 'number' &&
        typeof input['delayTimeUnit'] === 'string'
    );
}

function isOrgTime(input: unknown): input is OrgTime {
    if (!isObject(input)) return false;
    return (
        typeof input['date'] === 'string' &&
        (input['time'] === null || typeof input['time'] === 'string') &&
        (input['repeater'] === null || isRepeatInterval(input['repeater'])) &&
        (input['delay'] === null || isDelayInterval(input['delay']))
    );
}

export function isTask(input: unknown): input is Task {
    if (!isObject(input)) return false;
    return (
        typeof input['level'] === 'number' &&
        typeof input['todoKeyword'] === 'string' &&
        (input['priority'] === null || typeof input['priority'] === 'number') &&
        isRichText(input['title']) &&
        Array.isArray(input['tags']) && input['tags'].every(t => typeof t === 'string') &&
        (input['scheduled'] === null || isOrgTime(input['scheduled'])) &&
        (input['deadline'] === null || isOrgTime(input['deadline'])) &&
        (input['createdProp'] === null || isOrgTime(input['createdProp'])) &&
        (input['closed'] === null || isOrgTime(input['closed'])) &&
        Array.isArray(input['properties']) &&
        isRichText(input['description'])
    );
}

export function isTaskWithPointer(input: unknown): input is TaskWithPointer {
    if (!isObject(input)) return false;
    return isTask(input['task']) && isTaskPointer(input['pointer']);
}

function isPaginatedTasksResponse(input: unknown): input is PaginatedResponse {
    if (!isObject(input)) return false;
    const metadata = input['metadata'];
    return (
        isObject(metadata) &&
        typeof metadata['total'] === 'number' &&
        Array.isArray(input['data']) &&
        input['data'].every(isTaskWithPointer)
    );
}

export function isTaskNode(input: unknown): input is TaskNode {
    if (!isObject(input)) return false;
    return (
        isTask(input['task']) &&
        isTaskPointer(input['pointer']) &&
        Array.isArray(input['children']) &&
        input['children'].every(isTaskNode) // Recursive validation
    );
}

function isProjectTreeResponse(input: unknown): input is ProjectTreeResponse {
    if (!isObject(input)) return false;
    return isTaskNode(input['root']);
}

export async function parsePaginatedResponse(response: Response): Promise<PaginatedResponse> {
    const json = await response.json();
    if (!isPaginatedTasksResponse(json)) {
        throw new Error('Invalid paginated response from API');
    }
    return json;
}

export async function parseProjectTreeResponse(response: Response): Promise<ProjectTreeResponse> {
    const json = await response.json();
    if (!isProjectTreeResponse(json)) {
        throw new Error('Invalid project tree response from API');
    }
    return json;
}
