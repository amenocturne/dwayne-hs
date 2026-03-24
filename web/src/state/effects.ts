/**
 * Effect Types and Runner
 * 
 * Effect types are pure data structures describing side effects to perform.
 * The effect runner executes these effects and dispatches resulting actions.
 * 
 * Phase 4: Enhanced with exhaustiveness checking using assertNever.
 */

import type { ViewName, TaskPointer, TaskWithPointer, TaskNode, OrgTime } from "../types/domain.js";
import type { FilePath, TaskIndex } from "../types/branded.js";
import type { Action } from "./actions.js";
import { fetchTasks, fetchSearchResults, fetchProjectTree, fetchParentProject, captureTask, editTask, addTag, removeTag } from "../api/client.js";
import { assertNever } from "../types/utils.js";

export type Effect =
  | { type: 'None' }
  | { type: 'FetchTasks'; view: ViewName; offset: number; limit: number }
  | { type: 'SearchTasks'; query: string; view: ViewName | null; offset: number; limit: number }
  | { type: 'FetchProjectTree'; pointer: TaskPointer; requestId: number; updateTaskList: boolean }
  | { type: 'FetchParentProject'; pointer: TaskPointer; requestId: number }
  | { type: 'ShowToast'; message: string }
  | { type: 'LoadMoreTasks'; view: ViewName; offset: number; limit: number }
  | { type: 'SearchProjectLocally'; query: string; projectPointer: TaskPointer }
  | { type: 'CaptureTask'; title: string }
  | { type: 'ChangeKeyword'; file: FilePath; taskIndex: TaskIndex; keyword: string }
  | { type: 'ChangePriority'; file: FilePath; taskIndex: TaskIndex; priority: number | null }
  | { type: 'AddTag'; file: FilePath; taskIndex: TaskIndex; tag: string }
  | { type: 'RemoveTag'; file: FilePath; taskIndex: TaskIndex; tag: string }
  | { type: 'EditTask'; file: FilePath; taskIndex: TaskIndex; title?: string; tags?: ReadonlyArray<string>; scheduled?: OrgTime | null; deadline?: OrgTime | null }
  | { type: 'Batch'; effects: ReadonlyArray<Effect> };

export type Dispatch = (action: Action) => void;

/**
 * Flattens a task tree into a flat array of tasks with pointers.
 */
function flattenTaskTree(node: TaskNode): ReadonlyArray<TaskWithPointer> {
  const result: TaskWithPointer[] = [];
  
  function traverse(n: TaskNode): void {
    result.push({ task: n.task, pointer: n.pointer });
    n.children.forEach(traverse);
  }
  
  traverse(node);
  return result;
}

/**
 * Shows a toast notification.
 */
function showToastDOM(message: string): void {
  const toast = document.createElement("div");
  toast.textContent = message;
  toast.style.cssText = `
    position: fixed;
    bottom: 20px;
    right: 20px;
    background: var(--link-color);
    color: white;
    padding: 12px 20px;
    border-radius: 4px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
    z-index: 10000;
    font-size: 14px;
    animation: slideIn 0.3s ease-out;
  `;

  document.body.appendChild(toast);

  setTimeout(() => {
    toast.style.animation = "slideOut 0.3s ease-out";
    setTimeout(() => {
      document.body.removeChild(toast);
    }, 300);
  }, 3000);
}

/**
 * Executes effects and dispatches resulting actions.
 * This is where all side effects happen - API calls, DOM manipulation, etc.
 */
export async function runEffect(effect: Effect, dispatch: Dispatch): Promise<void> {
  switch (effect.type) {
    case 'None':
      break;

    case 'FetchTasks':
      try {
        const result = await fetchTasks(effect.view, effect.offset, effect.limit);
        dispatch({
          type: 'TasksLoaded',
          tasks: result.data,
          total: result.metadata.total,
          offset: effect.offset + result.data.length,
        });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Unknown error occurred";
        dispatch({ type: 'TasksLoadFailed', error: errorMessage });
      }
      break;

    case 'SearchTasks':
      try {
        const result = await fetchSearchResults(
          effect.query,
          effect.view,
          effect.offset,
          effect.limit,
        );
        dispatch({
          type: 'GlobalSearchCompleted',
          tasks: result.data,
          total: result.metadata.total,
        });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Search failed";
        dispatch({ type: 'GlobalSearchFailed', error: errorMessage });
      }
      break;

    case 'FetchProjectTree':
      try {
        const result = await fetchProjectTree(effect.pointer.file, effect.pointer.taskIndex);
        const flatTasks = flattenTaskTree(result.root);
        
        // Only update task list when explicitly requested (e.g., ProjectViewRequested)
        if (effect.updateTaskList) {
          dispatch({
            type: 'ProjectTasksLoaded',
            tasks: flatTasks,
            total: flatTasks.length,
          });
        }
        
        // Always update detail card tree
        dispatch({
          type: 'ProjectTreeLoaded',
          tree: result.root,
          requestId: effect.requestId,
        });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Failed to load project";
        if (effect.updateTaskList) {
          dispatch({ type: 'ProjectTasksLoadFailed', error: errorMessage });
        }
        dispatch({ 
          type: 'ProjectTreeLoadFailed',
          requestId: effect.requestId 
        });
      }
      break;

    case 'FetchParentProject':
      try {
        const parent = await fetchParentProject(effect.pointer.file, effect.pointer.taskIndex);
        dispatch({ 
          type: 'ParentProjectLoaded', 
          project: parent,
          requestId: effect.requestId 
        });
      } catch (err) {
        console.error("Failed to fetch parent project:", err);
        dispatch({ 
          type: 'ParentProjectLoadFailed',
          requestId: effect.requestId 
        });
      }
      break;

    case 'LoadMoreTasks':
      try {
        const result = await fetchTasks(effect.view, effect.offset, effect.limit);
        dispatch({
          type: 'LoadMoreCompleted',
          tasks: result.data,
          total: result.metadata.total,
        });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Unknown error occurred";
        dispatch({ type: 'LoadMoreFailed', error: errorMessage });
      }
      break;

    case 'SearchProjectLocally':
      try {
        const result = await fetchProjectTree(effect.projectPointer.file, effect.projectPointer.taskIndex);
        const flatTasks = flattenTaskTree(result.root);
        
        const lowerQuery = effect.query.toLowerCase();
        const filteredTasks = flatTasks.filter((taskWithPointer) => {
          const titleText = taskWithPointer.task.title
            .map((node) => (node.type === "plain" ? node.text : node.url))
            .join(" ")
            .toLowerCase();
          const descText = taskWithPointer.task.description
            .map((node) => (node.type === "plain" ? node.text : node.url))
            .join(" ")
            .toLowerCase();
          const tagsText = taskWithPointer.task.tags.join(" ").toLowerCase();
          
          return (
            titleText.includes(lowerQuery) ||
            descText.includes(lowerQuery) ||
            tagsText.includes(lowerQuery) ||
            taskWithPointer.task.todoKeyword.toLowerCase().includes(lowerQuery)
          );
        });

        dispatch({
          type: 'ProjectSearchCompleted',
          tasks: filteredTasks,
          total: filteredTasks.length,
        });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Search failed";
        dispatch({ type: 'ProjectSearchFailed', error: errorMessage });
      }
      break;

    case 'CaptureTask':
      try {
        await captureTask(effect.title);
        dispatch({ type: 'CaptureSucceeded' });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Capture failed";
        dispatch({ type: 'MutationFailed', error: errorMessage });
      }
      break;

    case 'ChangeKeyword':
      try {
        const updated = await editTask({ file: effect.file, taskIndex: effect.taskIndex, keyword: effect.keyword });
        dispatch({ type: 'MutationSucceeded', updatedTask: updated });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Failed to change keyword";
        dispatch({ type: 'MutationFailed', error: errorMessage });
      }
      break;

    case 'ChangePriority':
      try {
        const updated = await editTask({ file: effect.file, taskIndex: effect.taskIndex, priority: effect.priority });
        dispatch({ type: 'MutationSucceeded', updatedTask: updated });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Failed to change priority";
        dispatch({ type: 'MutationFailed', error: errorMessage });
      }
      break;

    case 'AddTag':
      try {
        const updated = await addTag(effect.file, effect.taskIndex, effect.tag);
        dispatch({ type: 'MutationSucceeded', updatedTask: updated });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Failed to add tag";
        dispatch({ type: 'MutationFailed', error: errorMessage });
      }
      break;

    case 'RemoveTag':
      try {
        const updated = await removeTag(effect.file, effect.taskIndex, effect.tag);
        dispatch({ type: 'MutationSucceeded', updatedTask: updated });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Failed to remove tag";
        dispatch({ type: 'MutationFailed', error: errorMessage });
      }
      break;

    case 'EditTask':
      try {
        const updated = await editTask({
          file: effect.file,
          taskIndex: effect.taskIndex,
          ...(effect.title !== undefined ? { title: effect.title } : {}),
          ...(effect.tags !== undefined ? { tags: effect.tags } : {}),
          ...(effect.scheduled !== undefined ? { scheduled: effect.scheduled } : {}),
          ...(effect.deadline !== undefined ? { deadline: effect.deadline } : {}),
        });
        dispatch({ type: 'MutationSucceeded', updatedTask: updated });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Failed to edit task";
        dispatch({ type: 'MutationFailed', error: errorMessage });
      }
      break;

    case 'ShowToast':
      showToastDOM(effect.message);
      break;

    case 'Batch':
      for (const e of effect.effects) {
        await runEffect(e, dispatch);
      }
      break;

    default:
      // Exhaustiveness check - ensures all effect types are handled at compile time
      // If a new effect is added but not handled, TypeScript will error here
      assertNever(effect);
  }
}
