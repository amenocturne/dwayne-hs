/**
 * Effect Types and Runner
 * 
 * Effect types are pure data structures describing side effects to perform.
 * The effect runner executes these effects and dispatches resulting actions.
 * 
 * Phase 4: Enhanced with exhaustiveness checking using assertNever.
 */

import type { ViewName, TaskPointer, TaskWithPointer, TaskNode } from "../types/domain.js";
import type { Action } from "./actions.js";
import { fetchTasks, fetchSearchResults, fetchProjectTree } from "../api/client.js";
import { assertNever } from "../types/utils.js";

export type Effect =
  | { type: 'None' }
  | { type: 'FetchTasks'; view: ViewName; offset: number; limit: number }
  | { type: 'SearchTasks'; query: string; view: ViewName | null; offset: number; limit: number }
  | { type: 'FetchProjectTree'; pointer: TaskPointer }
  | { type: 'FetchParentProject'; task: TaskWithPointer; allProjects: ReadonlyArray<TaskWithPointer> }
  | { type: 'ShowToast'; message: string }
  | { type: 'LoadMoreTasks'; view: ViewName; offset: number; limit: number }
  | { type: 'FetchAllProjects' }
  | { type: 'SearchProjectLocally'; query: string; projectPointer: TaskPointer }
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
 * Checks if a task tree contains a specific task by pointer.
 */
function containsTask(tree: TaskNode, pointer: TaskPointer): boolean {
  if (tree.pointer.file === pointer.file && tree.pointer.taskIndex === pointer.taskIndex) {
    return true;
  }
  return tree.children.some(child => containsTask(child, pointer));
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
        dispatch({
          type: 'ProjectTasksLoaded',
          tasks: flatTasks,
          total: flatTasks.length,
        });
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : "Failed to load project";
        dispatch({ type: 'ProjectTasksLoadFailed', error: errorMessage });
      }
      break;

    case 'FetchParentProject':
      try {
        const { task, pointer } = effect.task;
        
        if (task.todoKeyword === "PROJECT") {
          dispatch({ type: 'ParentProjectLoaded', project: null });
          return;
        }

        const projectsInSameFile = effect.allProjects.filter(
          (p) => p.pointer.file === pointer.file && p.pointer.taskIndex < pointer.taskIndex
        );

        let parentProject: TaskWithPointer | null = null;
        
        for (let i = projectsInSameFile.length - 1; i >= 0; i--) {
          const candidate = projectsInSameFile[i];
          if (!candidate) continue; // Handle noUncheckedIndexedAccess
          
          if (candidate.task.level >= task.level) {
            continue;
          }
          
          try {
            const tree = await fetchProjectTree(candidate.pointer.file, candidate.pointer.taskIndex);
            
            if (containsTask(tree.root, pointer)) {
              parentProject = { task: candidate.task, pointer: candidate.pointer };
              break;
            }
          } catch (err) {
            continue;
          }
        }

        dispatch({ type: 'ParentProjectLoaded', project: parentProject });
      } catch (err) {
        console.error("Failed to find parent project:", err);
        dispatch({ type: 'ParentProjectLoadFailed' });
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

    case 'FetchAllProjects':
      try {
        await fetchTasks("project", 0, 1000);
        // Store projects in a way accessible to parent project search
        // This is handled through the effect batching mechanism
      } catch (err) {
        console.error("Failed to fetch projects:", err);
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
