
/**
 * Root View Function
 *
 * Pure function: AppState => VNode
 * Composes all components into the complete application view.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { AppState } from "../types/state.js";
import type { ViewName, TaskPointer, TaskWithPointer } from "../types/domain.js";
import { renderSearchBar } from "./components/SearchBar.js";
import { renderViewSelector, VIEW_LABELS } from "./components/ViewSelector.js";
import { renderTaskGrid } from "./components/TaskCard.js";
import { renderLoadingIndicator } from "./components/LoadingIndicator.js";
import { renderSidebar } from "./components/Sidebar.js";
import type { SearchBarCallbacks } from "./components/SearchBar.js";
import type { ViewSelectorCallbacks } from "./components/ViewSelector.js";
import type { TaskCardCallbacks } from "./components/TaskCard.js";
import type { SidebarCallbacks } from "./components/Sidebar.js";

export interface AppCallbacks {
  readonly onSearchChange: (query: string) => void;
  readonly onClearSearch: () => void;
  readonly onViewChange: (view: ViewName) => void;
  readonly onTaskClick: (task: TaskWithPointer) => void;
  readonly onCloseSidebar: () => void;
  readonly onViewAllSubtasks: (pointer: TaskPointer) => void;
  readonly onClickParentProject: (parent: TaskWithPointer) => void;
  readonly onBackToView: () => void;
}

/**
 * Root view function.
 * Pure transformation: (AppState, AppCallbacks) => VNode
 */
export function view(state: AppState, callbacks: AppCallbacks): VNode {
  const filteredTasks = state.tasks;
  const isSearching = state.searchQuery.trim() !== "";
  const hasResults = filteredTasks.length > 0;
  const isProjectView = state.projectPointer !== null;

  const searchBarCallbacks: SearchBarCallbacks = {
    onSearchChange: callbacks.onSearchChange,
    onClearSearch: callbacks.onClearSearch,
  };

  const viewSelectorCallbacks: ViewSelectorCallbacks = {
    onViewChange: callbacks.onViewChange,
  };

  const taskCardCallbacks: TaskCardCallbacks = {
    onTaskClick: callbacks.onTaskClick,
  };

  const sidebarCallbacks: SidebarCallbacks = {
    onTaskClick: callbacks.onTaskClick,
    onClose: callbacks.onCloseSidebar,
    onViewAllSubtasks: callbacks.onViewAllSubtasks,
    onClickParentProject: callbacks.onClickParentProject,
  };

  const appChildren = [
    h(
      "div.app",
      {
        style: {
          padding: "40px 20px",
          maxWidth: "1400px",
          margin: "0 auto",
        },
      },
      [
        h(
          "header",
          {
            style: {
              marginBottom: "32px",
              textAlign: "center",
            },
          },
          [
            h(
              "h1",
              {
                style: {
                  margin: "0 0 8px 0",
                  fontSize: "2.5rem",
                  fontWeight: "700",
                  color: "var(--text-primary)",
                },
              },
              "Dwayne Task Manager",
            ),
            h(
              "p",
              {
                style: {
                  margin: "0 0 24px 0",
                  fontSize: "1rem",
                  color: "var(--text-secondary)",
                },
              },
              state.loading
                ? "Loading..."
                : state.error
                  ? `Error: ${state.error}`
                  : isProjectView && isSearching
                    ? `Found ${filteredTasks.length} tasks in project`
                    : isProjectView
                      ? `Viewing project tasks (${state.tasks.length} tasks)`
                      : isSearching
                        ? `Showing ${filteredTasks.length} of ${state.tasks.length} tasks`
                        : `${state.tasks.length} of ${state.totalCount.toLocaleString()} tasks in ${VIEW_LABELS[state.currentView] || 'All'}`,
            ),
          ],
        ),
        renderSearchBar(state.searchQuery, searchBarCallbacks),
        isProjectView
          ? h("div", {
              style: {
                display: "flex",
                justifyContent: "center",
                marginBottom: "24px",
              },
            }, [
              h("button", {
                style: {
                  padding: "8px 16px",
                  borderRadius: "6px",
                  border: "1px solid var(--link-color)",
                  backgroundColor: "rgba(100, 108, 255, 0.1)",
                  color: "var(--link-color)",
                  fontWeight: "600",
                  cursor: "pointer",
                  fontSize: "0.875rem",
                  transition: "all 0.2s",
                },
                on: {
                  click: callbacks.onBackToView,
                  mouseenter: (e: Event) => {
                    const target = e.target as HTMLElement;
                    target.style.backgroundColor = "rgba(100, 108, 255, 0.2)";
                  },
                  mouseleave: (e: Event) => {
                    const target = e.target as HTMLElement;
                    target.style.backgroundColor = "rgba(100, 108, 255, 0.1)";
                  },
                },
              }, `← Back to ${VIEW_LABELS[state.currentView] || 'All'}`),
            ])
          : renderViewSelector(state.currentView, viewSelectorCallbacks),
        state.error
          ? h(
              "div.error-message",
              {
                style: {
                  padding: "16px",
                  backgroundColor: "#fee",
                  border: "1px solid #fcc",
                  borderRadius: "8px",
                  color: "#c00",
                  textAlign: "center",
                  marginBottom: "20px",
                },
              },
              [
                h("strong", "Error: "),
                state.error,
                h("br"),
                h(
                  "small",
                  "Make sure the API server is running on http://localhost:8080",
                ),
              ],
            )
          : state.loading
            ? h(
                "div.loading",
                {
                  style: {
                    textAlign: "center",
                    padding: "60px 20px",
                    color: "var(--text-secondary)",
                    fontSize: "1.125rem",
                  },
                },
                "Loading tasks...",
              )
            : state.tasks.length === 0
              ? h(
                  "div.empty-state",
                  {
                    style: {
                      textAlign: "center",
                      padding: "60px 20px",
                      color: "var(--text-secondary)",
                      fontSize: "1.125rem",
                    },
                  },
                  `No tasks in ${VIEW_LABELS[state.currentView] || 'All'} view`,
                )
              : !hasResults && isSearching
                ? h(
                    "div.empty-state",
                    {
                      style: {
                        textAlign: "center",
                        padding: "60px 20px",
                        color: "var(--text-secondary)",
                        fontSize: "1.125rem",
                      },
                    },
                    "No tasks match your search",
                  )
                : h("div", [
                    renderTaskGrid(filteredTasks, taskCardCallbacks),
                    ...(state.loadingMore ? [renderLoadingIndicator()] : []),
                  ]),
      ],
    ),
  ];

  if (state.selectedTask) {
    appChildren.push(renderSidebar(state.selectedTask, state, sidebarCallbacks)!);
  }

  return h("div", appChildren);
}
