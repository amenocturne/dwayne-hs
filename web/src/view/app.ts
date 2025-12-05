import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { AppState } from "../types/state.js";
import type { ViewName, TaskPointer, TaskWithPointer } from "../types/domain.js";
import { renderSearchBar } from "./components/SearchBar.js";
import { renderViewSelector, VIEW_LABELS } from "./components/ViewSelector.js";
import { renderTaskGrid, CarouselCallbacks } from "./components/TaskCard.js";
import { renderLoadingIndicator } from "./components/LoadingIndicator.js";
import { renderDetailCard, type DetailCardCallbacks } from "./components/DetailCard.js";
import type { SearchBarCallbacks } from "./components/SearchBar.js";
import type { ViewSelectorCallbacks } from "./components/ViewSelector.js";
import type { TaskCardCallbacks } from "./components/TaskCard.js";

export interface AppCallbacks {
  readonly onSearchChange: (query: string) => void;
  readonly onClearSearch: () => void;
  readonly onViewChange: (view: ViewName) => void;
  readonly onTaskClick: (task: TaskWithPointer) => void;
  readonly onCloseDetailCard: () => void;
  readonly onViewAllSubtasks: (pointer: TaskPointer) => void;
  readonly onClickParentProject: (parent: TaskWithPointer) => void;
  readonly onBackToView: () => void;
  readonly onCarouselRotate: (delta: number) => void;
  readonly onLoadMore: () => void;
}

function createHoverStyle(normalBg: string, hoverBg: string): Record<string, (e: Event) => void> {
  return {
    mouseenter: (e: Event) => ((e.target as HTMLElement).style.backgroundColor = hoverBg),
    mouseleave: (e: Event) => ((e.target as HTMLElement).style.backgroundColor = normalBg),
  };
}

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

  const detailCardCallbacks: DetailCardCallbacks = {
    onTaskClick: callbacks.onTaskClick,
    onViewAllSubtasks: callbacks.onViewAllSubtasks,
    onClickParentProject: callbacks.onClickParentProject,
  };

  const carouselCallbacks: CarouselCallbacks = {
    onRotate: callbacks.onCarouselRotate,
    onLoadMore: callbacks.onLoadMore,
  };

  // Project view doesn't have pagination - all tasks loaded via tree flattening
  const canLoadMore = !isProjectView && state.hasMore;
  const isLoadingMore = state.loadingMore;

  const appChildren = [
    h(
      "div.app",
      {
        style: {
          display: "flex",
          flexDirection: "column",
          height: "100vh",
          overflow: "hidden",
        },
      },
      [
        // Top section: 3 panels side by side
        h("div.top-section", {
          style: {
            display: "flex",
            height: "50%",
            flexShrink: "0",
            position: "relative",
            zIndex: "100",
            pointerEvents: "auto",
            // borderBottom: "2px solid var(--card-border)",
          },
        }, [
          // Left panel: View selector
          h("div.left-panel", {
            style: {
              width: "20%",
              // backgroundColor: "var(--bg-secondary)",
              // borderRight: "2px solid var(--card-border)",
              overflowY: "auto",
              padding: "24px",
              display: "flex",
              flexDirection: "column",
              gap: "16px",
              pointerEvents: "auto",
            },
          }, [
            h("div", {
              style: {
                fontFamily: "var(--font-display)",
                fontSize: "0.75rem",
                fontWeight: "600",
                letterSpacing: "0.1em",
                textTransform: "uppercase",
                color: "var(--text-secondary)",
                marginBottom: "-1em",
              },
            }, "Status"),
            h("div", {
              style: {
                fontSize: "0.875rem",
                color: "var(--text-primary)",
                marginBottom: "16px",
              },
            }, state.loading
              ? "⏳ Loading..."
              : state.error
                ? `Error: ${state.error}`
                : `Online`),
            isProjectView
              ? h("button", {
                  style: {
                    padding: "8px 16px",
                    borderRadius: "6px",
                    border: "1px solid var(--link-color)",
                    backgroundColor: "rgba(0, 229, 255, 0.1)",
                    color: "var(--link-color)",
                    fontWeight: "600",
                    cursor: "pointer",
                    fontSize: "0.875rem",
                    transition: "all 0.2s",
                  },
                  on: {
                    click: callbacks.onBackToView,
                    ...createHoverStyle("rgba(0, 229, 255, 0.1)", "rgba(0, 229, 255, 0.2)"),
                  },
                }, `← Back to ${VIEW_LABELS[state.currentView] || 'All'}`)
              : renderViewSelector(state.currentView, viewSelectorCallbacks),
          ]),

          // Middle panel: Search bar
          h("div.middle-panel", {
            style: {
              flex: "1",
              backgroundColor: "var(--bg-primary)",
              padding: "24px",
              display: "flex",
              flexDirection: "column",
              justifyContent: "center",
              gap: "16px",
              pointerEvents: "auto",
            },
          }, [
            h("div", {
              style: {
                fontFamily: "var(--font-display)",
                fontSize: "2rem",
                fontWeight: "700",
                letterSpacing: "0.05em",
                textTransform: "uppercase",
                color: "var(--text-primary)",
                textAlign: "center",
              },
            }, "DWAYNE"),
            renderSearchBar(state.searchQuery, searchBarCallbacks),
            h("div", {
              style: {
                fontSize: "0.75rem",
                color: "var(--text-secondary)",
                textAlign: "center",
                fontFamily: "var(--font-display)",
                letterSpacing: "0.08em",
                textTransform: "uppercase",
              },
            }, isProjectView && isSearching
              ? `Found ${filteredTasks.length} tasks in project`
              : isProjectView
                ? `Viewing project (${state.tasks.length} tasks)`
                : isSearching
                  ? `${filteredTasks.length} / ${state.tasks.length} tasks`
                  : `${state.tasks.length} / ${state.totalCount.toLocaleString()} tasks in ${VIEW_LABELS[state.currentView] || 'All'}`),
          ]),
          
          // Right panel: Placeholder to reserve space for floating card
          h("div.right-panel-placeholder", {
            style: {
              width: "400px",
              flexShrink: "0",
            },
          }),
        ]),

        // Bottom section: Full-screen carousel
        h("div.carousel-section", {
          style: {
            position: "absolute",
            top: "0",
            left: "0",
            width: "100%",
            height: "100%",
            overflow: "hidden",
            zIndex: "50",
            pointerEvents: "none",
          },
        }, [
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
                    margin: "20px",
                    maxWidth: "600px",
                    marginLeft: "auto",
                    marginRight: "auto",
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
                  : h("div", {
                      style: {
                        width: "100%",
                        height: "100%",
                      },
                    }, [
                      renderTaskGrid(
                        filteredTasks,
                        state.carouselRotation,
                        taskCardCallbacks,
                        carouselCallbacks,
                        canLoadMore,
                        isLoadingMore,
                        state.pagesLoaded
                      ),
                      ...(state.loadingMore ? [renderLoadingIndicator()] : []),
                    ]),
        ]),
        
        // Floating detail card (rendered outside top section to avoid layout jump)
        renderDetailCard(state.selectedTask, state, detailCardCallbacks),
    ]),
  ];

  return h("div", appChildren);
}
