import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { AppState } from "../types/state.js";
import { hasMoreTasks, isProjectView, isSearching } from "../types/state.js";
import type { ViewName, TaskPointer, TaskWithPointer } from "../types/domain.js";
import { renderSearchBar } from "./components/SearchBar.js";
import { renderViewSelector, VIEW_LABELS } from "./components/ViewSelector.js";
import { renderCarousel3D, type CarouselCallbacks } from "./components/carousel/Carousel3D.js";
import { renderDetailCard, type DetailCardCallbacks } from "./components/DetailCard.js";
import { renderDebugPanel, renderDebugToggleButton, type DebugPanelCallbacks } from "./components/DebugPanel.js";
import type { SearchBarCallbacks } from "./components/SearchBar.js";
import type { ViewSelectorCallbacks } from "./components/ViewSelector.js";
import type { TaskCardCallbacks } from "./components/card/TaskCard.js";
import { ENABLE_DEBUG_MODE } from "./constants.js";
import { renderAtmosphere } from "./components/Atmosphere.js";

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
  readonly onDebugToggle: () => void;
  readonly onDebugParamChange: (param: keyof import("../types/state.js").Carousel3DParams, value: number | boolean) => void;
}



export function view(state: AppState, callbacks: AppCallbacks): VNode {
  const filteredTasks = state.taskList.tasks;
  const searching = isSearching(state);
  const projectView = isProjectView(state);

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
    onClose: callbacks.onCloseDetailCard,
  };

  const carouselCallbacks: CarouselCallbacks = {
    onRotate: callbacks.onCarouselRotate,
    onLoadMore: callbacks.onLoadMore,
  };

  const debugPanelCallbacks: DebugPanelCallbacks = {
    onToggle: callbacks.onDebugToggle,
    onParamChange: callbacks.onDebugParamChange,
  };

  // Project view doesn't have pagination - all tasks loaded via tree flattening
  const canLoadMore = !projectView && hasMoreTasks(state);
  const isLoadingMore = state.taskList.loadingMore;

  const appChildren = [
    // Atmospheric overhead glows - rendered first, sits between vignette (z:0) and content (z:50+)
    renderAtmosphere(),

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
        // Top section: NFS Carbon style layout
        h("div.top-section", {
          style: {
            display: "flex",
            flexDirection: "column",
            alignItems: "center",
            paddingTop: "24px",
            position: "relative",
            zIndex: "100",
            pointerEvents: "none",
          },
        }, [
          // Title
          h("div", {
            style: {
              fontFamily: "var(--font-display)",
              fontSize: "2.5rem",
              fontWeight: "700",
              letterSpacing: "0.08em",
              textTransform: "uppercase",
              color: "var(--text-primary)",
              textAlign: "center",
              marginBottom: "16px",
              pointerEvents: "auto",
            },
          }, "DWAYNE"),

          // View selector (NFS Carbon horizontal bar)
          projectView
            ? h("button.hover-brighten", {
                style: {
                  padding: "8px 24px",
                  borderRadius: "2px",
                  border: "1px solid rgba(255, 255, 255, 0.15)",
                  backgroundColor: "rgba(255, 255, 255, 0.05)",
                  color: "#ffffff",
                  fontWeight: "600",
                  cursor: "pointer",
                  fontSize: "0.875rem",
                  transition: "all 0.2s",
                  fontFamily: "var(--font-body)",
                  letterSpacing: "0.05em",
                  textTransform: "uppercase",
                  pointerEvents: "auto",
                },
                on: {
                  click: callbacks.onBackToView,
                },
              }, `← BACK TO ${(VIEW_LABELS[state.view.currentView] || 'All').toUpperCase()}`)
            : renderViewSelector(state.view.currentView, viewSelectorCallbacks),

          // Search bar
          h("div", {
            style: {
              marginTop: "20px",
              width: "100%",
              maxWidth: "500px",
              pointerEvents: "auto",
            },
          }, [
            renderSearchBar(state.view.searchQuery, searchBarCallbacks),
          ]),

          // Task count info
          h("div", {
            style: {
              fontSize: "0.7rem",
              color: "var(--text-secondary)",
              textAlign: "center",
              fontFamily: "var(--font-mono)",
              letterSpacing: "0.05em",
              textTransform: "uppercase",
              marginTop: "12px",
              pointerEvents: "auto",
            },
          }, projectView && searching
            ? `Found ${filteredTasks.length} tasks in project`
            : projectView
              ? `Viewing project (${state.taskList.tasks.length} tasks)`
              : searching
                ? `${filteredTasks.length} / ${state.taskList.tasks.length} tasks`
                : `${state.taskList.tasks.length} / ${state.taskList.totalCount.toLocaleString()} tasks`),

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
          ...(filteredTasks.length > 0 ? [
            h("div", {
              style: {
                width: "100%",
                height: "100%",
              },
            }, [
              renderCarousel3D(
                filteredTasks,
                state.carousel.rotation,
                taskCardCallbacks,
                carouselCallbacks,
                canLoadMore,
                isLoadingMore,
                state.taskList.pagesLoaded,
                state.debug.params
              ),
            ]),
          ] : []),
        ]),
        
        // Floating detail card (rendered outside top section to avoid layout jump)
        renderDetailCard(state.detail.selectedTask, state, detailCardCallbacks),
        
        // Debug panel and toggle (only if feature flag enabled)
        ...(ENABLE_DEBUG_MODE ? [
          renderDebugToggleButton(state.debug.enabled, debugPanelCallbacks),
          renderDebugPanel(state.debug.enabled, state.debug.params, debugPanelCallbacks),
        ] : []),
    ]),
  ];

  return h("div", appChildren);
}
