import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { AppState } from "../types/state.js";
import type { CommandBarMode } from "../types/state.js";
import { hasMoreTasks, isProjectView } from "../types/state.js";
import type { ActiveView } from "../types/domain.js";
import type { ViewName, TaskPointer, TaskWithPointer } from "../types/domain.js";
import type { FilePath, TaskIndex } from "../types/branded.js";

import { renderRail, type RailCallbacks } from "./components/Rail.js";
import { renderCommandBar, type CommandBarCallbacks } from "./components/CommandBar.js";
import { renderStatusBar, type StatusBarData } from "./components/StatusBar.js";
import { renderCarousel3D, type CarouselCallbacks } from "./components/carousel/Carousel3D.js";
import { renderDetailCard, type DetailCardCallbacks } from "./components/DetailCard.js";
import type { MutationCallbacks } from "./components/detail/MutationActions.js";
import { renderDebugPanel, renderDebugToggleButton, type DebugPanelCallbacks } from "./components/DebugPanel.js";
import type { TaskCardCallbacks } from "./components/card/TaskCard.js";
import { ENABLE_DEBUG_MODE } from "./constants.js";
import { renderAtmosphere } from "./components/Atmosphere.js";
import { colors, fonts, fontSize } from "./designSystem.js";
import { assertNever } from "../types/utils.js";

export interface AppCallbacks {
  // Shell navigation
  readonly onActiveViewChange: (view: ActiveView) => void;
  readonly onCommandBarModeChange: (mode: CommandBarMode) => void;

  // Search + capture
  readonly onSearchChange: (query: string) => void;
  readonly onClearSearch: () => void;
  readonly onCapture: (title: string) => void;

  // Legacy view system (used by carousel/garage internally)
  readonly onViewChange: (view: ViewName) => void;

  // Task interactions
  readonly onTaskClick: (task: TaskWithPointer) => void;
  readonly onCloseDetailCard: () => void;
  readonly onViewAllSubtasks: (pointer: TaskPointer) => void;
  readonly onClickParentProject: (parent: TaskWithPointer) => void;
  readonly onBackToView: () => void;

  // Carousel
  readonly onCarouselRotate: (delta: number) => void;
  readonly onLoadMore: () => void;

  // Debug
  readonly onDebugToggle: () => void;
  readonly onDebugParamChange: (param: keyof import("../types/state.js").Carousel3DParams, value: number | boolean) => void;

  // Mutations
  readonly onChangeKeyword: (file: FilePath, taskIndex: TaskIndex, keyword: string) => void;
}

function renderViewPlaceholder(label: string): VNode {
  return h("div.view-placeholder", {
    style: {
      display: "flex",
      alignItems: "center",
      justifyContent: "center",
      height: "100%",
      color: colors.grey,
      fontFamily: `'${fonts.body}', sans-serif`,
      fontSize: fontSize.lg,
      letterSpacing: "0.05em",
      textTransform: "uppercase",
      userSelect: "none",
    },
  }, `${label} view \u2014 coming soon`);
}

function computeInboxCount(state: AppState): number {
  // When viewing inbox, we have the total count available
  if (state.view.currentView === 'inbox') {
    return state.taskList.totalCount;
  }
  // Otherwise, no reliable count without a separate API call
  return 0;
}

function renderGarageView(state: AppState, callbacks: AppCallbacks): VNode {
  const filteredTasks = state.taskList.tasks;
  const projectView = isProjectView(state);
  const canLoadMore = !projectView && hasMoreTasks(state);
  const isLoadingMore = state.taskList.loadingMore;

  const taskCardCallbacks: TaskCardCallbacks = {
    onTaskClick: callbacks.onTaskClick,
  };

  const carouselCallbacks: CarouselCallbacks = {
    onRotate: callbacks.onCarouselRotate,
    onLoadMore: callbacks.onLoadMore,
  };

  return h("div.garage-view", {
    style: {
      position: "relative",
      width: "100%",
      height: "100%",
      overflow: "hidden",
    },
  }, [
    // Atmospheric effects (only in garage)
    renderAtmosphere(),

    // Carousel container
    ...(filteredTasks.length > 0 ? [
      h("div.carousel-container", {
        style: {
          position: "absolute",
          top: "0",
          left: "0",
          width: "100%",
          height: "100%",
          pointerEvents: "none",
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
          state.debug.params,
        ),
      ]),
    ] : []),

    // Task count overlay
    h("div.garage-info", {
      style: {
        position: "absolute",
        top: "16px",
        left: "50%",
        transform: "translateX(-50%)",
        fontFamily: `'${fonts.display}', sans-serif`,
        fontSize: fontSize.xxl,
        fontWeight: "700",
        letterSpacing: "0.08em",
        textTransform: "uppercase",
        color: colors.white,
        textAlign: "center",
        pointerEvents: "none",
        zIndex: "100",
      },
    }, "DWAYNE"),

    h("div.garage-stats", {
      style: {
        position: "absolute",
        top: "80px",
        left: "50%",
        transform: "translateX(-50%)",
        fontSize: fontSize.xs,
        color: colors.greyLight,
        fontFamily: `'${fonts.mono}', monospace`,
        letterSpacing: "0.05em",
        textTransform: "uppercase",
        pointerEvents: "none",
        zIndex: "100",
      },
    }, `${state.taskList.tasks.length} / ${state.taskList.totalCount.toLocaleString()} tasks`),
  ]);
}

function renderContentArea(state: AppState, callbacks: AppCallbacks): VNode {
  let content: VNode;

  switch (state.activeView) {
    case 'today':
      content = renderViewPlaceholder('Today');
      break;
    case 'inbox':
      content = renderViewPlaceholder('Inbox');
      break;
    case 'backlog':
      content = renderViewPlaceholder('Backlog');
      break;
    case 'lists':
      content = renderViewPlaceholder('Lists');
      break;
    case 'garage':
      content = renderGarageView(state, callbacks);
      break;
    default:
      assertNever(state.activeView);
  }

  return h("div.content-area", {
    style: {
      flex: "1",
      overflow: "hidden",
      position: "relative",
      backgroundColor: colors.void,
    },
  }, [content]);
}

export function view(state: AppState, callbacks: AppCallbacks): VNode {
  const inboxCount = computeInboxCount(state);

  const railCallbacks: RailCallbacks = {
    onViewChange: callbacks.onActiveViewChange,
  };

  const commandBarCallbacks: CommandBarCallbacks = {
    onCapture: callbacks.onCapture,
    onSearchChange: callbacks.onSearchChange,
    onClearSearch: callbacks.onClearSearch,
    onModeChange: callbacks.onCommandBarModeChange,
  };

  const statusBarData: StatusBarData = {
    taskCount: state.taskList.totalCount,
    inboxCount,
  };

  const mutationCallbacks: MutationCallbacks = {
    onChangeKeyword: callbacks.onChangeKeyword,
  };

  const detailCardCallbacks: DetailCardCallbacks = {
    onTaskClick: callbacks.onTaskClick,
    onViewAllSubtasks: callbacks.onViewAllSubtasks,
    onClickParentProject: callbacks.onClickParentProject,
    onClose: callbacks.onCloseDetailCard,
    mutation: mutationCallbacks,
  };

  const debugPanelCallbacks: DebugPanelCallbacks = {
    onToggle: callbacks.onDebugToggle,
    onParamChange: callbacks.onDebugParamChange,
  };

  return h("div.shell", {
    style: {
      display: "flex",
      flexDirection: "column",
      height: "100vh",
      width: "100vw",
      overflow: "hidden",
      backgroundColor: colors.void,
    },
  }, [
    // Command bar (top)
    renderCommandBar(state.commandBarMode, state.view.searchQuery, commandBarCallbacks),

    // Middle: rail + content area
    h("div.shell-body", {
      style: {
        display: "flex",
        flex: "1",
        overflow: "hidden",
      },
    }, [
      renderRail(state.activeView, inboxCount, railCallbacks),
      renderContentArea(state, callbacks),
    ]),

    // Status bar (bottom)
    renderStatusBar(statusBarData),

    // Detail card modal (floating, rendered outside normal flow)
    renderDetailCard(state.detail.selectedTask, state, detailCardCallbacks),

    // Debug panel and toggle (only if feature flag enabled)
    ...(ENABLE_DEBUG_MODE ? [
      renderDebugToggleButton(state.debug.enabled, debugPanelCallbacks),
      renderDebugPanel(state.debug.enabled, state.debug.params, debugPanelCallbacks),
    ] : []),
  ]);
}
