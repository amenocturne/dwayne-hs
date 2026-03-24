import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { AppState } from "../types/state.js";
import type { CommandBarMode } from "../types/state.js";
import { hasMoreTasks, isProjectView } from "../types/state.js";
import type { ActiveView, OrgTime } from "../types/domain.js";
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
import { renderTaskRow, type QuickAction } from "./components/TaskRow.js";
import { renderTaskDetail, type TaskDetailCallbacks } from "./components/TaskDetail.js";
import { renderBacklogView, type BacklogViewCallbacks } from "./views/BacklogView.js";
import { renderListsView, type ListsViewCallbacks } from "./views/ListsView.js";
import { colors, fonts, fontSize, spacing, fontWeight, transitions } from "./designSystem.js";
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

  // Detail panel (kept for garage)
  readonly onDetailPanelOpen: (task: TaskWithPointer) => void;
  readonly onDetailPanelClose: () => void;
  readonly onChangePriority: (file: FilePath, taskIndex: TaskIndex, priority: number | null) => void;
  readonly onChangeTitle: (file: FilePath, taskIndex: TaskIndex, title: string) => void;
  readonly onChangeDescription: (file: FilePath, taskIndex: TaskIndex, description: string) => void;
  readonly onChangeTags: (file: FilePath, taskIndex: TaskIndex, tags: ReadonlyArray<string>) => void;
  readonly onChangeScheduled: (file: FilePath, taskIndex: TaskIndex, scheduled: OrgTime | null) => void;
  readonly onChangeDeadline: (file: FilePath, taskIndex: TaskIndex, deadline: OrgTime | null) => void;

  // Inline expand/collapse
  readonly onToggleTaskExpand: (task: TaskWithPointer) => void;
  readonly onExpandAll: (tasks: ReadonlyArray<TaskWithPointer>) => void;
  readonly onCollapseAll: () => void;

  // Carousel
  readonly onCarouselRotate: (delta: number) => void;
  readonly onLoadMore: () => void;

  // Debug
  readonly onDebugToggle: () => void;
  readonly onDebugParamChange: (param: keyof import("../types/state.js").Carousel3DParams, value: number | boolean) => void;

  // Mutations
  readonly onChangeKeyword: (file: FilePath, taskIndex: TaskIndex, keyword: string) => void;

  // List filters
  readonly onListFilterToggle: (tag: string) => void;

  // Backlog section collapse
  readonly onBacklogSectionToggle: (sectionId: string) => void;
}

// --- Quick action presets per view ---

const INBOX_QUICK_ACTIONS: ReadonlyArray<QuickAction> = [
  { label: "[T]", keyword: "TODAY" },
  { label: "[O]", keyword: "TODO" },
  { label: "[S]", keyword: "SOMEDAY" },
  { label: "[X]", keyword: "TRASH" },
];

const TODAY_QUICK_ACTIONS: ReadonlyArray<QuickAction> = [];

// --- View header ---

function renderExpandCollapseButton(
  hasExpanded: boolean,
  onExpandAll: () => void,
  onCollapseAll: () => void,
): VNode {
  const isExpanded = hasExpanded;
  const label = isExpanded ? "\u25B2 Collapse" : "\u25BC Expand";
  return h("button.expand-collapse-btn", {
    style: {
      padding: `2px ${spacing.sm}`,
      fontFamily: `'${fonts.mono}', monospace`,
      fontSize: fontSize.xs,
      color: colors.grey,
      backgroundColor: "transparent",
      border: `1px solid ${colors.grey}`,
      borderRadius: "3px",
      cursor: "pointer",
      transition: `all ${transitions.fast}`,
      whiteSpace: "nowrap",
    },
    on: {
      click: () => isExpanded ? onCollapseAll() : onExpandAll(),
      mouseenter: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        el.style.color = colors.white;
        el.style.borderColor = colors.white;
      },
      mouseleave: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        el.style.color = colors.grey;
        el.style.borderColor = colors.grey;
      },
    },
  }, label);
}

function renderViewHeader(
  title: string,
  count: number,
  expandCollapseNode?: VNode,
): VNode {
  return h("div.view-header", {
    style: {
      display: "flex",
      justifyContent: "space-between",
      alignItems: "baseline",
      padding: `${spacing.lg} ${spacing.lg} ${spacing.sm}`,
      borderBottom: `1px solid rgba(255, 255, 255, 0.05)`,
      gap: spacing.sm,
    },
  }, [
    h("h1", {
      style: {
        margin: "0",
        fontFamily: `'${fonts.display}', sans-serif`,
        fontSize: fontSize.xl,
        fontWeight: fontWeight.bold,
        letterSpacing: "0.05em",
        textTransform: "uppercase",
        color: colors.white,
      },
    }, title),
    h("div.view-header-right", {
      style: {
        display: "flex",
        alignItems: "center",
        gap: spacing.sm,
        marginLeft: "auto",
      },
    }, [
      ...(expandCollapseNode ? [expandCollapseNode] : []),
      h("span", {
        style: {
          fontFamily: `'${fonts.mono}', monospace`,
          fontSize: fontSize.xs,
          color: colors.grey,
        },
      }, `${count} tasks`),
    ]),
  ]);
}

// --- Empty state ---

function renderEmptyState(message: string): VNode {
  return h("div.empty-state", {
    style: {
      display: "flex",
      alignItems: "center",
      justifyContent: "center",
      height: "200px",
      color: colors.grey,
      fontFamily: `'${fonts.body}', sans-serif`,
      fontSize: fontSize.md,
      fontStyle: "italic",
    },
  }, message);
}

// --- Task list view (reusable for Today, Inbox, Backlog) ---

function renderTaskListView(
  title: string,
  tasks: ReadonlyArray<TaskWithPointer>,
  totalCount: number,
  loading: boolean,
  showKeyword: boolean,
  quickActions: ReadonlyArray<QuickAction>,
  emptyMessage: string,
  callbacks: AppCallbacks,
  state: AppState,
  focusedTaskIndex: number | null = null,
  animatingOutTasks: ReadonlyArray<string> = [],
): VNode {
  const expandedTasks = state.expandedTasks;
  const hasExpanded = expandedTasks.length > 0;

  const detailCallbacks: TaskDetailCallbacks = {
    onChangeKeyword: callbacks.onChangeKeyword,
    onChangePriority: callbacks.onChangePriority,
    onChangeTitle: callbacks.onChangeTitle,
    onChangeDescription: callbacks.onChangeDescription,
    onChangeTags: callbacks.onChangeTags,
    onChangeScheduled: callbacks.onChangeScheduled,
    onChangeDeadline: callbacks.onChangeDeadline,
  };

  const expandCollapseBtn = tasks.length > 0
    ? renderExpandCollapseButton(
        hasExpanded,
        () => callbacks.onExpandAll(tasks),
        () => callbacks.onCollapseAll(),
      )
    : undefined;

  if (loading) {
    return h("div.list-view", {
      style: {
        display: "flex",
        flexDirection: "column",
        height: "100%",
      },
    }, [
      renderViewHeader(title, 0),
      h("div.loading", {
        style: {
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          flex: "1",
          color: colors.grey,
          fontFamily: `'${fonts.mono}', monospace`,
          fontSize: fontSize.sm,
        },
      }, "Loading..."),
    ]);
  }

  return h("div.list-view", {
    style: {
      display: "flex",
      flexDirection: "column",
      height: "100%",
    },
  }, [
    renderViewHeader(title, totalCount, expandCollapseBtn),
    tasks.length === 0
      ? renderEmptyState(emptyMessage)
      : h("div.task-list", {
          style: {
            flex: "1",
            overflowY: "auto",
            overflowX: "hidden",
          },
        }, tasks.map((twp, idx) => {
          const taskKey = `${twp.pointer.file}-${twp.pointer.taskIndex}`;
          const isExp = expandedTasks.includes(taskKey);
          return renderTaskRow({
            task: twp,
            showKeyword,
            quickActions: quickActions as QuickAction[],
            onTaskClick: (t) => callbacks.onToggleTaskExpand(t),
            onQuickAction: (t, keyword) => callbacks.onChangeKeyword(t.pointer.file, t.pointer.taskIndex, keyword),
            isFocused: focusedTaskIndex === idx,
            isAnimatingOut: animatingOutTasks.includes(taskKey),
            isExpanded: isExp,
            expandedContent: isExp ? renderTaskDetail(twp, state, detailCallbacks) : null,
          });
        })),
  ]);
}

function computeInboxCount(state: AppState): number {
  // When viewing inbox, use the live total count
  if (state.activeView === 'inbox') {
    return state.taskList.totalCount;
  }
  // Otherwise, use the stored inbox count (fetched at startup and updated on capture)
  return state.inboxCount;
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
      content = renderTaskListView(
        "Today",
        state.taskList.tasks,
        state.taskList.totalCount,
        state.taskList.loading,
        false,
        TODAY_QUICK_ACTIONS,
        "Nothing committed for today. Pull tasks from the backlog, or capture something new.",
        callbacks,
        state,
        state.focusedTaskIndex,
        state.animatingOutTasks,
      );
      break;
    case 'inbox':
      content = renderTaskListView(
        "Inbox",
        state.taskList.tasks,
        state.taskList.totalCount,
        state.taskList.loading,
        false,
        INBOX_QUICK_ACTIONS,
        "Inbox zero. Enjoy it.",
        callbacks,
        state,
        state.focusedTaskIndex,
        state.animatingOutTasks,
      );
      break;
    case 'backlog': {
      const backlogCallbacks: BacklogViewCallbacks = {
        onTaskClick: (t) => callbacks.onToggleTaskExpand(t),
        onQuickAction: (t, keyword) => callbacks.onChangeKeyword(t.pointer.file, t.pointer.taskIndex, keyword),
        onToggleSection: callbacks.onBacklogSectionToggle,
        focusedTaskIndex: state.focusedTaskIndex,
        animatingOutTasks: state.animatingOutTasks,
        expandedTasks: state.expandedTasks,
        state,
        detailCallbacks: {
          onChangeKeyword: callbacks.onChangeKeyword,
          onChangePriority: callbacks.onChangePriority,
          onChangeTitle: callbacks.onChangeTitle,
          onChangeDescription: callbacks.onChangeDescription,
          onChangeTags: callbacks.onChangeTags,
          onChangeScheduled: callbacks.onChangeScheduled,
          onChangeDeadline: callbacks.onChangeDeadline,
        },
        onExpandAll: (tasks) => callbacks.onExpandAll(tasks),
        onCollapseAll: () => callbacks.onCollapseAll(),
      };
      content = renderBacklogView(
        state.taskList.tasks,
        state.taskList.totalCount,
        state.taskList.loading,
        state.backlogCollapsed,
        backlogCallbacks,
      );
      break;
    }
    case 'lists': {
      const listsCallbacks: ListsViewCallbacks = {
        onCardClick: (t) => callbacks.onToggleTaskExpand(t),
        onToggleFilter: callbacks.onListFilterToggle,
        expandedTasks: state.expandedTasks,
        state,
        detailCallbacks: {
          onChangeKeyword: callbacks.onChangeKeyword,
          onChangePriority: callbacks.onChangePriority,
          onChangeTitle: callbacks.onChangeTitle,
          onChangeDescription: callbacks.onChangeDescription,
          onChangeTags: callbacks.onChangeTags,
          onChangeScheduled: callbacks.onChangeScheduled,
          onChangeDeadline: callbacks.onChangeDeadline,
        },
        onExpandAll: (tasks) => callbacks.onExpandAll(tasks),
        onCollapseAll: () => callbacks.onCollapseAll(),
      };
      content = renderListsView(
        state.taskList.tasks,
        state.taskList.totalCount,
        state.taskList.loading,
        state.listFilters,
        listsCallbacks,
      );
      break;
    }
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
    // Main body: rail (full height) + right column (command bar + content + status bar)
    h("div.shell-body", {
      style: {
        display: "flex",
        flex: "1",
        overflow: "hidden",
      },
    }, [
      renderRail(state.activeView, inboxCount, railCallbacks),

      // Right column: command bar + content + status bar
      h("div.shell-right", {
        style: {
          display: "flex",
          flexDirection: "column",
          flex: "1",
          overflow: "hidden",
        },
      }, [
        renderCommandBar(state.commandBarMode, state.view.searchQuery, commandBarCallbacks),
        renderContentArea(state, callbacks),
        renderStatusBar(statusBarData),
      ]),
    ]),

    // Detail card modal (only in garage view)
    ...(state.activeView === 'garage'
      ? [renderDetailCard(state.detail.selectedTask, state, detailCardCallbacks)]
      : []),

    // Debug panel and toggle (only if feature flag enabled)
    ...(ENABLE_DEBUG_MODE ? [
      renderDebugToggleButton(state.debug.enabled, debugPanelCallbacks),
      renderDebugPanel(state.debug.enabled, state.debug.params, debugPanelCallbacks),
    ] : []),
  ]);
}
