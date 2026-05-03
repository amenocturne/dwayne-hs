import { init } from "snabbdom/build/init.js";
import { classModule } from "snabbdom/build/modules/class.js";
import { propsModule } from "snabbdom/build/modules/props.js";
import { styleModule } from "snabbdom/build/modules/style.js";
import { eventListenersModule } from "snabbdom/build/modules/eventlisteners.js";
import { attributesModule } from "snabbdom/build/modules/attributes.js";
import type { VNode } from "snabbdom/build/vnode.js";

import type { AppState } from "./types/state.js";
import type { ActiveView, ViewName, TaskPointer, TaskWithPointer } from "./types/domain.js";
import { view } from "./view/app.js";
import type { AppCallbacks } from "./view/app.js";
import { createStore } from "./state/store.js";
import type { Dispatch } from "./state/effects.js";
import { getBacklogVisibleTasks } from "./view/views/BacklogView.js";

const initialState: AppState = {
  activeView: 'today',
  commandBarMode: 'capture',
  detailPanel: {
    open: false,
    task: null,
  },
  expandedTasks: [],
  taskList: {
    tasks: [],
    loading: true,
    loadingMore: false,
    offset: 0,
    pagesLoaded: 0,
    totalCount: 0,
  },
  view: {
    currentView: "all",
    searchQuery: "",
    projectPointer: null,
  },
  detail: {
    selectedTask: null,
    parentProject: null,
    loadingParentProject: false,
    parentProjectRequestId: 0,
    projectTree: null,
    loadingProjectTree: false,
    projectTreeRequestId: 0,
  },
  carousel: {
    rotation: 0,
    targetRotation: 0,
  },
  debug: {
    enabled: false,
    params: {
      radius: 2300,
      perspective: 1000,
      rotationSpeed: 0.3,
      anglePerCard: 15,
      visibleAngleRange: 60,
      fadeTransitionAngle: 10,
      rotateX: 0,
      rotateY: 61,
      rotateZ: -90,
      perspectiveOriginY: 110,
      originX: 0,
      originY: 360,
      originZ: 220,
      cardScale: 1.7,
      showDebugDots: true,
    },
  },
  error: null,
  listFilters: [],
  backlogCollapsed: [],
  focusedTaskIndex: null,
  inboxCount: 0,
  lastCapturedTask: null,
  animatingOutTasks: [],
};

const patch = init([
  classModule,
  propsModule,
  styleModule,
  eventListenersModule,
  attributesModule,
]);

let vnode: VNode | Element = document.getElementById("app")!;

function render(state: AppState): void {
  vnode = patch(vnode, view(state, callbacks));
}

const store = createStore(initialState, render);
const dispatch: Dispatch = store.dispatch;

let debounceTimeout: ReturnType<typeof setTimeout> | null = null;

function debouncedSearch(query: string): void {
  if (debounceTimeout !== null) {
    clearTimeout(debounceTimeout);
  }

  debounceTimeout = setTimeout(() => {
    if (query.trim() === "") {
      dispatch({ type: 'SearchCleared' });
    } else {
      dispatch({ type: 'GlobalSearchStarted', query });
    }
    debounceTimeout = null;
  }, 200);
}

const callbacks: AppCallbacks = {
  // Shell navigation
  onActiveViewChange: (activeView: ActiveView) => {
    dispatch({ type: 'ActiveViewChanged', activeView });
    // Load tasks via the appropriate backend view
    const viewMap: Record<ActiveView, ViewName> = {
      today: 'today',
      inbox: 'inbox',
      backlog: 'all',
      lists: 'list',
      garage: 'all',
    };
    dispatch({ type: 'ViewChanged', view: viewMap[activeView] });
  },
  onCommandBarModeChange: (mode) => {
    dispatch({ type: 'CommandBarModeChanged', mode });
  },

  // Search + capture
  onSearchChange: (query: string) => {
    dispatch({ type: 'SearchQueryChanged', query });
    debouncedSearch(query);
  },
  onClearSearch: () => dispatch({ type: 'SearchCleared' }),
  onCapture: (title) => dispatch({ type: 'CaptureRequested', title }),

  // Legacy view system
  onViewChange: (newView) => dispatch({ type: 'ViewChanged', view: newView }),

  // Task interactions
  onTaskClick: (taskWithPointer) => dispatch({ type: 'TaskClicked', task: taskWithPointer }),
  onCloseDetailCard: () => dispatch({ type: 'DetailCardClosed' }),
  onViewAllSubtasks: (pointer: TaskPointer) => dispatch({ type: 'ProjectViewRequested', pointer }),
  onClickParentProject: (parent: TaskWithPointer) => dispatch({ type: 'TaskClicked', task: parent }),
  onBackToView: () => dispatch({ type: 'BackToViewRequested' }),

  // Carousel
  onCarouselRotate: (delta: number) => {
    dispatch({ type: 'CarouselRotate', delta });
  },
  onLoadMore: () => {
    dispatch({ type: 'LoadMoreStarted' });
  },

  // Debug
  onDebugToggle: () => {
    dispatch({ type: 'DebugToggled' });
  },
  onDebugParamChange: (param, value) => {
    dispatch({ type: 'DebugParamChanged', param, value });
  },

  // Detail panel (kept for garage)
  onDetailPanelOpen: (task) => {
    dispatch({ type: 'DetailPanelOpened', task });
  },
  onDetailPanelClose: () => {
    dispatch({ type: 'DetailPanelClosed' });
  },

  // Inline expand/collapse
  onToggleTaskExpand: (task) => {
    const taskKey = `${task.pointer.file}-${task.pointer.taskIndex}`;
    const state = store.getState();
    if (state.expandedTasks.includes(taskKey)) {
      dispatch({ type: 'TaskCollapsed', taskKey });
    } else {
      dispatch({ type: 'TaskExpanded', taskKey });
    }
  },
  onExpandAll: (tasks) => {
    const taskKeys = tasks.map((t) => `${t.pointer.file}-${t.pointer.taskIndex}`);
    dispatch({ type: 'AllTasksExpanded', taskKeys });
  },
  onCollapseAll: () => {
    dispatch({ type: 'AllTasksCollapsed' });
  },
  onChangePriority: (file, taskIndex, priority) => {
    dispatch({ type: 'ChangePriorityRequested', file, taskIndex, priority });
  },
  onChangeTitle: (file, taskIndex, title) => {
    dispatch({ type: 'ChangeTitleRequested', file, taskIndex, title });
  },
  onChangeDescription: (file, taskIndex, description) => {
    dispatch({ type: 'ChangeDescriptionRequested', file, taskIndex, description });
  },
  onChangeTags: (file, taskIndex, tags) => {
    dispatch({ type: 'ChangeTagsRequested', file, taskIndex, tags });
  },
  onChangeScheduled: (file, taskIndex, scheduled) => {
    dispatch({ type: 'ChangeScheduledRequested', file, taskIndex, scheduled });
  },
  onChangeDeadline: (file, taskIndex, deadline) => {
    dispatch({ type: 'ChangeDeadlineRequested', file, taskIndex, deadline });
  },

  // Mutations
  onChangeKeyword: (file, taskIndex, keyword) => {
    dispatch({ type: 'ChangeKeywordRequested', file, taskIndex, keyword });
  },

  // List filters
  onListFilterToggle: (tag) => {
    dispatch({ type: 'ListFilterToggled', tag });
  },

  // Backlog section collapse
  onBacklogSectionToggle: (sectionId) => {
    dispatch({ type: 'BacklogSectionToggled', sectionId });
  },
};

function isInputFocused(): boolean {
  const active = document.activeElement;
  if (!active) return false;
  const tag = active.tagName;
  return tag === "INPUT" || tag === "TEXTAREA" || (active as HTMLElement).isContentEditable;
}

function focusCommandBarInput(mode: 'capture' | 'search'): void {
  dispatch({ type: 'CommandBarModeChanged', mode });
  // Focus the input after state update triggers re-render
  requestAnimationFrame(() => {
    const input = document.querySelector('.command-bar-input') as HTMLInputElement | null;
    if (input) {
      input.focus();
    }
  });
}

const ACTIVE_VIEW_KEYS: Record<string, ActiveView> = {
  '1': 'today',
  '2': 'inbox',
  '3': 'backlog',
  '4': 'lists',
  '5': 'garage',
};

/**
 * Returns the flat ordered list of visible tasks for the current view.
 * For today/inbox, this is just state.taskList.tasks.
 * For backlog, this is the section-organized visible subset.
 */
function getVisibleTasks(state: AppState): ReadonlyArray<TaskWithPointer> {
  if (state.activeView === 'backlog') {
    return getBacklogVisibleTasks(state.taskList.tasks, state.backlogCollapsed);
  }
  return state.taskList.tasks;
}

// Valid keyboard shortcuts per view. Maps key → keyword for the given view.
const VIEW_KEYBOARD_SHORTCUTS: Record<string, Record<string, string>> = {
  inbox: { t: 'TODAY', o: 'TODO', n: 'SOON', s: 'SOMEDAY', w: 'WAITING', l: 'LIST', d: 'DONE', x: 'TRASH' },
  today: { o: 'TODO', d: 'DONE' },
  backlog: { t: 'TODAY', d: 'DONE' },
};

// Views that support j/k focus navigation
const FOCUSABLE_VIEWS = new Set<string>(['today', 'inbox', 'backlog']);

function handleKeydown(e: KeyboardEvent): void {
  const state = store.getState();

  // Escape: collapse expanded tasks, close detail card, exit search, blur input, or clear focus
  if (e.key === "Escape") {
    if (isInputFocused()) {
      (document.activeElement as HTMLElement).blur();
      if (state.commandBarMode === 'search') {
        dispatch({ type: 'SearchCleared' });
        dispatch({ type: 'CommandBarModeChanged', mode: 'capture' });
      }
      return;
    }

    if (state.detail.selectedTask) {
      dispatch({ type: 'DetailCardClosed' });
      return;
    }

    // Collapse all expanded tasks
    if (state.expandedTasks.length > 0) {
      dispatch({ type: 'AllTasksCollapsed' });
      return;
    }

    // Clear keyboard focus
    if (state.focusedTaskIndex !== null) {
      dispatch({ type: 'TaskFocusChanged', index: null });
      return;
    }

    return;
  }

  // All shortcuts below only fire when no input is focused
  if (isInputFocused()) return;

  // (Detail panel j/k navigation removed — inline expansion replaces it)

  // --- View-level j/k navigation ---
  if (FOCUSABLE_VIEWS.has(state.activeView)) {
    const visibleTasks = getVisibleTasks(state);
    const taskCount = visibleTasks.length;

    if (e.key === "j" || e.key === "ArrowDown") {
      e.preventDefault();
      if (taskCount === 0) return;
      const nextIdx = state.focusedTaskIndex === null
        ? 0
        : Math.min(state.focusedTaskIndex + 1, taskCount - 1);
      dispatch({ type: 'TaskFocusChanged', index: nextIdx });
      scrollFocusedRowIntoView();
      return;
    }
    if (e.key === "k" || e.key === "ArrowUp") {
      e.preventDefault();
      if (taskCount === 0) return;
      const prevIdx = state.focusedTaskIndex === null
        ? 0
        : Math.max(state.focusedTaskIndex - 1, 0);
      dispatch({ type: 'TaskFocusChanged', index: prevIdx });
      scrollFocusedRowIntoView();
      return;
    }

    // Enter: toggle inline expansion for focused task
    if (e.key === "Enter" && state.focusedTaskIndex !== null) {
      e.preventDefault();
      const task = visibleTasks[state.focusedTaskIndex];
      if (task) {
        const taskKey = `${task.pointer.file}-${task.pointer.taskIndex}`;
        if (state.expandedTasks.includes(taskKey)) {
          dispatch({ type: 'TaskCollapsed', taskKey });
        } else {
          dispatch({ type: 'TaskExpanded', taskKey });
        }
      }
      return;
    }

    // Task-level shortcuts (only when a task is focused)
    if (state.focusedTaskIndex !== null) {
      const focusedTask = visibleTasks[state.focusedTaskIndex];
      if (!focusedTask) return;

      const shortcuts = VIEW_KEYBOARD_SHORTCUTS[state.activeView];
      if (shortcuts) {
        const keyword = shortcuts[e.key];
        if (keyword) {
          e.preventDefault();
          const taskKey = `${focusedTask.pointer.file}-${focusedTask.pointer.taskIndex}`;
          // Move focus to next task (or previous if at end)
          const newFocus = state.focusedTaskIndex >= taskCount - 1
            ? Math.max(0, state.focusedTaskIndex - 1)
            : state.focusedTaskIndex;
          dispatch({ type: 'TaskFocusChanged', index: taskCount <= 1 ? null : newFocus });
          dispatch({ type: 'TaskAnimatingOut', taskKey });
          dispatch({ type: 'ChangeKeywordRequested', file: focusedTask.pointer.file, taskIndex: focusedTask.pointer.taskIndex, keyword });
          showKeyboardActionToast(`Moved to ${keyword}`);
          return;
        }
      }

      // `p` → cycle priority (available in any view)
      if (e.key === "p") {
        e.preventDefault();
        const currentP = focusedTask.task.priority;
        const nextP = currentP === null ? 0 : currentP === 0 ? 1 : currentP === 1 ? 2 : null;
        const prioLabel = nextP === null ? 'none' : `P${nextP}`;
        dispatch({ type: 'ChangePriorityRequested', file: focusedTask.pointer.file, taskIndex: focusedTask.pointer.taskIndex, priority: nextP });
        showKeyboardActionToast(`Priority: ${prioLabel}`);
        return;
      }
    }
  }

  // Number keys: switch views
  const activeView = ACTIVE_VIEW_KEYS[e.key];
  if (activeView) {
    e.preventDefault();
    callbacks.onActiveViewChange(activeView);
    return;
  }

  // `/` → focus command bar in search mode
  if (e.key === "/") {
    e.preventDefault();
    focusCommandBarInput('search');
    return;
  }

  // `c` → focus command bar in capture mode
  if (e.key === "c") {
    e.preventDefault();
    focusCommandBarInput('capture');
    return;
  }

  // Legacy arrow key navigation (garage carousel)
  if (state.activeView === 'garage') {
    if (e.key === "ArrowLeft") {
      e.preventDefault();
      dispatch({ type: 'ViewNavigateLeft' });
    } else if (e.key === "ArrowRight") {
      e.preventDefault();
      dispatch({ type: 'ViewNavigateRight' });
    }
  }
}

function showKeyboardActionToast(message: string): void {
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
      if (toast.parentNode) toast.parentNode.removeChild(toast);
    }, 300);
  }, 2000);
}

function scrollFocusedRowIntoView(): void {
  requestAnimationFrame(() => {
    const focused = document.querySelector('.task-row.task-row-focused') as HTMLElement | null;
    if (focused) {
      focused.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
    }
  });
}

function handleScroll(): void {
  const scrollTop = window.scrollY;
  const scrollHeight = document.documentElement.scrollHeight;
  const clientHeight = window.innerHeight;

  if (scrollHeight - scrollTop - clientHeight < 300) {
    dispatch({ type: 'LoadMoreStarted' });
  }
}

let ws: WebSocket | null = null;

function connectWebSocket(): void {
  const wsUrl = "ws://localhost:8080/ws";

  try {
    ws = new WebSocket(wsUrl);

    ws.onopen = () => console.log("WebSocket connected");
    ws.onmessage = (event) => {
      if (event.data === "reload") {
        console.log("Files changed, reloading view...");
        dispatch({ type: 'WebSocketReloadReceived' });
      }
    };
    ws.onerror = (error) => console.error("WebSocket error:", error);
    ws.onclose = () => {
      console.log("WebSocket disconnected, reconnecting in 5s...");
      setTimeout(connectWebSocket, 5000);
    };
  } catch (error) {
    console.error("Failed to create WebSocket:", error);
    setTimeout(connectWebSocket, 5000);
  }
}

// Initial render + load
render(initialState);
// Load today view (the default starting view)
callbacks.onActiveViewChange('today');

// Fetch inbox count on startup (for status bar and rail badge)
setTimeout(() => {
  fetch('http://localhost:8080/api/views/inbox?offset=0&limit=1')
    .then(r => r.json())
    .then((data: { metadata?: { total?: number } }) => {
      if (data?.metadata?.total !== undefined) {
        dispatch({ type: 'InboxCountLoaded', count: data.metadata.total });
      }
    })
    .catch(() => { /* silently fail */ });
}, 500);

window.addEventListener("scroll", handleScroll);
window.addEventListener("keydown", handleKeydown);
connectWebSocket();
window.addEventListener("beforeunload", () => ws?.close());
