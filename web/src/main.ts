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

const initialState: AppState = {
  activeView: 'today',
  commandBarMode: 'capture',
  detailPanel: {
    open: false,
    task: null,
  },
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

  // Detail panel
  onDetailPanelOpen: (task) => {
    dispatch({ type: 'DetailPanelOpened', task });
  },
  onDetailPanelClose: () => {
    dispatch({ type: 'DetailPanelClosed' });
  },
  onChangePriority: (file, taskIndex, priority) => {
    dispatch({ type: 'ChangePriorityRequested', file, taskIndex, priority });
  },
  onChangeTitle: (file, taskIndex, title) => {
    dispatch({ type: 'ChangeTitleRequested', file, taskIndex, title });
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

function handleKeydown(e: KeyboardEvent): void {
  // Escape: close detail panel, exit search, or blur input
  if (e.key === "Escape") {
    const state = store.getState();

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

    if (state.detailPanel.open) {
      dispatch({ type: 'DetailPanelClosed' });
      return;
    }

    return;
  }

  // All shortcuts below only fire when no input is focused
  if (isInputFocused()) return;

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
  if (e.key === "ArrowLeft") {
    e.preventDefault();
    dispatch({ type: 'ViewNavigateLeft' });
  } else if (e.key === "ArrowRight") {
    e.preventDefault();
    dispatch({ type: 'ViewNavigateRight' });
  }
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
dispatch({ type: 'ViewChanged', view: 'all' });

window.addEventListener("scroll", handleScroll);
window.addEventListener("keydown", handleKeydown);
connectWebSocket();
window.addEventListener("beforeunload", () => ws?.close());
