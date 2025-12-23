import { init } from "snabbdom/build/init.js";
import { classModule } from "snabbdom/build/modules/class.js";
import { propsModule } from "snabbdom/build/modules/props.js";
import { styleModule } from "snabbdom/build/modules/style.js";
import { eventListenersModule } from "snabbdom/build/modules/eventlisteners.js";
import { attributesModule } from "snabbdom/build/modules/attributes.js";
import type { VNode } from "snabbdom/build/vnode.js";

import type { AppState } from "./types/state.js";
import type { TaskPointer, TaskWithPointer } from "./types/domain.js";
import { view } from "./view/app.js";
import type { AppCallbacks } from "./view/app.js";
import { createStore } from "./state/store.js";
import type { Dispatch } from "./state/effects.js";
import { carousel3DConfig } from "./view/constants.js";

const initialState: AppState = {
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
  onSearchChange: (query: string) => {
    dispatch({ type: 'SearchQueryChanged', query });
    debouncedSearch(query);
  },
  onClearSearch: () => dispatch({ type: 'SearchCleared' }),
  onViewChange: (newView) => dispatch({ type: 'ViewChanged', view: newView }),
  onTaskClick: (taskWithPointer) => dispatch({ type: 'TaskClicked', task: taskWithPointer }),
  onCloseDetailCard: () => dispatch({ type: 'DetailCardClosed' }),
  onViewAllSubtasks: (pointer: TaskPointer) => dispatch({ type: 'ProjectViewRequested', pointer }),
  onClickParentProject: (parent: TaskWithPointer) => dispatch({ type: 'TaskClicked', task: parent }),
  onBackToView: () => dispatch({ type: 'BackToViewRequested' }),
  onCarouselRotate: (delta: number) => {
    dispatch({ type: 'CarouselRotate', delta });
  },
  onLoadMore: () => {
    dispatch({ type: 'LoadMoreStarted' });
  },
  onDebugToggle: () => {
    dispatch({ type: 'DebugToggled' });
  },
  onDebugParamChange: (param, value) => {
    dispatch({ type: 'DebugParamChanged', param, value });
  },
};

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

render(initialState);
dispatch({ type: 'ViewChanged', view: 'all' });

window.addEventListener("scroll", handleScroll);
connectWebSocket();
window.addEventListener("beforeunload", () => ws?.close());
