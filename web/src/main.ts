import { init } from "snabbdom/build/init.js";
import { classModule } from "snabbdom/build/modules/class.js";
import { propsModule } from "snabbdom/build/modules/props.js";
import { styleModule } from "snabbdom/build/modules/style.js";
import { eventListenersModule } from "snabbdom/build/modules/eventlisteners.js";
import { attributesModule } from "snabbdom/build/modules/attributes.js";
import type { VNode } from "snabbdom/build/vnode.js";

import type { AppState } from "./types/state.js";
import type { TaskWithPointer, TaskPointer } from "./types/domain.js";
import { view } from "./view/app.js";
import type { AppCallbacks } from "./view/app.js";
import { createStore } from "./state/store.js";
import type { Dispatch } from "./state/effects.js";
import { fetchTasks, fetchProjectTree } from "./api/client.js";

const initialState: AppState = {
  tasks: [],
  loading: true,
  error: null,
  currentView: "all",
  searchQuery: "",
  offset: 0,
  hasMore: true,
  loadingMore: false,
  totalCount: 0,
  selectedTask: null,
  projectTree: null,
  loadingProjectTree: false,
  projectPointer: null,
  parentProject: null,
  loadingParentProject: false,
  carouselRotation: 0,
  carouselTargetRotation: 0,
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

async function findParentProject(
  taskWithPointer: TaskWithPointer
): Promise<TaskWithPointer | null> {
  const { task, pointer } = taskWithPointer;
  const projectsResult = await fetchTasks("project", 0, 1000);
  const projectsInSameFile = projectsResult.data.filter(
    (p) => p.pointer.file === pointer.file && p.pointer.taskIndex < pointer.taskIndex
  );

  for (let i = projectsInSameFile.length - 1; i >= 0; i--) {
    const candidate = projectsInSameFile[i];
    if (!candidate || candidate.task.level >= task.level) continue;

    try {
      const tree = await fetchProjectTree(candidate.pointer.file, candidate.pointer.taskIndex);
      
      const containsTask = (node: any): boolean => {
        if (node.pointer.file === pointer.file && node.pointer.taskIndex === pointer.taskIndex) {
          return true;
        }
        return node.children.some(containsTask);
      };

      if (containsTask(tree.root)) {
        return { task: candidate.task, pointer: candidate.pointer };
      }
    } catch {
      continue;
    }
  }

  return null;
}

async function handleTaskSelection(taskWithPointer: TaskWithPointer): Promise<void> {
  dispatch({ type: 'TaskSelected', task: taskWithPointer });

  const { task, pointer } = taskWithPointer;

  if (task.todoKeyword !== "PROJECT") {
    dispatch({ type: 'ParentProjectLoadStarted' });
    try {
      const parentProject = await findParentProject(taskWithPointer);
      dispatch({ type: 'ParentProjectLoaded', project: parentProject });
    } catch (err) {
      console.error("Failed to find parent project:", err);
      dispatch({ type: 'ParentProjectLoadFailed' });
    }
  }

  if (task.todoKeyword === "PROJECT") {
    dispatch({ type: 'ProjectTreeLoadStarted' });
    try {
      const result = await fetchProjectTree(pointer.file, pointer.taskIndex);
      dispatch({ type: 'ProjectTreeLoaded', tree: result.root });
    } catch (err) {
      console.error("Failed to load project tree:", err);
      dispatch({ type: 'ProjectTreeLoadFailed' });
    }
  }
}

const callbacks: AppCallbacks = {
  onSearchChange: (query: string) => {
    dispatch({ type: 'SearchQueryChanged', query });
    debouncedSearch(query);
  },
  onClearSearch: () => dispatch({ type: 'SearchCleared' }),
  onViewChange: (newView) => dispatch({ type: 'ViewChanged', view: newView }),
  onTaskClick: (taskWithPointer) => handleTaskSelection(taskWithPointer),
  onCloseSidebar: () => dispatch({ type: 'SidebarClosed' }),
  onViewAllSubtasks: (pointer: TaskPointer) => dispatch({ type: 'ProjectViewRequested', pointer }),
  onClickParentProject: (parent: TaskWithPointer) => handleTaskSelection(parent),
  onBackToView: () => dispatch({ type: 'BackToViewRequested' }),
  onCarouselRotate: (delta: number) => {
    console.log('onCarouselRotate called with delta:', delta);
    dispatch({ type: 'CarouselRotate', delta });
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
