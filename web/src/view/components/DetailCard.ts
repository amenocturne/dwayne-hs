import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer, TaskPointer } from "../../types/domain.js";
import type { AppState } from "../../types/state.js";
import {
  renderDatesSection,
  renderPropertiesSection,
  renderLocationSection,
} from "./detail/SidebarMetadata.js";
import {
  renderParentProjectSection,
  renderSubtasksSection,
} from "./detail/SidebarProject.js";
import { renderModalCard } from "./card/TaskCard.js";

export interface DetailCardCallbacks {
  readonly onTaskClick: (task: TaskWithPointer) => void;
  readonly onViewAllSubtasks: (pointer: TaskPointer) => void;
  readonly onClickParentProject: (parent: TaskWithPointer) => void;
  readonly onClose: () => void;
}

export function renderDetailCard(
  taskWithPointer: TaskWithPointer | null,
  state: AppState,
  callbacks: DetailCardCallbacks
): VNode {
  // No task selected - return hidden element
  if (!taskWithPointer) {
    return h('div.detail-card-modal', {
      style: {
        display: 'none',
      },
    });
  }

  const { task, pointer } = taskWithPointer;
  const isProject = task.todoKeyword === "PROJECT";

  // Build extra content sections for the modal
  const extraContent: Array<VNode | null> = [
    // Parent project (only for non-PROJECT tasks)
    !isProject
      ? renderParentProjectSection(
          state.detail.parentProject,
          state.detail.loadingParentProject,
          callbacks.onClickParentProject
        )
      : null,

    // Dates
    renderDatesSection(task),

    // Properties
    renderPropertiesSection(task.properties),

    // Subtasks (only for PROJECT tasks)
    isProject
      ? renderSubtasksSection(
          state.detail.projectTree,
          state.detail.loadingProjectTree,
          pointer,
          callbacks
        )
      : null,

    // Location
    renderLocationSection(pointer.file, pointer.taskIndex),
  ];

  // Modal overlay with dimmed background
  return h('div.detail-card-modal', {
    style: {
      position: 'fixed',
      top: '0',
      left: '0',
      width: '100vw',
      height: '100vh',
      backgroundColor: 'rgba(0, 0, 0, 0.85)',
      zIndex: '1000',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      pointerEvents: 'auto',
    },
    on: {
      click: (e: MouseEvent) => {
        // Only close if clicking the overlay itself, not the card
        if ((e.target as HTMLElement).classList.contains('detail-card-modal')) {
          callbacks.onClose();
        }
      },
    },
  }, [
    renderModalCard(taskWithPointer, extraContent),
  ]);
}
