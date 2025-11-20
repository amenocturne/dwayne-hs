/**
 * Sidebar Component
 * 
 * Pure function rendering the task detail sidebar.
 * Refactored to use composable subcomponents.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer, TaskPointer } from "../../types/domain.js";
import type { AppState } from "../../types/state.js";
import { getTodoKeywordColor, renderTextNodes } from "../helpers.js";
import type { TaskCardCallbacks } from "./TaskCard.js";
import { renderSidebarSection } from "./sidebar/SidebarSection.js";
import {
  renderTagsSection,
  renderDatesSection,
  renderPropertiesSection,
  renderLocationSection,
} from "./sidebar/SidebarMetadata.js";
import {
  renderParentProjectSection,
  renderSubtasksSection,
} from "./sidebar/SidebarProject.js";
import { renderKeywordBadge } from "./common/Badge.js";
import { priorityColors, fontSize, fontWeight, lineHeight, spacing, cssClasses } from "../designSystem.js";

export interface SidebarCallbacks extends TaskCardCallbacks {
  readonly onClose: () => void;
  readonly onViewAllSubtasks: (pointer: TaskPointer) => void;
  readonly onClickParentProject: (parent: TaskWithPointer) => void;
}

/**
 * Renders the sidebar header.
 * Pure function: (onClose) => VNode
 */
function renderSidebarHeader(onClose: () => void): VNode {
  return h('div.sidebar-header', {
    style: {
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center',
      padding: `${spacing.xl} ${spacing.xxl}`,
      borderBottom: '1px solid var(--card-border)',
      position: 'sticky',
      top: '0',
      backgroundColor: 'var(--card-bg)',
      zIndex: '10',
    },
  }, [
    h('h2', {
      style: {
        margin: '0',
        fontSize: fontSize.xl,
        fontWeight: fontWeight.bold,
        color: 'var(--text-primary)',
      },
    }, 'Task Details'),
    h(`button.${cssClasses.hoverableButton}`, {
      props: {
        type: 'button',
        'aria-label': 'Close sidebar',
      },
      style: {
        background: 'none',
        border: 'none',
        fontSize: '1.5rem',
        cursor: 'pointer',
        color: 'var(--text-secondary)',
        padding: `${spacing.xs} ${spacing.sm}`,
        lineHeight: lineHeight.tight,
      },
      on: {
        click: onClose,
      },
    }, '×'),
  ]);
}

/**
 * Renders the task title section with keyword badge.
 * Pure function: (task) => VNode
 */
function renderTitleSection(taskWithPointer: TaskWithPointer): VNode {
  const { task } = taskWithPointer;
  const keywordColor = getTodoKeywordColor(task.todoKeyword);
  const priorityColor = task.priority !== null ? priorityColors[task.priority as keyof typeof priorityColors] : null;
  const titleNodes = renderTextNodes(task.title);
  
  return h('div', {
    style: {
      display: 'flex',
      flexDirection: 'column',
      gap: spacing.md,
    },
  }, [
    // Badges
    h('div', {
      style: {
        display: 'flex',
        alignItems: 'center',
        gap: spacing.sm,
      },
    }, [
      renderKeywordBadge(task.todoKeyword, keywordColor, 'large'),
      task.priority !== null && priorityColor
        ? h('span.priority', {
            style: {
              color: priorityColor,
              fontSize: fontSize.base,
              fontWeight: fontWeight.semibold,
            },
          }, `Priority ${task.priority}`)
        : null,
    ].filter(Boolean)),
    
    // Title
    renderSidebarSection('Title', h('p', {
      style: {
        margin: '0',
        fontSize: fontSize.xl,
        fontWeight: fontWeight.semibold,
        color: 'var(--text-primary)',
        lineHeight: lineHeight.normal,
        textDecoration: task.todoKeyword === 'DONE' ? 'line-through' : 'none',
        opacity: task.todoKeyword === 'DONE' ? '0.7' : '1',
      },
    }, titleNodes)),
  ]);
}

/**
 * Renders the description section.
 * Pure function: (task) => VNode | null
 */
function renderDescriptionSection(taskWithPointer: TaskWithPointer): VNode | null {
  const { task } = taskWithPointer;
  if (task.description.length === 0) return null;
  
  const descriptionNodes = renderTextNodes(task.description);
  const content = h('div', {
    style: {
      margin: '0',
      fontSize: fontSize.base,
      color: 'var(--text-secondary)',
      lineHeight: lineHeight.relaxed,
    },
  }, descriptionNodes);
  
  return renderSidebarSection('Description', content);
}

/**
 * Renders the task detail sidebar.
 * Pure function: (TaskWithPointer | null, AppState, SidebarCallbacks) => VNode | null
 */
export function renderSidebar(
  taskWithPointer: TaskWithPointer | null,
  state: AppState,
  callbacks: SidebarCallbacks
): VNode | null {
  if (!taskWithPointer) return null;

  const { task, pointer } = taskWithPointer;
  const isProject = task.todoKeyword === "PROJECT";

  return h('div.sidebar-overlay', {
    style: {
      position: 'fixed',
      top: '0',
      left: '0',
      right: '0',
      bottom: '0',
      backgroundColor: 'rgba(0, 0, 0, 0.5)',
      zIndex: '1000',
      animation: 'fadeIn 0.2s ease-out',
    },
    on: {
      click: (e: Event) => {
        if (e.target === e.currentTarget) {
          callbacks.onClose();
        }
      },
    },
  }, [
    h('div.sidebar', {
      style: {
        position: 'fixed',
        top: '0',
        right: '0',
        bottom: '0',
        width: '600px',
        maxWidth: '90vw',
        backgroundColor: 'var(--card-bg)',
        boxShadow: '-4px 0 24px rgba(0, 0, 0, 0.2)',
        overflowY: 'auto',
        animation: 'slideInRight 0.3s ease-out',
        display: 'flex',
        flexDirection: 'column',
      },
    }, [
      renderSidebarHeader(callbacks.onClose),
      
      // Content
      h('div.sidebar-content', {
        style: {
          padding: spacing.xxl,
          flex: '1',
          display: 'flex',
          flexDirection: 'column',
          gap: spacing.xl,
        },
      }, [
        renderTitleSection(taskWithPointer),
        renderDescriptionSection(taskWithPointer),
        renderTagsSection(task.tags),
        
        // Parent project (only for non-PROJECT tasks)
        !isProject
          ? renderParentProjectSection(
              state.parentProject,
              state.loadingParentProject,
              callbacks.onClickParentProject
            )
          : null,
        
        renderDatesSection(task),
        renderPropertiesSection(task.properties),
        
        // Subtasks (only for PROJECT tasks)
        isProject
          ? renderSubtasksSection(
              state.projectTree,
              state.loadingProjectTree,
              pointer,
              callbacks
            )
          : null,
        
        renderLocationSection(pointer.file, pointer.taskIndex),
      ].filter(Boolean)),
    ]),
  ]);
}
