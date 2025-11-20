
/**
 * Sidebar Project Components
 *
 * Components for rendering parent project and subtasks.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer, TaskNode, TaskPointer } from "../../../types/domain.js";
import { renderSidebarSection, renderSidebarSectionHeader } from "./SidebarSection.js";
import { renderTaskNodeCard } from "../TaskCard.js";
import type { TaskCardCallbacks } from "../TaskCard.js";
import { todoKeywordColors, renderTextNodes } from "../../helpers.js";
import { fontSize, fontWeight, spacing, cssClasses } from "../../designSystem.js";

/**
 * Renders the parent project section.
 * Pure function: (parentProject, loading, onClick) => VNode | null
 */
export function renderParentProjectSection(
  parentProject: TaskWithPointer | null,
  loading: boolean,
  onClick: (parent: TaskWithPointer) => void
): VNode | null {
  let content: VNode;

  if (loading) {
    content = h('div', {
      style: {
        padding: spacing.md,
        color: 'var(--text-secondary)',
        fontSize: fontSize.md,
      },
    }, 'Loading...');
  } else if (parentProject) {
    content = h(`div.${cssClasses.hoverableProject}`, {
      style: {
        backgroundColor: 'var(--card-bg)',
        border: '1px solid var(--card-border)',
        borderRadius: '6px',
        padding: spacing.md,
        cursor: 'pointer',
      },
      on: {
        click: () => onClick(parentProject),
      },
    }, [
      h('div', {
        style: {
          display: 'flex',
          alignItems: 'center',
          gap: spacing.sm,
          marginBottom: spacing.xs,
        },
      }, [
        h('span', {
          style: {
            backgroundColor: todoKeywordColors['PROJECT'],
            color: '#fff',
            padding: '3px 6px',
            borderRadius: '3px',
            fontSize: '0.65rem',
            fontWeight: fontWeight.semibold,
            textTransform: 'uppercase',
          },
        }, 'PROJECT'),
        parentProject.task.priority !== null
          ? h('span', {
              style: {
                color: parentProject.task.priority === 1 ? '#ef4444' : '#f59e0b',
                fontSize: fontSize.sm,
                fontWeight: fontWeight.semibold,
              },
            }, `P${parentProject.task.priority}`)
          : null,
      ]),
      h('div', {
        style: {
          fontSize: fontSize.md,
          fontWeight: fontWeight.medium,
          color: 'var(--text-primary)',
        },
      }, renderTextNodes(parentProject.task.title)),
    ]);
  } else {
    content = h('div', {
      style: {
        padding: spacing.md,
        color: 'var(--text-tertiary)',
        fontSize: fontSize.md,
      },
    }, 'Not part of any project');
  }

  return renderSidebarSection('Project', content);
}

/**
 * Renders the subtasks section.
 * Pure function: (projectTree, loading, pointer, callbacks) => VNode | null
 */
export function renderSubtasksSection(
  projectTree: TaskNode | null,
  loading: boolean,
  pointer: TaskPointer,
  callbacks: TaskCardCallbacks & { readonly onViewAllSubtasks: (pointer: TaskPointer) => void }
): VNode | null {
    const hasSubtasks = projectTree !== null && projectTree.children.length > 0;

  let content: VNode;

  if (loading) {
    content = h('div', {
      style: {
        textAlign: 'center',
        padding: spacing.xl,
        color: 'var(--text-secondary)',
      },
    }, 'Loading subtasks...');
  } else if (hasSubtasks) {
    content = h('div', {
      style: {
        display: 'flex',
        flexDirection: 'column',
        gap: '0',
      },
    }, projectTree!.children.map((child) => renderTaskNodeCard(child, 0, callbacks)));
  } else {
    content = h('div', {
      style: {
        textAlign: 'center',
        padding: spacing.xl,
        color: 'var(--text-tertiary)',
        fontSize: fontSize.md,
      },
    }, 'No subtasks');
  }

  return h('div', [
    renderSidebarSectionHeader('Subtasks', 'View All', () => callbacks.onViewAllSubtasks(pointer)),
    content,
  ]);
}
