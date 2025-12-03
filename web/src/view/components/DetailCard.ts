import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer, TaskPointer } from "../../types/domain.js";
import type { AppState } from "../../types/state.js";
import { getTodoKeywordColor, renderTextNodes } from "../helpers.js";
import { renderKeywordBadge } from "./common/Badge.js";
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
import { priorityColors, fontSize, fontWeight, lineHeight, spacing, colors, fonts, cssClasses } from "../designSystem.js";

export interface DetailCardCallbacks {
  readonly onTaskClick: (task: TaskWithPointer) => void;
  readonly onViewAllSubtasks: (pointer: TaskPointer) => void;
  readonly onClickParentProject: (parent: TaskWithPointer) => void;
}

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
    
    h('p', {
      style: {
        margin: '0',
        fontSize: fontSize.xl,
        fontWeight: fontWeight.semibold,
        color: colors.white,
        lineHeight: lineHeight.normal,
        textDecoration: task.todoKeyword === 'DONE' ? 'line-through' : 'none',
        opacity: task.todoKeyword === 'DONE' ? '0.7' : '1',
      },
    }, titleNodes),
  ]);
}

function renderDescriptionSection(taskWithPointer: TaskWithPointer): VNode | null {
  const { task } = taskWithPointer;
  if (task.description.length === 0) return null;
  
  const descriptionNodes = renderTextNodes(task.description);
  const content = h('div', {
    style: {
      margin: '0',
      fontSize: fontSize.base,
      color: colors.greyLight,
      lineHeight: lineHeight.relaxed,
    },
  }, descriptionNodes);
  
  return renderSidebarSection('Description', content);
}

function renderEmptyState(): VNode {
  return h('div', {
    style: {
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      height: '100%',
      padding: spacing.xxl,
      textAlign: 'center',
    },
  }, [
    h('div', {
      style: {
        display: 'flex',
        flexDirection: 'column',
        gap: spacing.lg,
        opacity: '0.5',
      },
    }, [
      h('div', {
        style: {
          fontSize: '4rem',
          fontFamily: fonts.display,
          color: colors.greyDark,
        },
      }, '⚡'),
      h('p', {
        style: {
          margin: '0',
          fontFamily: fonts.display,
          fontSize: fontSize.xl,
          fontWeight: fontWeight.bold,
          color: colors.greyLight,
          letterSpacing: '0.05em',
          textTransform: 'uppercase',
        },
      }, 'Select a task'),
      h('p', {
        style: {
          margin: '0',
          fontSize: fontSize.sm,
          color: colors.greyDark,
        },
      }, 'Click on a card to view details'),
    ]),
  ]);
}

export function renderDetailCard(
  taskWithPointer: TaskWithPointer | null,
  state: AppState,
  callbacks: DetailCardCallbacks
): VNode {
  if (!taskWithPointer) {
    return h('div.detail-card', {
      style: {
        width: '400px',
        height: '100%',
        backgroundColor: colors.asphalt,
        borderLeft: `2px solid ${colors.greyDark}`,
        overflowY: 'auto',
        flexShrink: '0',
      },
    }, [renderEmptyState()]);
  }

  const { task, pointer } = taskWithPointer;
  const isProject = task.todoKeyword === "PROJECT";
  const keywordColor = getTodoKeywordColor(task.todoKeyword);
  const priorityColor = task.priority !== null ? priorityColors[task.priority as keyof typeof priorityColors] : null;
  const titleNodes = renderTextNodes(task.title);

  return h('div.detail-card', {
    style: {
      width: '400px',
      height: '100%',
      backgroundColor: colors.asphalt,
      borderLeft: `2px solid ${colors.greyDark}`,
      overflowY: 'auto',
      overflowX: 'hidden',
      flexShrink: '0',
    },
  }, [
    h('div', {
      style: {
        padding: spacing.lg,
        display: 'flex',
        flexDirection: 'column',
        gap: spacing.sm,
      },
    }, [
      h('div', {
        style: {
          fontFamily: fonts.display,
          fontSize: fontSize.xs,
          fontWeight: fontWeight.semibold,
          letterSpacing: '0.1em',
          textTransform: 'uppercase',
          color: colors.greyDark,
          marginBottom: spacing.xs,
        },
      }, 'Selected Task'),
      
      h('div', {
        style: {
          display: 'flex',
          alignItems: 'center',
          gap: spacing.xs,
        },
      }, [
        renderKeywordBadge(task.todoKeyword, keywordColor, 'medium'),
        task.priority !== null && priorityColor
          ? h('span', {
              style: {
                color: priorityColor,
                fontSize: fontSize.xs,
                fontWeight: fontWeight.semibold,
              },
            }, `P${task.priority}`)
          : null,
      ].filter(Boolean)),
      
      h('p', {
        style: {
          margin: '0',
          fontSize: fontSize.sm,
          fontWeight: fontWeight.semibold,
          color: colors.white,
          lineHeight: lineHeight.tight,
          textDecoration: task.todoKeyword === 'DONE' ? 'line-through' : 'none',
          opacity: task.todoKeyword === 'DONE' ? '0.7' : '1',
          display: '-webkit-box',
          WebkitLineClamp: '3',
          WebkitBoxOrient: 'vertical',
          overflow: 'hidden',
        },
      }, titleNodes),
      
      task.tags.length > 0 ? h('div', {
        style: {
          display: 'flex',
          gap: spacing.xs,
          flexWrap: 'wrap',
        },
      }, task.tags.slice(0, 3).map(tag => 
        h('span', {
          style: {
            fontSize: fontSize.xs,
            padding: '2px 6px',
            borderRadius: '3px',
            backgroundColor: 'var(--tag-bg)',
            color: 'var(--tag-text)',
            fontFamily: fonts.body,
            fontWeight: fontWeight.medium,
          },
        }, tag)
      )) : null,
      
      h('div', {
        style: {
          fontSize: fontSize.xs,
          color: colors.greyDark,
          fontFamily: fonts.display,
          marginTop: spacing.xs,
        },
      }, `${pointer.file.split('/').pop()} #${pointer.taskIndex + 1}`),
    ]),
  ]);
}
