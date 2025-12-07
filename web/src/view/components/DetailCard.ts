import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer, TaskPointer } from "../../types/domain.js";
import type { AppState } from "../../types/state.js";
import { getTodoKeywordColor } from "../designSystem.js";
import { renderTextNodes } from "./common/TextNodes.js";
import { renderKeywordBadge } from "./common/Badge.js";
import { renderSidebarSection } from "./detail/SidebarSection.js";
import {
  renderTagsSection,
  renderDatesSection,
  renderPropertiesSection,
  renderLocationSection,
} from "./detail/SidebarMetadata.js";
import {
  renderParentProjectSection,
  renderSubtasksSection,
} from "./detail/SidebarProject.js";
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
        position: 'fixed',
        top: '10%',
        right: '5%',
        width: '380px',
        maxHeight: '85vh',
        backgroundColor: 'transparent',
        zIndex: '200',
        padding: spacing.lg,
        display: 'flex',
        alignItems: 'flex-start',
        justifyContent: 'flex-start',
        perspective: '1200px',
        pointerEvents: 'none',
      },
    }, [
      h('div', {
        style: {
          backgroundColor: colors.asphalt,
          border: `2px solid ${colors.greyDark}`,
          outline: `1px solid ${colors.void}`,
          outlineOffset: '4px',
          clipPath: 'polygon(0 0, calc(100% - 24px) 0, 100% 24px, 100% 100%, 24px 100%, 0 calc(100% - 24px))',
          padding: spacing.lg,
          display: 'flex',
          flexDirection: 'column',
          gap: spacing.sm,
          width: '100%',
          opacity: '0.3',
          position: 'relative',
          transformStyle: 'preserve-3d',
          transform: 'rotateY(-12deg) rotateX(3deg)',
          fontSize: fontSize.sm,
          minHeight: '400px',
          alignItems: 'center',
          justifyContent: 'center',
        },
      }, [
        h('div', {
          style: {
            textAlign: 'center',
            color: colors.greyLight,
          },
        }, [
          h('div', {
            style: {
              fontSize: '3rem',
              marginBottom: spacing.md,
            },
          }, '→'),
          h('p', {
            style: {
              margin: '0',
              fontFamily: fonts.display,
              fontSize: fontSize.lg,
              fontWeight: fontWeight.bold,
              letterSpacing: '0.05em',
              textTransform: 'uppercase',
            },
          }, 'Select a card'),
        ]),
      ]),
    ]);
  }

  const { task, pointer } = taskWithPointer;
  const isProject = task.todoKeyword === "PROJECT";
  const keywordColor = getTodoKeywordColor(task.todoKeyword);
  const priorityColor = task.priority !== null ? priorityColors[task.priority as keyof typeof priorityColors] : null;
  const titleNodes = renderTextNodes(task.title);
  const isPriority = task.priority === 0;
  const isRunning = task.todoKeyword === 'DOING' || task.todoKeyword === 'NEXT';
  const isDone = task.todoKeyword === 'DONE';
  const cardNumber = `${pointer.taskIndex + 1}`.padStart(3, '0');

  const borderColor = isPriority ? colors.redBright : isRunning ? colors.pinkBright : colors.greyDark;
  const outlineColor = isPriority ? 'rgba(255, 51, 51, 0.3)' : isRunning ? 'rgba(255, 0, 153, 0.3)' : colors.void;

  const descriptionNodes = task.description.length > 0
    ? h('div', {
        style: {
          margin: '0',
          fontFamily: fonts.body,
          fontSize: fontSize.sm,
          fontWeight: '400',
          color: colors.greyLight,
          lineHeight: '1.5',
        },
      }, renderTextNodes(task.description))
    : null;

  return h('div.detail-card', {
    style: {
      position: 'fixed',
      top: '10%',
      right: '5%',
      width: '380px',
      maxHeight: '85vh',
      backgroundColor: 'transparent',
      zIndex: '200',
      padding: spacing.lg,
      display: 'flex',
      alignItems: 'flex-start',
      justifyContent: 'flex-start',
      perspective: '1200px',
      pointerEvents: 'auto',
    },
  }, [
    h('div', {
      style: {
        backgroundColor: colors.asphalt,
        border: `2px solid ${borderColor}`,
        outline: `1px solid ${outlineColor}`,
        outlineOffset: '4px',
        clipPath: 'polygon(0 0, calc(100% - 24px) 0, 100% 24px, 100% 100%, 24px 100%, 0 calc(100% - 24px))',
        padding: spacing.lg,
        display: 'flex',
        flexDirection: 'column',
        gap: spacing.sm,
        width: '100%',
        opacity: isDone ? '0.5' : '1',
        position: 'relative',
        transformStyle: 'preserve-3d',
        transform: 'rotateY(-12deg) rotateX(3deg)',
        fontSize: fontSize.sm,
      },
    }, [
      // Corner accent
      h('div', {
        style: {
          position: 'absolute',
          top: '0',
          right: '0',
          width: '24px',
          height: '24px',
          background: `linear-gradient(135deg, ${isPriority ? colors.redBright : isRunning ? colors.pinkBright : colors.cyanDim} 0%, transparent 50%)`,
          clipPath: 'polygon(0 0, 100% 0, 100% 100%)',
          opacity: isPriority || isRunning ? '0.5' : '0.3',
          pointerEvents: 'none',
        },
      }),

      // Header with card number and badge
      h('div', {
        style: {
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'flex-start',
          gap: spacing.md,
          flexShrink: '0',
          marginBottom: spacing.sm,
        },
      }, [
        h('span', {
          style: {
            fontFamily: fonts.display,
            fontSize: fontSize.xs,
            fontWeight: fontWeight.bold,
            color: colors.greyDark,
            letterSpacing: '0.1em',
            textTransform: 'uppercase',
          },
        }, cardNumber),
        renderKeywordBadge(task.todoKeyword, keywordColor, 'medium'),
      ]),

      // Title
      h('h3', {
        style: {
          margin: '0',
          fontFamily: fonts.body,
          fontSize: fontSize.base,
          fontWeight: fontWeight.bold,
          letterSpacing: '-0.02em',
          color: colors.white,
          textDecoration: isDone ? 'line-through' : 'none',
          opacity: isDone ? '0.7' : '1',
          flexShrink: '0',
          lineHeight: '1.3',
        },
      }, titleNodes),

      // Description
      descriptionNodes ? renderSidebarSection('Description', descriptionNodes) : null,

      // Tags
      renderTagsSection(task.tags),

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
    ].filter(Boolean)),
  ]);
}
