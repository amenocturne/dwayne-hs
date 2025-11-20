/**
 * Task Card Components
 * 
 * Pure functions for rendering task cards in different formats.
 * Refactored to use a unified parametric renderer.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer, TaskNode } from "../../types/domain.js";
import { getTodoKeywordColor, renderTextNodes } from "../helpers.js";
import { renderKeywordBadge, renderPriorityBadge } from "./common/Badge.js";
import { renderTags } from "./common/Tag.js";
import { renderCompactDates } from "./common/DateDisplay.js";
import { cardSizes, priorityColors, spacing, cssClasses, fontWeight, lineHeight } from "../designSystem.js";

export interface TaskCardCallbacks {
  readonly onTaskClick: (task: TaskWithPointer) => void;
}

/**
 * Card size variants
 */
type CardSize = keyof typeof cardSizes;

/**
 * Configuration for card rendering behavior
 */
interface CardConfig {
  readonly size: CardSize;
  readonly showDescription: boolean;
  readonly showDates: boolean;
  readonly depth?: number; // For tree rendering with indentation
}

/**
 * Unified task card renderer.
 * Pure parametric function that handles all card variants through configuration.
 * 
 * Pure function: (TaskWithPointer, CardConfig, TaskCardCallbacks) => VNode
 */
function renderUnifiedCard(
  taskWithPointer: TaskWithPointer,
  config: CardConfig,
  callbacks: TaskCardCallbacks
): VNode {
  const { task, pointer } = taskWithPointer;
  const sizeConfig = cardSizes[config.size];
  const keywordColor = getTodoKeywordColor(task.todoKeyword);
  const titleNodes = renderTextNodes(task.title);
  const priorityColor = task.priority !== null ? priorityColors[task.priority as keyof typeof priorityColors] : null;
  
  const cardClass = config.size === 'large' ? 'task-card' : 'subtask-card';
  const hoverClass = config.size === 'large' ? cssClasses.hoverable : cssClasses.hoverableSubtle;

  // Render description if needed
  const descriptionNodes = config.showDescription && task.description.length > 0
    ? h('p', {
        style: {
          margin: '0',
          fontSize: sizeConfig.titleSize,
          color: 'var(--text-secondary)',
          lineHeight: lineHeight.normal,
          overflow: 'hidden',
          display: '-webkit-box',
          WebkitLineClamp: sizeConfig.descriptionLineClamp,
          WebkitBoxOrient: 'vertical',
          flexShrink: '1',
        },
      }, renderTextNodes(task.description))
    : null;

  // Content wrapper for large cards (push footer down)
  const contentWrapper = config.size === 'large'
    ? (children: ReadonlyArray<VNode | null>) => h('div', {
        style: {
          display: 'flex',
          flexDirection: 'column',
          gap: sizeConfig.gap,
          flex: '1',
          minHeight: '0',
          overflow: 'hidden',
          position: 'relative',
        },
      }, [
        ...children,
        // Fade overlay for large cards
        h('div', {
          style: {
            position: 'absolute',
            bottom: '0',
            left: '0',
            right: '0',
            height: '40px',
            background: 'linear-gradient(to bottom, transparent, var(--card-bg))',
            pointerEvents: 'none',
          },
        }),
      ].filter(Boolean))
    : (children: ReadonlyArray<VNode | null>) => h('div', {
        style: {
          display: 'flex',
          flexDirection: 'column',
          gap: sizeConfig.gap,
        },
      }, children.filter(Boolean));

  const cardContent = [
    // Header: badges
    h('div', {
      style: {
        display: 'flex',
        alignItems: 'center',
        gap: spacing.sm,
        flexShrink: '0',
      },
    }, [
      renderKeywordBadge(task.todoKeyword, keywordColor, config.size),
      task.priority !== null && priorityColor
        ? renderPriorityBadge(task.priority, priorityColor, config.size)
        : null,
    ].filter(Boolean)),

    // Title
    h(config.size === 'large' ? 'h3' : 'div', {
      style: config.size === 'large' ? {
        margin: '0',
        fontSize: sizeConfig.titleSize,
        fontWeight: fontWeight.semibold,
        color: 'var(--text-primary)',
        textDecoration: task.todoKeyword === 'DONE' ? 'line-through' : 'none',
        opacity: task.todoKeyword === 'DONE' ? '0.7' : '1',
        flexShrink: '0',
      } : {
        margin: '0',
        fontSize: sizeConfig.titleSize,
        fontWeight: fontWeight.medium,
        color: 'var(--text-primary)',
        textDecoration: task.todoKeyword === 'DONE' ? 'line-through' : 'none',
        opacity: task.todoKeyword === 'DONE' ? '0.7' : '1',
      },
    }, titleNodes),

    // Description (if enabled)
    descriptionNodes,

    // Tags
    renderTags(task.tags, config.size, config.size === 'large' ? '6px' : '4px'),
  ];

  const cardBody = config.size === 'large'
    ? [
        contentWrapper(cardContent),
        config.showDates
          ? renderCompactDates(task.scheduled, task.deadline, task.closed, task.createdProp)
          : null,
      ]
    : cardContent;

  // Build style object without undefined values for exactOptionalPropertyTypes
  const cardStyle: Record<string, string> = {
    backgroundColor: 'var(--card-bg)',
    border: '1px solid var(--card-border)',
    borderRadius: config.size === 'large' ? '8px' : '6px',
    padding: sizeConfig.padding,
    display: 'flex',
    flexDirection: 'column',
    gap: sizeConfig.gap,
    cursor: 'pointer',
    height: sizeConfig.height,
  };
  
  if (config.depth !== undefined) {
    cardStyle['marginLeft'] = `${config.depth * 16}px`;
    cardStyle['marginBottom'] = spacing.sm;
  }

  return h(`div.${cardClass}.${hoverClass}`, {
    key: `${pointer.file}-${pointer.taskIndex}`,
    style: cardStyle,
    on: {
      click: () => callbacks.onTaskClick(taskWithPointer),
    },
  }, cardBody.filter(Boolean));
}

/**
 * Renders a full task card with description and dates.
 * Pure function: (TaskWithPointer, TaskCardCallbacks) => VNode
 */
export function renderTaskCard(
  taskWithPointer: TaskWithPointer,
  callbacks: TaskCardCallbacks
): VNode {
  return renderUnifiedCard(
    taskWithPointer,
    { size: 'large', showDescription: true, showDates: true },
    callbacks
  );
}

/**
 * Renders a compact subtask card without description.
 * Pure function: (TaskWithPointer, TaskCardCallbacks) => VNode
 */
export function renderSubtaskCard(
  taskWithPointer: TaskWithPointer,
  callbacks: TaskCardCallbacks
): VNode {
  return renderUnifiedCard(
    taskWithPointer,
    { size: 'medium', showDescription: false, showDates: false },
    callbacks
  );
}

/**
 * Renders a task node card with indented children for tree display.
 * Pure function: (TaskNode, number, TaskCardCallbacks) => VNode
 */
export function renderTaskNodeCard(
  node: TaskNode,
  depth: number,
  callbacks: TaskCardCallbacks
): VNode {
  const { task, pointer, children } = node;

  return h('div', {
    key: `${pointer.file}-${pointer.taskIndex}`,
  }, [
    renderUnifiedCard(
      { task, pointer },
      { size: 'medium', showDescription: false, showDates: false, depth },
      callbacks
    ),
    ...children.map((child) => renderTaskNodeCard(child, depth + 1, callbacks)),
  ]);
}

/**
 * Renders a grid of task cards.
 * Pure function: (ReadonlyArray<TaskWithPointer>, TaskCardCallbacks) => VNode
 */
export function renderTaskGrid(
  tasks: ReadonlyArray<TaskWithPointer>,
  callbacks: TaskCardCallbacks
): VNode {
  return h('div.task-grid', {
    style: {
      display: 'grid',
      gridTemplateColumns: 'repeat(auto-fill, minmax(min(320px, 100%), 1fr))',
      gap: '20px',
      width: '100%',
    },
  }, tasks.map((task) => renderTaskCard(task, callbacks)));
}
