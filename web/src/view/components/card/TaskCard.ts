import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer, TaskNode } from "../../../types/domain.js";
import { getTodoKeywordColor } from "../../designSystem.js";
import { renderTextNodes } from "../common/TextNodes.js";
import { renderKeywordBadge, renderPriorityBadge } from "../common/Badge.js";
import { renderTags } from "../common/Tag.js";
import { renderCompactDates } from "../common/DateDisplay.js";
import {
  cardSizes,
  priorityColors,
  spacing,
  cssClasses,
  fontWeight,
  colors,
  fonts,
  clipPaths,
  transitions,
} from "../../designSystem.js";

export interface TaskCardCallbacks {
  readonly onTaskClick: (task: TaskWithPointer) => void;
}

export type CardVariant = 'task' | 'subtask' | 'tree-node';

type CardSize = keyof typeof cardSizes;

interface CardConfig {
  readonly size: CardSize;
  readonly showDescription: boolean;
  readonly showDates: boolean;
  readonly depth?: number;
}

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
  const isPriority = task.priority === 0;
  const isRunning = task.todoKeyword === 'DOING' || task.todoKeyword === 'NEXT';
  const isDone = task.todoKeyword === 'DONE';
  const cardNumber = `${pointer.taskIndex + 1}`.padStart(3, '0');
  const descriptionNodes = config.showDescription && task.description.length > 0
    ? h('p', {
        style: {
          margin: '0',
          fontFamily: fonts.body,
          fontSize: config.size === 'large' ? '0.875rem' : sizeConfig.titleSize,
          fontWeight: '400',
          color: colors.greyLight,
          lineHeight: '1.7',
          overflow: 'hidden',
          display: '-webkit-box',
          WebkitLineClamp: sizeConfig.descriptionLineClamp,
          WebkitBoxOrient: 'vertical',
          flexShrink: '1',
        },
      }, renderTextNodes(task.description))
    : null;
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
    config.size === 'large' ? h('div', {
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
          fontSize: sizeConfig.cardNumberSize,
          fontWeight: fontWeight.bold,
          color: colors.greyDark,
          letterSpacing: '0.1em',
          textTransform: 'uppercase',
        },
      }, cardNumber),
      renderKeywordBadge(task.todoKeyword, keywordColor, config.size),
    ]) : h('div', {
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

    h(config.size === 'large' ? 'h3' : 'div', {
      style: config.size === 'large' ? {
        margin: '0',
        fontFamily: fonts.body,
        fontSize: sizeConfig.titleSize,
        fontWeight: fontWeight.bold,
        letterSpacing: '-0.02em',
        color: colors.white,
        textDecoration: isDone ? 'line-through' : 'none',
        opacity: isDone ? '0.7' : '1',
        flexShrink: '0',
        lineHeight: '1.3',
      } : {
        margin: '0',
        fontSize: sizeConfig.titleSize,
        fontWeight: fontWeight.medium,
        color: 'var(--text-primary)',
        textDecoration: isDone ? 'line-through' : 'none',
        opacity: isDone ? '0.7' : '1',
      },
    }, titleNodes),

    descriptionNodes,
    config.size === 'large' ? h('div', {
      style: {
        display: 'flex',
        flexDirection: 'column',
        gap: spacing.md,
        marginTop: 'auto',
      },
    }, [
      renderTags(task.tags, config.size, '6px'),
    ]) : renderTags(task.tags, config.size, '4px'),
  ];

  const cardBody = config.size === 'large'
    ? [
        contentWrapper(cardContent),
        config.showDates
          ? renderCompactDates(task.scheduled, task.deadline, task.closed, task.createdProp)
          : null,
      ]
    : cardContent;
  const cardStyle: Record<string, string> = {
    backgroundColor: colors.asphalt,
    border: `2px solid ${colors.greyDark}`,
    outline: `1px solid ${colors.void}`,
    outlineOffset: '4px',
    padding: sizeConfig.padding,
    display: 'flex',
    flexDirection: 'column',
    gap: sizeConfig.gap,
    cursor: 'pointer',
    height: sizeConfig.height,
    position: 'relative',
    transition: `all ${transitions.verySlow} cubic-bezier(0.4, 0, 0.2, 1)`,
    flexShrink: '0',
    userSelect: 'none',
  };

  if (config.size === 'large') {
    cardStyle['width'] = sizeConfig.width;
    cardStyle['clipPath'] = clipPaths.cardDefault;
  } else {
    cardStyle['borderRadius'] = '6px';
  }

  if (isPriority && config.size === 'large') {
    cardStyle['borderColor'] = colors.redBright;
    cardStyle['outlineColor'] = 'rgba(255, 51, 51, 0.3)';
  } else if (isRunning && config.size === 'large') {
    cardStyle['borderColor'] = colors.pinkBright;
    cardStyle['outlineColor'] = 'rgba(255, 0, 153, 0.3)';
  } else if (config.size === 'large') {
    cardStyle['borderColor'] = colors.greyDark;
    cardStyle['outlineColor'] = colors.void;
  }

  if (isDone) {
    cardStyle['opacity'] = '0.5';
  }

  if (config.depth !== undefined) {
    cardStyle['marginLeft'] = `${config.depth * 16}px`;
    cardStyle['marginBottom'] = spacing.sm;
  }
  const cornerAccent = config.size === 'large' ? h('div', {
    style: {
      position: 'absolute',
      top: '0',
      right: '0',
      width: '24px',
      height: '24px',
      background: `linear-gradient(135deg, ${isPriority ? colors.redBright : isRunning ? colors.pinkBright : colors.cyanDim} 0%, transparent 50%)`,
      clipPath: 'polygon(0 0, 100% 0, 100% 100%)',
      opacity: isPriority || isRunning ? '0.5' : '0.3',
      transition: `opacity ${transitions.normal}`,
      pointerEvents: 'none',
    },
  }) : null;
  const leftAccent = config.size === 'large' ? h('div', {
    class: {
      'task-accent': true,
    },
    style: {
      position: 'absolute',
      left: '0',
      top: '20%',
      bottom: '20%',
      width: '6px',
      background: isPriority ? colors.redBright : isRunning ? colors.pinkBright : colors.cyanBright,
      boxShadow: `0 0 15px ${isPriority ? colors.redBright : isRunning ? colors.pinkBright : colors.cyanBright}`,
      clipPath: 'polygon(0 0, 100% 0, 100% 40%, 0 45%, 0 55%, 100% 60%, 100% 100%, 0 100%)',
      opacity: '0',
      transition: `opacity ${transitions.normal}`,
      pointerEvents: 'none',
    },
  }) : null;

  const cardWithAccents = [
    cornerAccent,
    leftAccent,
    ...cardBody.filter(Boolean),
  ].filter(Boolean);

  return h(`div.${cardClass}.${hoverClass}`, {
    key: `${pointer.file}-${pointer.taskIndex}`,
    class: {
      'priority': isPriority && config.size === 'large',
      'running': isRunning && config.size === 'large',
      'completed': isDone,
    },
    style: cardStyle,
    on: {
      click: () => callbacks.onTaskClick(taskWithPointer),
    },
  }, cardWithAccents);
}

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
