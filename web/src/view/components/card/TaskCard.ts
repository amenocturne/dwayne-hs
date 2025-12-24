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

type CardSize = keyof typeof cardSizes;

interface CardLayout {
  // Card container
  readonly width: string;
  readonly height: string;
  readonly padding: string;
  readonly gap: string;
  readonly useClipPath: boolean;
  readonly borderRadius: string;

  // Scrolling & overflow
  readonly scrollable: boolean;
  readonly clipContent: boolean;
  readonly showGradientFade: boolean;

  // Interactions
  readonly clickable: boolean;
  readonly showHoverEffects: boolean;
  readonly showLeftAccent: boolean;

  // Typography
  readonly titleTag: 'h3' | 'div';
  readonly titleSize: string;
  readonly titleFontWeight: string;
  readonly descriptionSize: string;
  readonly cardNumberSize: string;

  // Description behavior
  readonly descriptionLineClamp: string;
  readonly descriptionPreserveWhitespace: boolean;
  readonly descriptionAtEnd: boolean;

  // Visual elements
  readonly cornerAccentSize: string;
  readonly showCornerAccent: boolean;
  readonly showStatusBorder: boolean;
  readonly tagSize: CardSize;
  readonly badgeSize: CardSize;
  readonly tagGap: string;
  readonly tagsMarginTop: string;

  // Header layout
  readonly headerLayout: 'split' | 'inline';
  readonly showCardNumber: boolean;
  readonly showPriorityBadge: boolean;

  // Content
  readonly showDates: boolean;
  readonly extraContent: ReadonlyArray<VNode | null>;

  // Classes
  readonly cardClass: string;
  readonly hoverClass: string;

  // Depth (for tree nodes)
  readonly depth: number;
}

const CAROUSEL_LAYOUT: CardLayout = {
  width: cardSizes.large.width,
  height: cardSizes.large.height,
  padding: cardSizes.large.padding,
  gap: cardSizes.large.gap,
  useClipPath: true,
  borderRadius: '0',

  scrollable: false,
  clipContent: true,
  showGradientFade: true,

  clickable: true,
  showHoverEffects: true,
  showLeftAccent: true,

  titleTag: 'h3',
  titleSize: cardSizes.large.titleSize,
  titleFontWeight: fontWeight.bold,
  descriptionSize: '0.875rem',
  cardNumberSize: cardSizes.large.cardNumberSize,

  descriptionLineClamp: cardSizes.large.descriptionLineClamp,
  descriptionPreserveWhitespace: false,
  descriptionAtEnd: false,

  cornerAccentSize: '24px',
  showCornerAccent: true,
  showStatusBorder: true,
  tagSize: 'large',
  badgeSize: 'large',
  tagGap: '6px',
  tagsMarginTop: 'auto',

  headerLayout: 'split',
  showCardNumber: true,
  showPriorityBadge: false,

  showDates: true,
  extraContent: [],

  cardClass: 'task-card',
  hoverClass: cssClasses.hoverable,

  depth: 0,
};

const MODAL_LAYOUT: CardLayout = {
  width: cardSizes.modal.width,
  height: cardSizes.modal.height,
  padding: cardSizes.modal.padding,
  gap: cardSizes.modal.gap,
  useClipPath: true,
  borderRadius: '0',

  scrollable: true,
  clipContent: false,
  showGradientFade: false,

  clickable: false,
  showHoverEffects: false,
  showLeftAccent: false,

  titleTag: 'h3',
  titleSize: cardSizes.modal.titleSize,
  titleFontWeight: fontWeight.bold,
  descriptionSize: '0.875rem',
  cardNumberSize: cardSizes.modal.cardNumberSize,

  descriptionLineClamp: 'none',
  descriptionPreserveWhitespace: true,
  descriptionAtEnd: true,

  cornerAccentSize: '36px',
  showCornerAccent: true,
  showStatusBorder: true,
  tagSize: 'large',
  badgeSize: 'large',
  tagGap: '6px',
  tagsMarginTop: '0',

  headerLayout: 'split',
  showCardNumber: true,
  showPriorityBadge: false,

  showDates: false,
  extraContent: [],

  cardClass: 'task-card',
  hoverClass: '',

  depth: 0,
};

const SUBTASK_LAYOUT: CardLayout = {
  width: cardSizes.medium.width,
  height: cardSizes.medium.height,
  padding: cardSizes.medium.padding,
  gap: cardSizes.medium.gap,
  useClipPath: false,
  borderRadius: '6px',

  scrollable: false,
  clipContent: false,
  showGradientFade: false,

  clickable: true,
  showHoverEffects: true,
  showLeftAccent: false,

  titleTag: 'div',
  titleSize: cardSizes.medium.titleSize,
  titleFontWeight: fontWeight.medium,
  descriptionSize: cardSizes.medium.titleSize,
  cardNumberSize: cardSizes.medium.cardNumberSize,

  descriptionLineClamp: cardSizes.medium.descriptionLineClamp,
  descriptionPreserveWhitespace: false,
  descriptionAtEnd: false,

  cornerAccentSize: '0',
  showCornerAccent: false,
  showStatusBorder: false,
  tagSize: 'medium',
  badgeSize: 'medium',
  tagGap: '4px',
  tagsMarginTop: '0',

  headerLayout: 'inline',
  showCardNumber: false,
  showPriorityBadge: true,

  showDates: false,
  extraContent: [],

  cardClass: 'subtask-card',
  hoverClass: cssClasses.hoverableSubtle,

  depth: 0,
};

function renderCard(
  taskWithPointer: TaskWithPointer,
  layout: CardLayout,
  callbacks: TaskCardCallbacks | null
): VNode {
  const { task, pointer } = taskWithPointer;
  const keywordColor = getTodoKeywordColor(task.todoKeyword);
  const titleNodes = renderTextNodes(task.title);
  const priorityColor = task.priority !== null ? priorityColors[task.priority as keyof typeof priorityColors] : null;

  const isPriority = task.priority === 0;
  const isRunning = task.todoKeyword === 'DOING' || task.todoKeyword === 'NEXT';
  const isDone = task.todoKeyword === 'DONE';
  const cardNumber = `${pointer.taskIndex + 1}`.padStart(3, '0');
  const accentColor = keywordColor;

  // Description element
  const descriptionStyle: Record<string, string> = {
    margin: '0',
    fontFamily: fonts.body,
    fontSize: layout.descriptionSize,
    fontWeight: '400',
    color: colors.greyLight,
    lineHeight: '1.7',
  };

  if (layout.descriptionPreserveWhitespace) {
    descriptionStyle['whiteSpace'] = 'pre-wrap';
  }

  if (layout.clipContent && layout.descriptionLineClamp !== 'none') {
    descriptionStyle['overflow'] = 'hidden';
    descriptionStyle['display'] = '-webkit-box';
    descriptionStyle['WebkitLineClamp'] = layout.descriptionLineClamp;
    descriptionStyle['WebkitBoxOrient'] = 'vertical';
    descriptionStyle['flexShrink'] = '1';
  }

  const descriptionNode = task.description.length > 0
    ? h('p', { style: descriptionStyle }, renderTextNodes(task.description))
    : null;

  // Header element
  const header = layout.headerLayout === 'split'
    ? h('div', {
        style: {
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'flex-start',
          gap: spacing.md,
          flexShrink: '0',
          marginBottom: spacing.sm,
        },
      }, [
        layout.showCardNumber ? h('span', {
          style: {
            fontFamily: fonts.display,
            fontSize: layout.cardNumberSize,
            fontWeight: fontWeight.bold,
            color: colors.greyDark,
            letterSpacing: '0.1em',
            textTransform: 'uppercase',
          },
        }, cardNumber) : null,
        renderKeywordBadge(task.todoKeyword, keywordColor, layout.badgeSize),
      ].filter(Boolean))
    : h('div', {
        style: {
          display: 'flex',
          alignItems: 'center',
          gap: spacing.sm,
          flexShrink: '0',
        },
      }, [
        renderKeywordBadge(task.todoKeyword, keywordColor, layout.badgeSize),
        layout.showPriorityBadge && task.priority !== null && priorityColor
          ? renderPriorityBadge(task.priority, priorityColor, layout.badgeSize)
          : null,
      ].filter(Boolean));

  // Title element
  const titleStyle = layout.titleTag === 'h3'
    ? {
        margin: '0',
        fontFamily: fonts.body,
        fontSize: layout.titleSize,
        fontWeight: layout.titleFontWeight,
        letterSpacing: '-0.02em',
        color: colors.white,
        textDecoration: isDone ? 'line-through' : 'none',
        opacity: isDone ? '0.7' : '1',
        flexShrink: '0',
        lineHeight: '1.3',
      }
    : {
        margin: '0',
        fontSize: layout.titleSize,
        fontWeight: layout.titleFontWeight,
        color: 'var(--text-primary)',
        textDecoration: isDone ? 'line-through' : 'none',
        opacity: isDone ? '0.7' : '1',
      };

  const title = h(layout.titleTag, { style: titleStyle }, titleNodes);

  // Tags element
  const tags = h('div', {
    style: {
      display: 'flex',
      flexDirection: 'column',
      gap: spacing.md,
      marginTop: layout.tagsMarginTop,
    },
  }, [
    renderTags(task.tags, layout.tagSize, layout.tagGap),
  ]);

  // Build content array based on layout
  const contentItems: Array<VNode | null> = layout.descriptionAtEnd
    ? [header, title, tags, ...layout.extraContent, descriptionNode]
    : [header, title, descriptionNode, tags];

  // Wrap content if clipping with gradient
  const content = layout.clipContent
    ? h('div', {
        style: {
          display: 'flex',
          flexDirection: 'column',
          gap: layout.gap,
          flex: '1',
          minHeight: '0',
          overflow: 'hidden',
          position: 'relative',
        },
      }, [
        ...contentItems.filter(Boolean),
        layout.showGradientFade ? h('div', {
          style: {
            position: 'absolute',
            bottom: '0',
            left: '0',
            right: '0',
            height: '40px',
            background: 'linear-gradient(to bottom, transparent, var(--card-bg))',
            pointerEvents: 'none',
          },
        }) : null,
      ].filter(Boolean))
    : h('div', {
        style: {
          display: 'flex',
          flexDirection: 'column',
          gap: layout.gap,
        },
      }, contentItems.filter(Boolean));

  // Card body with optional dates
  const cardBody = [
    content,
    layout.showDates
      ? renderCompactDates(task.scheduled, task.deadline, task.closed, task.createdProp)
      : null,
  ].filter(Boolean);

  // Corner accent
  const cornerAccent = layout.showCornerAccent
    ? h('div', {
        style: {
          position: 'absolute',
          top: '0',
          right: '0',
          width: layout.cornerAccentSize,
          height: layout.cornerAccentSize,
          background: `linear-gradient(135deg, ${keywordColor} 0%, transparent 50%)`,
          clipPath: 'polygon(0 0, 100% 0, 100% 100%)',
          opacity: isPriority || isRunning ? '0.5' : '0.3',
          transition: `opacity ${transitions.normal}`,
          pointerEvents: 'none',
        },
      })
    : null;

  // Left accent (hover glow)
  const leftAccent = layout.showLeftAccent
    ? h('div', {
        class: { 'task-accent': true },
        style: {
          position: 'absolute',
          left: '0',
          top: '20%',
          bottom: '20%',
          width: '6px',
          background: accentColor,
          boxShadow: `0 0 15px ${accentColor}`,
          clipPath: 'polygon(0 0, 100% 0, 100% 40%, 0 45%, 0 55%, 100% 60%, 100% 100%, 0 100%)',
          opacity: '0',
          transition: `opacity ${transitions.normal}`,
          pointerEvents: 'none',
        },
      })
    : null;

  // Convert hex to rgba for CSS variables
  const hexToRgba = (hex: string, alpha: number): string => {
    const r = parseInt(hex.slice(1, 3), 16);
    const g = parseInt(hex.slice(3, 5), 16);
    const b = parseInt(hex.slice(5, 7), 16);
    return `rgba(${r}, ${g}, ${b}, ${alpha})`;
  };

  // Brighten grey/neutral colors for underglow visibility
  const getBrightenedGlowColor = (hex: string, alpha: number): string => {
    const r = parseInt(hex.slice(1, 3), 16);
    const g = parseInt(hex.slice(3, 5), 16);
    const b = parseInt(hex.slice(5, 7), 16);

    // Check if color is grey/neutral (all RGB values close to each other)
    const isGrey = Math.abs(r - g) < 30 && Math.abs(g - b) < 30 && Math.abs(r - b) < 30;

    if (isGrey && r < 160) {
      // For grey colors, use white/light grey for the glow
      return `rgba(220, 220, 220, ${alpha})`;
    }

    return `rgba(${r}, ${g}, ${b}, ${alpha})`;
  };

  // Card style
  const cardStyle: Record<string, string> = {
    backgroundColor: colors.asphalt,
    border: `2px solid ${colors.greyDark}`,
    outline: `1px solid ${colors.void}`,
    outlineOffset: '4px',
    padding: layout.padding,
    display: 'flex',
    flexDirection: 'column',
    gap: layout.gap,
    height: layout.height,
    position: 'relative',
    flexShrink: '0',
    userSelect: 'none',
    '--keyword-color': keywordColor,
    '--keyword-color-dim': hexToRgba(keywordColor, 0.3),
    '--keyword-color-medium': hexToRgba(keywordColor, 0.4),
    '--keyword-color-bright': hexToRgba(keywordColor, 0.5),
    '--keyword-color-glow': getBrightenedGlowColor(keywordColor, 0.6),
  };

  if (layout.width !== 'auto') {
    cardStyle['width'] = layout.width;
  }

  if (layout.useClipPath) {
    cardStyle['clipPath'] = clipPaths.cardDefault;
  } else {
    cardStyle['borderRadius'] = layout.borderRadius;
  }

  if (layout.clickable) {
    cardStyle['cursor'] = 'pointer';
    cardStyle['transition'] = `all ${transitions.verySlow} cubic-bezier(0.4, 0, 0.2, 1)`;
  }

  if (layout.scrollable) {
    cardStyle['overflowY'] = 'auto';
  }

  if (layout.showStatusBorder) {
    if (isPriority || isRunning) {
      cardStyle['borderColor'] = keywordColor;
      cardStyle['outlineColor'] = hexToRgba(keywordColor, 0.3);
    }
  }

  if (isDone) {
    cardStyle['opacity'] = '0.5';
  }

  if (layout.depth > 0) {
    cardStyle['marginLeft'] = `${layout.depth * 16}px`;
    cardStyle['marginBottom'] = spacing.sm;
  }

  // Build final element
  const classNames = [layout.cardClass, layout.hoverClass].filter(Boolean).join('.');
  const elementSelector = classNames ? `div.${classNames}` : 'div';

  const nodeData: Record<string, unknown> = {
    key: `${pointer.file}-${pointer.taskIndex}`,
    class: {
      'priority': isPriority && layout.showStatusBorder,
      'running': isRunning && layout.showStatusBorder,
      'completed': isDone,
    },
    style: cardStyle,
  };

  if (callbacks && layout.clickable) {
    nodeData['on'] = {
      click: (e: MouseEvent) => {
        e.stopPropagation();
        callbacks.onTaskClick(taskWithPointer);
      },
    };
  }

  const cardElement = h(elementSelector, nodeData, [
    cornerAccent,
    leftAccent,
    ...cardBody,
  ].filter(Boolean));

  // Only wrap carousel cards (not subtasks or modal) to add detached underglow effect
  if (layout.showHoverEffects && layout.useClipPath) {
    return h('div.card-wrapper', {
      key: `wrapper-${pointer.file}-${pointer.taskIndex}`,
      class: {
        'priority': isPriority,
        'running': isRunning,
      },
      style: {
        position: 'relative',
        '--keyword-color': keywordColor,
        '--keyword-color-glow': getBrightenedGlowColor(keywordColor, 0.6),
        // Don't add width/height - let card define size
      },
    }, [cardElement]);
  }

  return cardElement;
}

export function renderTaskCard(
  taskWithPointer: TaskWithPointer,
  callbacks: TaskCardCallbacks
): VNode {
  return renderCard(taskWithPointer, CAROUSEL_LAYOUT, callbacks);
}

export function renderSubtaskCard(
  taskWithPointer: TaskWithPointer,
  callbacks: TaskCardCallbacks
): VNode {
  return renderCard(taskWithPointer, SUBTASK_LAYOUT, callbacks);
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
    renderCard(
      { task, pointer },
      { ...SUBTASK_LAYOUT, depth },
      callbacks
    ),
    ...children.map((child) => renderTaskNodeCard(child, depth + 1, callbacks)),
  ]);
}

export function renderModalCard(
  taskWithPointer: TaskWithPointer,
  extraContent: ReadonlyArray<VNode | null>
): VNode {
  return renderCard(
    taskWithPointer,
    { ...MODAL_LAYOUT, extraContent },
    null
  );
}
