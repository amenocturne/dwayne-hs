import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer } from "../../types/domain.js";
import { renderTextNodes } from "./common/TextNodes.js";
import {
  colors,
  fonts,
  fontSize,
  fontWeight,
  spacing,
  transitions,
  getTodoKeywordColor,
} from "../designSystem.js";

// --- Types ---

export interface QuickAction {
  readonly label: string;
  readonly keyword: string;
}

export interface TaskRowCallbacks {
  readonly onTaskClick: (task: TaskWithPointer) => void;
  readonly onQuickAction: (task: TaskWithPointer, keyword: string) => void;
}

export interface TaskRowProps {
  readonly task: TaskWithPointer;
  readonly showKeyword: boolean;
  readonly quickActions: ReadonlyArray<QuickAction>;
  readonly onTaskClick: (task: TaskWithPointer) => void;
  readonly onQuickAction: (task: TaskWithPointer, keyword: string) => void;
  readonly isFocused?: boolean;
  readonly isAnimatingOut?: boolean;
}

// --- Priority helpers ---

const PRIORITY_LABELS: Record<number, string> = {
  0: "[#A]",
  1: "[#B]",
  2: "[#C]",
};

const PRIORITY_COLORS: Record<number, string> = {
  0: colors.redBright,
  1: "#fbbf24",
  2: "#84cc16",
};

// --- Relative time ---

function relativeTime(dateStr: string): string {
  const now = Date.now();
  const then = new Date(dateStr).getTime();
  const diffMs = now - then;

  if (diffMs < 0) return "future";

  const minutes = Math.floor(diffMs / 60000);
  if (minutes < 1) return "just now";
  if (minutes < 60) return `${minutes}m ago`;

  const hours = Math.floor(minutes / 60);
  if (hours < 24) return `${hours}h ago`;

  const days = Math.floor(hours / 24);
  if (days < 30) return `${days}d ago`;

  const months = Math.floor(days / 30);
  if (months < 12) return `${months}mo ago`;

  const years = Math.floor(months / 12);
  return `${years}y ago`;
}

// --- Sub-components ---

function renderCompletionDot(
  task: TaskWithPointer,
  onQuickAction: (task: TaskWithPointer, keyword: string) => void,
): VNode {
  return h("button.completion-dot", {
    style: {
      width: "20px",
      height: "20px",
      minWidth: "20px",
      borderRadius: "50%",
      border: `2px solid ${colors.grey}`,
      background: "transparent",
      cursor: "pointer",
      display: "flex",
      alignItems: "center",
      justifyContent: "center",
      padding: "0",
      fontSize: "10px",
      color: colors.grey,
      transition: `all ${transitions.fast}`,
      flexShrink: "0",
    },
    on: {
      click: (e: Event) => {
        e.stopPropagation();
        // Flash green before transitioning
        const el = e.currentTarget as HTMLElement;
        el.style.borderColor = colors.greenBright;
        el.style.color = colors.greenBright;
        el.style.boxShadow = `0 0 8px ${colors.greenBright}`;
        setTimeout(() => {
          onQuickAction(task, "DONE");
        }, 150);
      },
      mouseenter: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        el.style.borderColor = colors.greenBright;
        el.style.color = colors.greenBright;
      },
      mouseleave: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        el.style.borderColor = colors.grey;
        el.style.color = colors.grey;
        el.style.boxShadow = "none";
      },
    },
  }, "\u25CF");
}

function renderKeywordBadge(keyword: string): VNode {
  const color = getTodoKeywordColor(keyword);
  return h("span.task-row-keyword", {
    style: {
      fontFamily: `'${fonts.mono}', monospace`,
      fontSize: fontSize.xs,
      fontWeight: fontWeight.bold,
      letterSpacing: "0.08em",
      textTransform: "uppercase",
      color,
      whiteSpace: "nowrap",
      flexShrink: "0",
    },
  }, keyword);
}

function renderPriority(priority: number): VNode {
  const label = PRIORITY_LABELS[priority] ?? `[#${priority}]`;
  const color = PRIORITY_COLORS[priority] ?? colors.greyLight;
  return h("span.task-row-priority", {
    style: {
      fontFamily: `'${fonts.mono}', monospace`,
      fontSize: fontSize.xs,
      fontWeight: fontWeight.bold,
      color,
      whiteSpace: "nowrap",
      flexShrink: "0",
    },
  }, label);
}

function renderRowTags(tags: ReadonlyArray<string>): VNode {
  const tagStr = tags.map((t) => `:${t}:`).join("");
  return h("span.task-row-tags", {
    style: {
      fontFamily: `'${fonts.mono}', monospace`,
      fontSize: fontSize.xs,
      color: colors.grey,
      whiteSpace: "nowrap",
      flexShrink: "0",
    },
  }, tagStr);
}

function renderRelativeTime(dateStr: string): VNode {
  return h("span.task-row-time", {
    style: {
      fontFamily: `'${fonts.mono}', monospace`,
      fontSize: fontSize.xs,
      color: colors.grey,
      whiteSpace: "nowrap",
      flexShrink: "0",
    },
  }, relativeTime(dateStr));
}

function renderQuickActionButton(
  action: QuickAction,
  task: TaskWithPointer,
  onQuickAction: (task: TaskWithPointer, keyword: string) => void,
): VNode {
  const color = getTodoKeywordColor(action.keyword);
  return h("button.quick-action-btn", {
    key: action.keyword,
    style: {
      padding: `2px ${spacing.xs}`,
      fontSize: fontSize.xs,
      fontFamily: `'${fonts.mono}', monospace`,
      fontWeight: fontWeight.bold,
      letterSpacing: "0.05em",
      color,
      backgroundColor: "transparent",
      border: `1px solid ${color}`,
      borderRadius: "3px",
      cursor: "pointer",
      opacity: "0.8",
      transition: `all ${transitions.fast}`,
      whiteSpace: "nowrap",
    },
    on: {
      click: (e: Event) => {
        e.stopPropagation();
        onQuickAction(task, action.keyword);
      },
      mouseenter: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        el.style.backgroundColor = color;
        el.style.color = colors.void;
        el.style.opacity = "1";
      },
      mouseleave: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        el.style.backgroundColor = "transparent";
        el.style.color = color;
        el.style.opacity = "0.8";
      },
    },
  }, action.label);
}

// --- Main render ---

export function renderTaskRow(props: TaskRowProps): VNode {
  const { task: twp, showKeyword, quickActions, onTaskClick, onQuickAction, isFocused = false, isAnimatingOut = false } = props;
  const { task } = twp;

  // Determine created date for relative time
  const createdDate = task.createdProp?.date ?? null;

  // Build left side elements
  const leftElements: Array<VNode | string> = [];

  // Completion dot
  leftElements.push(renderCompletionDot(twp, onQuickAction));

  // Keyword badge (only when showKeyword is true)
  if (showKeyword) {
    leftElements.push(renderKeywordBadge(task.todoKeyword));
  }

  // Priority
  if (task.priority !== null) {
    leftElements.push(renderPriority(task.priority));
  }

  // Title
  const titleNode = h("span.task-row-title", {
    style: {
      fontFamily: `'${fonts.body}', sans-serif`,
      fontSize: fontSize.base,
      fontWeight: fontWeight.normal,
      color: colors.white,
      overflow: "hidden",
      textOverflow: "ellipsis",
      whiteSpace: "nowrap",
      flex: "1",
      minWidth: "0",
    },
  }, renderTextNodes(task.title));
  leftElements.push(titleNode);

  // Build right side elements
  const rightElements: Array<VNode> = [];

  // Tags
  if (task.tags.length > 0) {
    rightElements.push(renderRowTags(task.tags));
  }

  // Relative time
  if (createdDate) {
    rightElements.push(renderRelativeTime(createdDate));
  }

  // Quick action buttons (shown on hover via CSS-in-JS)
  const quickActionNodes = quickActions.length > 0
    ? h("div.task-row-quick-actions", {
        style: {
          display: "flex",
          gap: spacing.xs,
          opacity: "0",
          transition: `opacity ${transitions.fast}`,
          position: "absolute",
          right: spacing.md,
          top: "50%",
          transform: "translateY(-50%)",
          pointerEvents: "none",
          background: `linear-gradient(to right, transparent, ${colors.void} 8px)`,
          paddingLeft: spacing.lg,
        },
      }, quickActions.map((action) =>
        renderQuickActionButton(action, twp, onQuickAction)
      ))
    : null;

  const focusClass = isFocused ? ".task-row-focused" : "";
  const animClass = isAnimatingOut ? ".task-row-animating-out" : "";

  const animatingOutStyle = isAnimatingOut ? {
    opacity: "0",
    maxHeight: "0",
    overflow: "hidden",
    padding: "0",
    minHeight: "0",
    borderBottom: "none",
  } : {
    opacity: "1",
    maxHeight: "100px",
    overflow: "visible",
    minHeight: "44px",
    padding: `${spacing.sm} ${spacing.md}`,
    borderBottom: `1px solid rgba(255, 255, 255, 0.03)`,
  };

  return h(`div.task-row${focusClass}${animClass}`, {
    key: `${twp.pointer.file}-${twp.pointer.taskIndex}`,
    style: {
      display: "flex",
      alignItems: "center",
      gap: spacing.sm,
      cursor: "pointer",
      transition: `background ${transitions.normal}, border-left 100ms ease, opacity 200ms ease-out, max-height 200ms ease-out`,
      position: "relative",
      borderLeft: isFocused ? `3px solid ${colors.cyanBright}` : '3px solid transparent',
      background: isFocused ? colors.rowFocus : "transparent",
      ...animatingOutStyle,
    },
    on: {
      click: () => onTaskClick(twp),
      mouseenter: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        if (!isFocused) {
          el.style.background = colors.rowHover;
        }
        // Show quick actions
        const qa = el.querySelector(".task-row-quick-actions") as HTMLElement | null;
        if (qa) {
          qa.style.opacity = "1";
          qa.style.pointerEvents = "auto";
        }
      },
      mouseleave: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        if (!isFocused) {
          el.style.background = "transparent";
        }
        // Hide quick actions
        const qa = el.querySelector(".task-row-quick-actions") as HTMLElement | null;
        if (qa) {
          qa.style.opacity = "0";
          qa.style.pointerEvents = "none";
        }
      },
    },
  }, [
    // Left side: dot + keyword + priority + title
    ...leftElements,

    // Right side: tags + time
    ...(rightElements.length > 0
      ? [
          h("div.task-row-right", {
            style: {
              display: "flex",
              alignItems: "center",
              gap: spacing.sm,
              flexShrink: "0",
              marginLeft: "auto",
            },
          }, rightElements),
        ]
      : []),

    // Quick actions overlay
    ...(quickActionNodes ? [quickActionNodes] : []),
  ]);
}
