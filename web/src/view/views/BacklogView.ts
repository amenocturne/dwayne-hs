/**
 * Backlog View — Multi-Section Dashboard
 *
 * Shows all active work across multiple states as collapsible sections.
 * Data comes from a single 'all' view fetch, then filtered/sorted client-side.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer } from "../../types/domain.js";
import { renderTaskRow, type QuickAction } from "../components/TaskRow.js";
import {
  colors,
  fonts,
  fontSize,
  fontWeight,
  spacing,
  transitions,
} from "../designSystem.js";

// --- Types ---

export interface BacklogSection {
  readonly id: string;
  readonly label: string;
  readonly accent: string;
  readonly tasks: ReadonlyArray<TaskWithPointer>;
}

export interface BacklogViewCallbacks {
  readonly onTaskClick: (task: TaskWithPointer) => void;
  readonly onQuickAction: (task: TaskWithPointer, keyword: string) => void;
  readonly onToggleSection: (sectionId: string) => void;
  readonly focusedTaskIndex?: number | null;
  readonly animatingOutTasks?: ReadonlyArray<string>;
}

// --- Quick actions for backlog rows ---

const BACKLOG_QUICK_ACTIONS: ReadonlyArray<QuickAction> = [
  { label: "\u25CF", keyword: "TODAY" },
];

// --- Section accent colors ---

const SECTION_ACCENTS: Record<string, string> = {
  overdue: colors.redBright,
  'scheduled-today': colors.orangeBright,
  waiting: '#a855f7',
  soon: '#c084fc',
  'todo-prioritized': '#fbbf24',
  'todo-unprioritized': 'rgba(251, 191, 36, 0.5)',
  projects: '#84cc16',
  someday: 'rgba(249, 115, 22, 0.5)',
};

// --- Date helpers ---

function todayStr(): string {
  const d = new Date();
  const yyyy = d.getFullYear();
  const mm = String(d.getMonth() + 1).padStart(2, '0');
  const dd = String(d.getDate()).padStart(2, '0');
  return `${yyyy}-${mm}-${dd}`;
}

function isOverdue(twp: TaskWithPointer): boolean {
  const today = todayStr();
  const deadline = twp.task.deadline?.date;
  const scheduled = twp.task.scheduled?.date;
  return (deadline !== undefined && deadline !== null && deadline < today)
    || (scheduled !== undefined && scheduled !== null && scheduled < today);
}

function isScheduledToday(twp: TaskWithPointer): boolean {
  const today = todayStr();
  return twp.task.scheduled?.date === today;
}

// --- Section builders ---

function buildSections(tasks: ReadonlyArray<TaskWithPointer>): ReadonlyArray<BacklogSection> {
  const overdue: TaskWithPointer[] = [];
  const scheduledToday: TaskWithPointer[] = [];
  const waiting: TaskWithPointer[] = [];
  const soon: TaskWithPointer[] = [];
  const todoPrioritized: TaskWithPointer[] = [];
  const todoUnprioritized: TaskWithPointer[] = [];
  const projects: TaskWithPointer[] = [];
  const someday: TaskWithPointer[] = [];

  // Terminal states to exclude from backlog
  const excludeKeywords = new Set(['DONE', 'TRASH', 'INBOX']);

  for (const twp of tasks) {
    const kw = twp.task.todoKeyword;

    if (excludeKeywords.has(kw)) continue;

    // Overdue check (can appear alongside other sections, but we deduplicate)
    if (isOverdue(twp)) {
      overdue.push(twp);
      continue;
    }

    // Scheduled today (not yet TODAY keyword)
    if (isScheduledToday(twp) && kw !== 'TODAY') {
      scheduledToday.push(twp);
      continue;
    }

    switch (kw) {
      case 'WAITING':
        waiting.push(twp);
        break;
      case 'SOON':
        soon.push(twp);
        break;
      case 'TODO':
        if (twp.task.priority !== null) {
          todoPrioritized.push(twp);
        } else {
          todoUnprioritized.push(twp);
        }
        break;
      case 'PROJECT':
        projects.push(twp);
        break;
      case 'SOMEDAY':
        someday.push(twp);
        break;
      // TODAY tasks aren't shown in backlog sections (they have their own view)
      default:
        break;
    }
  }

  // Sort overdue by date (most overdue first)
  overdue.sort((a, b) => {
    const dateA = a.task.deadline?.date ?? a.task.scheduled?.date ?? '';
    const dateB = b.task.deadline?.date ?? b.task.scheduled?.date ?? '';
    return dateA.localeCompare(dateB);
  });

  // Sort waiting by age (oldest first — longest-waiting surfaces)
  waiting.sort((a, b) => {
    const dateA = a.task.createdProp?.date ?? '';
    const dateB = b.task.createdProp?.date ?? '';
    return dateA.localeCompare(dateB);
  });

  // Sort TODO prioritized by priority (P0 first)
  todoPrioritized.sort((a, b) => {
    return (a.task.priority ?? 99) - (b.task.priority ?? 99);
  });

  // Sort TODO unprioritized by creation date (newest first)
  todoUnprioritized.sort((a, b) => {
    const dateA = a.task.createdProp?.date ?? '';
    const dateB = b.task.createdProp?.date ?? '';
    return dateB.localeCompare(dateA);
  });

  // Sort someday by age (oldest first — combats SOMEDAY black hole)
  someday.sort((a, b) => {
    const dateA = a.task.createdProp?.date ?? '';
    const dateB = b.task.createdProp?.date ?? '';
    return dateA.localeCompare(dateB);
  });

  const sections: BacklogSection[] = [
    { id: 'overdue', label: 'Overdue', accent: SECTION_ACCENTS['overdue']!, tasks: overdue },
    { id: 'scheduled-today', label: 'Scheduled Today', accent: SECTION_ACCENTS['scheduled-today']!, tasks: scheduledToday },
    { id: 'waiting', label: 'Waiting', accent: SECTION_ACCENTS['waiting']!, tasks: waiting },
    { id: 'soon', label: 'Soon', accent: SECTION_ACCENTS['soon']!, tasks: soon },
    { id: 'todo-prioritized', label: 'TODO (prioritized)', accent: SECTION_ACCENTS['todo-prioritized']!, tasks: todoPrioritized },
    { id: 'todo-unprioritized', label: 'TODO (unprioritized)', accent: SECTION_ACCENTS['todo-unprioritized']!, tasks: todoUnprioritized },
    { id: 'projects', label: 'Projects', accent: SECTION_ACCENTS['projects']!, tasks: projects },
    { id: 'someday', label: 'Someday', accent: SECTION_ACCENTS['someday']!, tasks: someday },
  ];

  // Only show sections with items
  return sections.filter((s) => s.tasks.length > 0);
}

// --- Section header ---

function renderSectionHeader(
  section: BacklogSection,
  collapsed: boolean,
  onToggle: (sectionId: string) => void,
): VNode {
  const chevron = collapsed ? '\u25B6' : '\u25BC';

  return h("div.backlog-section-header", {
    key: `header-${section.id}`,
    style: {
      display: "flex",
      alignItems: "center",
      gap: spacing.sm,
      padding: `${spacing.sm} ${spacing.lg}`,
      cursor: "pointer",
      userSelect: "none",
      borderLeft: `3px solid ${section.accent}`,
      transition: `background ${transitions.fast}`,
    },
    on: {
      click: () => onToggle(section.id),
      mouseenter: (e: Event) => {
        (e.currentTarget as HTMLElement).style.background = colors.rowHover;
      },
      mouseleave: (e: Event) => {
        (e.currentTarget as HTMLElement).style.background = "transparent";
      },
    },
  }, [
    h("span.section-chevron", {
      style: {
        fontSize: "10px",
        color: section.accent,
        width: "14px",
        flexShrink: "0",
      },
    }, chevron),
    h("span.section-label", {
      style: {
        fontFamily: `'${fonts.body}', sans-serif`,
        fontSize: fontSize.sm,
        fontWeight: fontWeight.bold,
        letterSpacing: "0.05em",
        textTransform: "uppercase",
        color: section.accent,
      },
    }, section.label),
    h("span.section-count", {
      style: {
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        color: colors.grey,
        marginLeft: "auto",
      },
    }, `(${section.tasks.length})`),
  ]);
}

// --- Section body ---

function renderSectionBody(
  section: BacklogSection,
  callbacks: BacklogViewCallbacks,
  globalIndexStart: number,
): VNode {
  const focusedIdx = callbacks.focusedTaskIndex ?? null;
  const animatingOut = callbacks.animatingOutTasks ?? [];

  return h("div.backlog-section-body", {
    key: `body-${section.id}`,
    style: {
      borderLeft: `3px solid rgba(255, 255, 255, 0.03)`,
      marginLeft: "0",
    },
  }, section.tasks.map((twp, localIdx) => {
    const globalIdx = globalIndexStart + localIdx;
    const taskKey = `${twp.pointer.file}-${twp.pointer.taskIndex}`;
    return renderTaskRow({
      task: twp,
      showKeyword: true,
      quickActions: BACKLOG_QUICK_ACTIONS as QuickAction[],
      onTaskClick: (t) => callbacks.onTaskClick(t),
      onQuickAction: (t, keyword) => callbacks.onQuickAction(t, keyword),
      isFocused: focusedIdx === globalIdx,
      isAnimatingOut: animatingOut.includes(taskKey),
    });
  }));
}

// --- View header ---

function renderBacklogHeader(totalCount: number): VNode {
  return h("div.backlog-header", {
    style: {
      display: "flex",
      justifyContent: "space-between",
      alignItems: "baseline",
      padding: `${spacing.lg} ${spacing.lg} ${spacing.sm}`,
      borderBottom: `1px solid rgba(255, 255, 255, 0.05)`,
    },
  }, [
    h("h1", {
      style: {
        margin: "0",
        fontFamily: `'${fonts.display}', sans-serif`,
        fontSize: fontSize.xl,
        fontWeight: fontWeight.bold,
        letterSpacing: "0.05em",
        textTransform: "uppercase",
        color: colors.white,
      },
    }, "Backlog"),
    h("span", {
      style: {
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        color: colors.grey,
      },
    }, `${totalCount} tasks`),
  ]);
}

/**
 * Returns the flat ordered task list as rendered in the backlog sections.
 * Used for keyboard navigation to map focusedTaskIndex to the correct task.
 */
export function getBacklogVisibleTasks(
  tasks: ReadonlyArray<TaskWithPointer>,
  collapsedSections: ReadonlyArray<string>,
): ReadonlyArray<TaskWithPointer> {
  const sections = buildSections(tasks);
  const result: TaskWithPointer[] = [];
  for (const section of sections) {
    if (!collapsedSections.includes(section.id)) {
      result.push(...section.tasks);
    }
  }
  return result;
}

// --- Main render ---

export function renderBacklogView(
  tasks: ReadonlyArray<TaskWithPointer>,
  totalCount: number,
  loading: boolean,
  collapsedSections: ReadonlyArray<string>,
  callbacks: BacklogViewCallbacks,
): VNode {
  if (loading) {
    return h("div.backlog-view", {
      style: {
        display: "flex",
        flexDirection: "column",
        height: "100%",
      },
    }, [
      renderBacklogHeader(0),
      h("div.loading", {
        style: {
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          flex: "1",
          color: colors.grey,
          fontFamily: `'${fonts.mono}', monospace`,
          fontSize: fontSize.sm,
        },
      }, "Loading..."),
    ]);
  }

  const sections = buildSections(tasks);

  if (sections.length === 0) {
    return h("div.backlog-view", {
      style: {
        display: "flex",
        flexDirection: "column",
        height: "100%",
      },
    }, [
      renderBacklogHeader(totalCount),
      h("div.empty-state", {
        style: {
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          height: "200px",
          color: colors.grey,
          fontFamily: `'${fonts.body}', sans-serif`,
          fontSize: fontSize.md,
          fontStyle: "italic",
        },
      }, "No active backlog items."),
    ]);
  }

  // Count all tasks across visible sections
  const sectionTaskCount = sections.reduce((acc, s) => acc + s.tasks.length, 0);

  const sectionNodes: VNode[] = [];
  let globalIndex = 0;
  for (const section of sections) {
    const isCollapsed = collapsedSections.includes(section.id);
    sectionNodes.push(renderSectionHeader(section, isCollapsed, callbacks.onToggleSection));
    if (!isCollapsed) {
      sectionNodes.push(renderSectionBody(section, callbacks, globalIndex));
      globalIndex += section.tasks.length;
    }
  }

  return h("div.backlog-view", {
    style: {
      display: "flex",
      flexDirection: "column",
      height: "100%",
    },
  }, [
    renderBacklogHeader(sectionTaskCount),
    h("div.backlog-sections", {
      style: {
        flex: "1",
        overflowY: "auto",
        overflowX: "hidden",
      },
    }, sectionNodes),
  ]);
}
