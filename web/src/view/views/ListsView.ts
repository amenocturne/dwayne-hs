/**
 * Lists View — Card Grid + Filter Chips
 *
 * Shows LIST keyword tasks as a card grid with tag-based filter chips.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer } from "../../types/domain.js";
import { renderTextNodes } from "../components/common/TextNodes.js";
import {
  colors,
  fonts,
  fontSize,
  fontWeight,
  spacing,
  transitions,
} from "../designSystem.js";

// --- Types ---

export interface ListsViewCallbacks {
  readonly onCardClick: (task: TaskWithPointer) => void;
  readonly onToggleFilter: (tag: string) => void;
}

// --- Type tag color map ---

const TYPE_TAG_COLORS: Record<string, string> = {
  music: '#2dd4bf',
  film: '#c084fc',
  book: '#fbbf24',
  article: '#60a5fa',
  podcast: '#f97316',
  game: '#84cc16',
  show: '#ff6b35',
  album: '#2dd4bf',
  video: '#ff3333',
};

function getTypeColor(tag: string): string {
  return TYPE_TAG_COLORS[tag.toLowerCase()] ?? colors.grey;
}

function getFirstTypeTag(tags: ReadonlyArray<string>): string | null {
  for (const tag of tags) {
    if (TYPE_TAG_COLORS[tag.toLowerCase()] !== undefined) {
      return tag;
    }
  }
  // If no recognized type tag, return the first tag if available
  return tags.length > 0 ? tags[0]! : null;
}

// --- Filter chips ---

function computeAvailableTags(tasks: ReadonlyArray<TaskWithPointer>): ReadonlyArray<string> {
  const tagCounts = new Map<string, number>();
  for (const twp of tasks) {
    for (const tag of twp.task.tags) {
      tagCounts.set(tag, (tagCounts.get(tag) ?? 0) + 1);
    }
  }
  // Sort by frequency (most common first)
  return [...tagCounts.entries()]
    .sort((a, b) => b[1] - a[1])
    .map(([tag]) => tag);
}

function renderFilterChip(
  tag: string,
  isActive: boolean,
  callbacks: ListsViewCallbacks,
): VNode {
  const color = getTypeColor(tag);
  return h("button.filter-chip", {
    key: `chip-${tag}`,
    style: {
      padding: `${spacing.xs} ${spacing.sm}`,
      fontFamily: `'${fonts.mono}', monospace`,
      fontSize: fontSize.xs,
      fontWeight: fontWeight.bold,
      letterSpacing: "0.05em",
      color: isActive ? colors.void : color,
      backgroundColor: isActive ? color : "transparent",
      border: `1px solid ${color}`,
      borderRadius: "3px",
      cursor: "pointer",
      transition: `all ${transitions.fast}`,
      whiteSpace: "nowrap",
      textTransform: "lowercase",
    },
    on: {
      click: () => callbacks.onToggleFilter(tag),
      mouseenter: (e: Event) => {
        if (!isActive) {
          const el = e.currentTarget as HTMLElement;
          el.style.backgroundColor = `${color}22`;
        }
      },
      mouseleave: (e: Event) => {
        if (!isActive) {
          const el = e.currentTarget as HTMLElement;
          el.style.backgroundColor = "transparent";
        }
      },
    },
  }, tag);
}

function renderFilterBar(
  availableTags: ReadonlyArray<string>,
  activeFilters: ReadonlyArray<string>,
  callbacks: ListsViewCallbacks,
): VNode {
  return h("div.filter-bar", {
    style: {
      display: "flex",
      flexWrap: "wrap",
      gap: spacing.xs,
      padding: `${spacing.sm} ${spacing.lg}`,
    },
  }, [
    // "All" chip to clear filters
    h("button.filter-chip-all", {
      key: "chip-all",
      style: {
        padding: `${spacing.xs} ${spacing.sm}`,
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        fontWeight: fontWeight.bold,
        letterSpacing: "0.05em",
        color: activeFilters.length === 0 ? colors.void : colors.greyLight,
        backgroundColor: activeFilters.length === 0 ? colors.greyLight : "transparent",
        border: `1px solid ${colors.greyLight}`,
        borderRadius: "3px",
        cursor: "pointer",
        transition: `all ${transitions.fast}`,
        whiteSpace: "nowrap",
        textTransform: "lowercase",
      },
      on: {
        // Clicking "all" when filters are active should clear them
        // We toggle each active filter off by dispatching empty — handled by parent
        click: () => {
          // Toggle a sentinel value that the parent interprets as "clear all"
          // Since the parent checks tag inclusion, toggling a tag already present removes it.
          // We'll handle clearing all by toggling each active filter.
          // Simpler: just toggle 'all' which the parent can handle as a clear signal.
          callbacks.onToggleFilter('__all__');
        },
      },
    }, "all"),
    ...availableTags.map((tag) =>
      renderFilterChip(tag, activeFilters.includes(tag), callbacks)
    ),
  ]);
}

// --- Card ---

function renderListCard(
  twp: TaskWithPointer,
  callbacks: ListsViewCallbacks,
): VNode {
  const typeTag = getFirstTypeTag(twp.task.tags);
  const borderColor = typeTag ? getTypeColor(typeTag) : colors.grey;

  return h("div.list-card", {
    key: `${twp.pointer.file}-${twp.pointer.taskIndex}`,
    style: {
      width: "200px",
      minHeight: "160px",
      backgroundColor: colors.asphalt,
      borderLeft: `3px solid ${borderColor}`,
      borderRadius: "3px",
      padding: spacing.md,
      cursor: "pointer",
      display: "flex",
      flexDirection: "column",
      justifyContent: "space-between",
      transition: `background ${transitions.normal}, transform ${transitions.fast}`,
      position: "relative",
      overflow: "hidden",
    },
    on: {
      click: () => callbacks.onCardClick(twp),
      mouseenter: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        el.style.background = colors.concrete;
        el.style.transform = "translateY(-2px)";
      },
      mouseleave: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        el.style.background = colors.asphalt;
        el.style.transform = "translateY(0)";
      },
    },
  }, [
    // Type indicator (top-right)
    ...(typeTag ? [
      h("span.card-type", {
        style: {
          position: "absolute",
          top: spacing.sm,
          right: spacing.sm,
          fontFamily: `'${fonts.mono}', monospace`,
          fontSize: fontSize.xs,
          fontWeight: fontWeight.bold,
          letterSpacing: "0.05em",
          textTransform: "uppercase",
          color: borderColor,
          opacity: "0.7",
        },
      }, typeTag),
    ] : []),

    // Title (2 lines max)
    h("div.card-title", {
      style: {
        fontFamily: `'${fonts.body}', sans-serif`,
        fontSize: fontSize.md,
        fontWeight: fontWeight.normal,
        color: colors.white,
        lineHeight: "1.4",
        overflow: "hidden",
        display: "-webkit-box",
        WebkitLineClamp: "2",
        WebkitBoxOrient: "vertical",
        wordBreak: "break-word",
        flex: "1",
        paddingTop: typeTag ? spacing.lg : "0",
      },
    }, renderTextNodes(twp.task.title)),

    // Tags at bottom
    ...(twp.task.tags.length > 0 ? [
      h("div.card-tags", {
        style: {
          display: "flex",
          flexWrap: "wrap",
          gap: "4px",
          marginTop: spacing.sm,
        },
      }, twp.task.tags.map((tag) =>
        h("span.card-tag-pill", {
          key: tag,
          style: {
            fontFamily: `'${fonts.mono}', monospace`,
            fontSize: "0.55rem",
            color: getTypeColor(tag),
            border: `1px solid ${getTypeColor(tag)}33`,
            borderRadius: "2px",
            padding: "1px 4px",
            opacity: "0.7",
          },
        }, tag)
      )),
    ] : []),
  ]);
}

// --- View header ---

function renderListsHeader(count: number): VNode {
  return h("div.lists-header", {
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
    }, "Lists"),
    h("span", {
      style: {
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        color: colors.grey,
      },
    }, `${count} items`),
  ]);
}

// --- Main render ---

export function renderListsView(
  tasks: ReadonlyArray<TaskWithPointer>,
  totalCount: number,
  loading: boolean,
  activeFilters: ReadonlyArray<string>,
  callbacks: ListsViewCallbacks,
): VNode {
  if (loading) {
    return h("div.lists-view", {
      style: {
        display: "flex",
        flexDirection: "column",
        height: "100%",
      },
    }, [
      renderListsHeader(0),
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

  // Compute available filter tags from all tasks
  const availableTags = computeAvailableTags(tasks);

  // Apply filters (OR logic — show task if it has ANY active tag)
  const filteredTasks = activeFilters.length === 0
    ? tasks
    : tasks.filter((twp) =>
        twp.task.tags.some((tag) => activeFilters.includes(tag))
      );

  if (tasks.length === 0) {
    return h("div.lists-view", {
      style: {
        display: "flex",
        flexDirection: "column",
        height: "100%",
      },
    }, [
      renderListsHeader(totalCount),
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
      }, "No list items yet."),
    ]);
  }

  return h("div.lists-view", {
    style: {
      display: "flex",
      flexDirection: "column",
      height: "100%",
    },
  }, [
    renderListsHeader(filteredTasks.length),

    // Filter chips
    ...(availableTags.length > 0
      ? [renderFilterBar(availableTags, activeFilters, callbacks)]
      : []),

    // Card grid
    h("div.lists-grid", {
      style: {
        flex: "1",
        overflowY: "auto",
        overflowX: "hidden",
        padding: spacing.lg,
        display: "flex",
        flexWrap: "wrap",
        gap: spacing.md,
        alignContent: "flex-start",
      },
    }, filteredTasks.map((twp) =>
      renderListCard(twp, callbacks)
    )),
  ]);
}
