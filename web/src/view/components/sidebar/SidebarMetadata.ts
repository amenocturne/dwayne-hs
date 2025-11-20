/**
 * Sidebar Metadata Components
 * 
 * Components for rendering task metadata (tags, dates, properties).
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { Task } from "../../../types/domain.js";
import { renderSidebarSection } from "./SidebarSection.js";
import { renderDetailedDates } from "../common/DateDisplay.js";
import { fontSize, fontWeight, spacing } from "../../designSystem.js";

/**
 * Renders the tags section.
 * Pure function: (tags) => VNode | null
 */
export function renderTagsSection(tags: ReadonlyArray<string>): VNode | null {
  if (tags.length === 0) return null;
  
  const content = h('div.tags', {
    style: {
      display: 'flex',
      flexWrap: 'wrap',
      gap: spacing.sm,
    },
  }, tags.map((tag) => h('span.tag', {
    key: tag,
    style: {
      backgroundColor: 'var(--tag-bg)',
      color: 'var(--tag-text)',
      padding: `${spacing.xs} ${spacing.md}`,
      borderRadius: '12px',
      fontSize: fontSize.md,
    },
  }, `#${tag}`)));
  
  return renderSidebarSection('Tags', content);
}

/**
 * Renders the dates section.
 * Pure function: (task) => VNode | null
 */
export function renderDatesSection(task: Task): VNode | null {
  const content = renderDetailedDates(
    task.scheduled,
    task.deadline,
    task.closed,
    task.createdProp
  );
  
  if (!content) return null;
  return renderSidebarSection('Dates', content);
}

/**
 * Renders the properties section.
 * Pure function: (properties) => VNode | null
 */
export function renderPropertiesSection(
  properties: ReadonlyArray<readonly [string, string]>
): VNode | null {
  if (properties.length === 0) return null;
  
  const content = h('div', {
    style: {
      display: 'flex',
      flexDirection: 'column',
      gap: spacing.xs,
    },
  }, properties.map(([key, value]) => h('div', {
    key,
    style: {
      display: 'flex',
      gap: spacing.sm,
      fontSize: fontSize.md,
    },
  }, [
    h('span', {
      style: {
        fontWeight: fontWeight.semibold,
        color: 'var(--text-primary)',
        minWidth: '120px',
      },
    }, `${key}:`),
    h('span', {
      style: {
        color: 'var(--text-secondary)',
      },
    }, value),
  ])));
  
  return renderSidebarSection('Properties', content);
}

/**
 * Renders the location section.
 * Pure function: (file, taskIndex) => VNode | null
 */
export function renderLocationSection(file: string, taskIndex: number): VNode | null {
  const content = h('div', {
    style: {
      fontSize: fontSize.md,
      color: 'var(--text-secondary)',
      fontFamily: 'monospace',
      backgroundColor: 'var(--tag-bg)',
      padding: `${spacing.sm} ${spacing.md}`,
      borderRadius: '4px',
    },
  }, [
    h('div', `File: ${file}`),
    h('div', `Task Index: ${taskIndex}`),
  ]);
  
  return renderSidebarSection('Location', content);
}
