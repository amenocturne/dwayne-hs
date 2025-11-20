/**
 * Sidebar Section Component
 * 
 * Reusable section with heading for the sidebar.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import { fontSize, fontWeight, spacing } from "../../designSystem.js";

/**
 * Renders a sidebar section with optional heading and content.
 * Pure function: (title, content) => VNode | null
 */
export function renderSidebarSection(
  title: string,
  content: VNode | ReadonlyArray<VNode | null> | null
): VNode | null {
  // Don't render section if content is null/empty
  if (content === null) return null;
  if (Array.isArray(content) && content.filter(Boolean).length === 0) return null;
  
  return h('div', [
    h('h3', {
      style: {
        margin: `0 0 ${spacing.sm} 0`,
        fontSize: fontSize.sm,
        fontWeight: fontWeight.semibold,
        color: 'var(--text-tertiary)',
        textTransform: 'uppercase',
        letterSpacing: '0.05em',
      },
    }, title),
    Array.isArray(content) ? h('div', content.filter(Boolean)) : content,
  ]);
}

/**
 * Renders a section heading with an action button.
 * Pure function: (title, buttonLabel, onClick) => VNode
 */
export function renderSidebarSectionHeader(
  title: string,
  buttonLabel: string,
  onClick: () => void
): VNode {
  return h('div', {
    style: {
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center',
      marginBottom: spacing.sm,
    },
  }, [
    h('h3', {
      style: {
        margin: '0',
        fontSize: fontSize.sm,
        fontWeight: fontWeight.semibold,
        color: 'var(--text-tertiary)',
        textTransform: 'uppercase',
        letterSpacing: '0.05em',
      },
    }, title),
    h('button.hoverable-button', {
      props: { type: 'button' },
      style: {
        padding: `${spacing.xs} ${spacing.md}`,
        fontSize: fontSize.sm,
        fontWeight: fontWeight.semibold,
        color: 'var(--link-color)',
        backgroundColor: 'rgba(100, 108, 255, 0.1)',
        border: '1px solid var(--link-color)',
        borderRadius: '4px',
        cursor: 'pointer',
      },
      on: {
        click: (e: Event) => {
          e.stopPropagation();
          onClick();
        },
      },
    }, buttonLabel),
  ]);
}
