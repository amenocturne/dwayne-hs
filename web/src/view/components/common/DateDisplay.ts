import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { OrgTime } from "../../../types/domain.js";
import { fontSize, fontWeight, spacing } from "../../designSystem.js";

function formatDate(orgTime: OrgTime | null): string {
  if (!orgTime) return "";
  const date = new Date(orgTime.date);
  return date.toLocaleDateString("en-US", {
    month: "short",
    day: "numeric",
    year: "numeric",
  });
}

export function renderCompactDate(icon: string, date: OrgTime | null): VNode | null {
  if (!date) return null;
  
  return h('span', `${icon} ${formatDate(date)}`);
}

export function renderDetailedDate(
  icon: string,
  label: string,
  date: OrgTime | null
): VNode | null {
  if (!date) return null;
  
  return h('div', {
    style: {
      display: 'flex',
      alignItems: 'center',
      gap: spacing.sm,
      fontSize: fontSize.md,
      color: 'var(--text-secondary)',
    },
  }, [
    h('span', { style: { fontSize: fontSize.base } }, icon),
    h('span', { style: { fontWeight: fontWeight.semibold, color: 'var(--text-primary)' } }, label),
    h('span', formatDate(date)),
  ]);
}

export function renderCompactDates(
  scheduled: OrgTime | null,
  deadline: OrgTime | null,
  closed: OrgTime | null,
  created: OrgTime | null
): VNode {
  return h(
    'div.dates',
    {
      style: {
        display: 'flex',
        gap: spacing.md,
        fontSize: fontSize.sm,
        color: 'var(--text-tertiary)',
        paddingTop: spacing.sm,
        borderTop: '1px solid var(--card-border)',
        flexShrink: '0',
      },
    },
    [
      renderCompactDate('📅', scheduled),
      renderCompactDate('⏰', deadline),
      renderCompactDate('✓', closed),
      renderCompactDate('🕐', created),
    ].filter(Boolean)
  );
}

export function renderDetailedDates(
  scheduled: OrgTime | null,
  deadline: OrgTime | null,
  closed: OrgTime | null,
  created: OrgTime | null
): VNode | null {
  const hasDates = scheduled || deadline || closed || created;
  if (!hasDates) return null;
  
  return h('div', {
    style: {
      display: 'flex',
      flexDirection: 'column',
      gap: spacing.sm,
    },
  }, [
    renderDetailedDate('📅', 'Scheduled:', scheduled),
    renderDetailedDate('⏰', 'Deadline:', deadline),
    renderDetailedDate('✓', 'Closed:', closed),
    renderDetailedDate('🕐', 'Created:', created),
  ].filter(Boolean));
}
