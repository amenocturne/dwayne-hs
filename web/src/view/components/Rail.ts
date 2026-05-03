import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { ActiveView } from "../../types/domain.js";
import { colors, fonts, fontSize, spacing, transitions } from "../designSystem.js";

export interface RailCallbacks {
  readonly onViewChange: (view: ActiveView) => void;
}

interface RailItem {
  readonly view: ActiveView;
  readonly icon: string;
  readonly label: string;
  readonly shortcut: string;
}

const TOP_ITEMS: ReadonlyArray<RailItem> = [
  { view: 'today', icon: '\u2600', label: 'Today', shortcut: '1' },
  { view: 'inbox', icon: '\u2610', label: 'Inbox', shortcut: '2' },
  { view: 'backlog', icon: '\u2630', label: 'Backlog', shortcut: '3' },
  { view: 'lists', icon: '\u266B', label: 'Lists', shortcut: '4' },
];

const BOTTOM_ITEMS: ReadonlyArray<RailItem> = [
  { view: 'garage', icon: '\u2699', label: 'Garage', shortcut: '5' },
];

function renderRailItem(
  item: RailItem,
  isActive: boolean,
  inboxCount: number,
  callbacks: RailCallbacks,
): VNode {
  const showBadge = item.view === 'inbox' && inboxCount > 0;

  return h(
    "button.rail-item",
    {
      key: item.view,
      style: {
        display: "flex",
        alignItems: "center",
        gap: spacing.sm,
        width: "100%",
        padding: `${spacing.sm} ${spacing.md}`,
        border: "none",
        borderRadius: "0",
        background: isActive ? colors.railActive : "transparent",
        color: isActive ? colors.white : colors.greyLight,
        cursor: "pointer",
        fontFamily: `'${fonts.body}', sans-serif`,
        fontSize: fontSize.sm,
        textAlign: "left",
        position: "relative",
        transition: `background ${transitions.normal}, color ${transitions.normal}`,
        whiteSpace: "nowrap",
        overflow: "hidden",
      },
      on: {
        click: () => callbacks.onViewChange(item.view),
      },
    },
    [
      // Icon
      h("span.rail-icon", {
        style: {
          fontSize: "18px",
          width: "32px",
          height: "32px",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          flexShrink: "0",
        },
      }, item.icon),

      // Label (visible when rail is expanded)
      h("span.rail-label", {
        style: {
          opacity: "1",
          fontSize: fontSize.sm,
          letterSpacing: "0.05em",
          textTransform: "uppercase",
        },
      }, item.label),

      // Inbox count badge (removed — just show "INBOX" without count)
      ...(false && showBadge ? [
        h("span.rail-badge", {
          style: {
            position: "absolute",
            top: "4px",
            left: "36px",
            minWidth: "16px",
            height: "16px",
            padding: "0 4px",
            borderRadius: "8px",
            backgroundColor: colors.orangeBright,
            color: colors.void,
            fontSize: "10px",
            fontWeight: "700",
            fontFamily: `'${fonts.mono}', monospace`,
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            lineHeight: "1",
          },
        }, String(inboxCount > 99 ? '99+' : inboxCount)),
      ] : []),
    ],
  );
}

export function renderRail(
  activeView: ActiveView,
  inboxCount: number,
  callbacks: RailCallbacks,
): VNode {
  return h(
    "nav.rail",
    {
      style: {
        width: "180px",
        minWidth: "180px",
        height: "100%",
        backgroundColor: colors.railBg,
        borderRight: `1px solid ${colors.concreteDark}`,
        display: "flex",
        flexDirection: "column",
        justifyContent: "space-between",
        overflow: "hidden",
        zIndex: "200",
        flexShrink: "0",
      },
    },
    [
      // Top section: main views
      h("div.rail-top", {
        style: {
          display: "flex",
          flexDirection: "column",
          paddingTop: spacing.sm,
        },
      }, TOP_ITEMS.map((item) =>
        renderRailItem(item, activeView === item.view, inboxCount, callbacks)
      )),

      // Bottom section: garage (separated by divider)
      h("div.rail-bottom", {
        style: {
          display: "flex",
          flexDirection: "column",
          paddingBottom: spacing.sm,
          borderTop: `1px solid ${colors.concreteDark}`,
        },
      }, BOTTOM_ITEMS.map((item) =>
        renderRailItem(item, activeView === item.view, inboxCount, callbacks)
      )),
    ],
  );
}
