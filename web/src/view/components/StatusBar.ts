import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import { colors, fonts, fontSize, spacing } from "../designSystem.js";

export interface StatusBarData {
  readonly taskCount: number;
  readonly inboxCount: number;
}

export function renderStatusBar(data: StatusBarData): VNode {
  const parts = [
    `${data.taskCount} tasks`,
    `${data.inboxCount} inbox`,
    'no context',
    'energy: \u2014',
  ];

  return h(
    "div.status-bar",
    {
      style: {
        height: "28px",
        minHeight: "28px",
        display: "flex",
        alignItems: "center",
        paddingLeft: spacing.lg,
        paddingRight: spacing.lg,
        backgroundColor: colors.statusBg,
        borderTop: `1px solid ${colors.concreteDark}`,
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        color: colors.statusText,
        letterSpacing: "0.03em",
        zIndex: "200",
        userSelect: "none",
        flexShrink: "0",
      },
    },
    [
      h("span", parts.join(' \u00B7 ')),
    ],
  );
}
