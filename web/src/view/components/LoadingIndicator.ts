/**
 * Loading Indicator Component
 * 
 * Pure function rendering a loading spinner.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";

/**
 * Renders a loading spinner indicator.
 * Pure function: () => VNode
 */
export function renderLoadingIndicator(): VNode {
  return h(
    "div.loading-indicator",
    {
      style: {
        textAlign: "center",
        padding: "40px 20px",
      },
    },
    h("div", {
      style: {
        display: "inline-block",
        width: "24px",
        height: "24px",
        border: "3px solid var(--card-border)",
        borderTop: "3px solid var(--link-color)",
        borderRadius: "50%",
        animation: "spin 1s linear infinite",
      },
    }),
  );
}
