/**
 * View Helpers
 * 
 * Pure utility functions for rendering and formatting.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { RichText, OrgTime } from "../types/domain.js";

/**
 * Color mapping for TODO keywords.
 * Phase 4: Uses const assertion for literal type inference.
 */
export const todoKeywordColors = {
  TODO: "#646cff",
  INBOX: "#888",
  WAITING: "#f59e0b",
  DONE: "#10b981",
  PROJECT: "#8b5cf6",
  SOMEDAY: "#6b7280",
  RELEVANT: "#3b82f6",
  NOTES: "#ec4899",
  LIST: "#14b8a6",
  TRASH: "#ef4444",
} as const satisfies Record<string, string>;

/**
 * Type-safe accessor for todoKeywordColors.
 * Returns color for known keywords, default gray for unknown.
 */
export function getTodoKeywordColor(keyword: string): string {
  return (todoKeywordColors as Record<string, string>)[keyword] ?? "#888";
}

/**
 * Renders rich text nodes as VNode array.
 * Pure function: RichText => Array<VNode | string>
 */
export function renderTextNodes(nodes: RichText): Array<VNode | string> {
  return nodes.map((node) => {
    if (node.type === "plain") {
      return node.text;
    } else {
      const displayText = node.title || node.url;
      return h(
        "a",
        {
          attrs: {
            href: node.url,
            target: "_blank",
            rel: "noopener noreferrer",
          },
          style: {
            color: "var(--link-color)",
            textDecoration: "underline",
          },
          on: {
            click: (e: Event) => {
              e.stopPropagation();
            },
          },
        },
        displayText,
      );
    }
  });
}

/**
 * Formats an OrgTime date for display.
 * Pure function: OrgTime | null => string
 */
export function formatDate(orgTime: OrgTime | null): string {
  if (!orgTime) return "";
  const date = new Date(orgTime.date);
  return date.toLocaleDateString("en-US", {
    month: "short",
    day: "numeric",
    year: "numeric",
  });
}
