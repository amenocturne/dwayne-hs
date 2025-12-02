import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { RichText, OrgTime } from "../types/domain.js";

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

export function getTodoKeywordColor(keyword: string): string {
  return (todoKeywordColors as Record<string, string>)[keyword] ?? "#888";
}

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

export function formatDate(orgTime: OrgTime | null): string {
  if (!orgTime) return "";
  const date = new Date(orgTime.date);
  return date.toLocaleDateString("en-US", {
    month: "short",
    day: "numeric",
    year: "numeric",
  });
}

export function calculateCarouselPosition(
  cardIndex: number,
  totalCards: number,
  rotationOffset: number,
  radius: number,
  anglePerCard: number,
): { readonly x: number; readonly z: number; readonly rotateY: number } {
  const centerOffset = 0;
  const cardAngle = centerOffset + (totalCards - cardIndex - 1) * anglePerCard + rotationOffset;

  const angleRad = (cardAngle * Math.PI) / 180;
  const x = Math.sin(angleRad) * radius;
  const z = Math.cos(angleRad) * radius;

  const rotateY = cardAngle;
  return { x, z, rotateY };
}
