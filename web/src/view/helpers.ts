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

/**
 * 3D Carousel Position Calculator
 * 
 * Calculates the position and rotation for a card in a circular 3D carousel.
 * Based on the formula from the video guide:
 * - Each card is positioned on a circle in 3D space (XZ plane)
 * - Cards are evenly distributed around Y-axis
 * - Rotation angle = (position - 1) * (360 / totalCards)
 * 
 * Pure function: (number, number, number, number) => { x, z, rotateY }
 * 
 * @param cardIndex - Zero-based index of the card (0, 1, 2, ...)
 * @param totalCards - Total number of cards in the carousel
 * @param rotationOffset - Current rotation offset in degrees (from user scroll)
 * @param radius - Circle radius in pixels
 * @returns Position { x, z } and rotation { rotateY } for the card
 */
export function calculateCarouselPosition(
  cardIndex: number,
  totalCards: number,
  rotationOffset: number,
  radius: number
): { readonly x: number; readonly z: number; readonly rotateY: number } {
  // Calculate angle per card (evenly distributed around 360°)
  const anglePerCard = 360 / totalCards;
  
  // Calculate this card's angle
  // Position is 1-based in the formula: (position - 1) * anglePerCard
  // Since cardIndex is 0-based, we use: cardIndex * anglePerCard
  const cardAngle = cardIndex * anglePerCard + rotationOffset;
  
  // Convert to radians for trigonometry
  const angleRad = (cardAngle * Math.PI) / 180;
  
  // Position on XZ plane (Y-axis points up)
  // x = sin(angle) * radius (horizontal position)
  // z = cos(angle) * radius (depth position)
  const x = Math.sin(angleRad) * radius;
  const z = Math.cos(angleRad) * radius;
  
  // Rotate card to face center of circle
  // Return the angle itself - we'll handle the sign in the component
  const rotateY = cardAngle;
  
  return { x, z, rotateY };
}
