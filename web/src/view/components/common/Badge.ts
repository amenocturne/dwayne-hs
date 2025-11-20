/**
 * Badge Component
 * 
 * Pure function for rendering keyword and priority badges.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import { cardSizes, fontWeight } from "../../designSystem.js";

type BadgeSize = keyof typeof cardSizes;

/**
 * Renders a colored keyword badge.
 * Pure function: (text, color, size) => VNode
 */
export function renderKeywordBadge(
  text: string,
  color: string,
  size: BadgeSize = 'medium'
): VNode {
  const sizeConfig = cardSizes[size];
  
  return h(
    'span.keyword-badge',
    {
      style: {
        backgroundColor: color,
        color: '#fff',
        padding: sizeConfig.badgePadding,
        borderRadius: sizeConfig.badgeBorderRadius,
        fontSize: sizeConfig.badgeFontSize,
        fontWeight: fontWeight.semibold,
        textTransform: 'uppercase',
      },
    },
    text
  );
}

/**
 * Renders a priority badge.
 * Pure function: (priority, color, size) => VNode
 */
export function renderPriorityBadge(
  priority: number,
  color: string,
  size: BadgeSize = 'medium'
): VNode {
  const sizeConfig = cardSizes[size];
  
  return h(
    'span.priority',
    {
      style: {
        color,
        fontSize: sizeConfig.badgeFontSize,
        fontWeight: fontWeight.semibold,
      },
    },
    `P${priority}`
  );
}
