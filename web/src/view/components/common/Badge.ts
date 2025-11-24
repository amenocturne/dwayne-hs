/**
 * Badge Component
 * 
 * Pure function for rendering keyword and priority badges.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import { cardSizes, fontWeight, fonts, clipPaths, colors } from "../../designSystem.js";

type BadgeSize = keyof typeof cardSizes;

/**
 * Renders a colored keyword badge with hexagonal clip-path.
 * Pure function: (text, color, size) => VNode
 */
export function renderKeywordBadge(
  text: string,
  color: string,
  size: BadgeSize = 'medium'
): VNode {
  const sizeConfig = cardSizes[size];
  const isLarge = size === 'large';
  
  return h(
    'span.keyword-badge',
    {
      style: {
        fontFamily: fonts.display,
        backgroundColor: `rgba(0, 0, 0, 0.${isLarge ? '5' : '4'})`,
        color: color,
        border: `1px solid ${color}`,
        padding: sizeConfig.badgePadding,
        fontSize: sizeConfig.badgeFontSize,
        fontWeight: fontWeight.bold,
        letterSpacing: '0.12em',
        textTransform: 'uppercase',
        clipPath: isLarge ? clipPaths.badgeHex : 'none',
        borderRadius: isLarge ? '0' : sizeConfig.badgeBorderRadius,
        whiteSpace: 'nowrap',
        transition: 'all 0.3s',
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
