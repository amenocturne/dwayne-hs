import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import { cardSizes, fontWeight, fonts, clipPaths } from "../../designSystem.js";

type BadgeSize = keyof typeof cardSizes;

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
        fontFamily: fonts.mono,
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
        fontFamily: fonts.mono,
        color,
        fontSize: sizeConfig.badgeFontSize,
        fontWeight: fontWeight.semibold,
      },
    },
    `P${priority}`
  );
}
