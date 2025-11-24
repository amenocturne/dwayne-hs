/**
 * Tag Component
 * 
 * Pure function for rendering tags.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import { cardSizes, fonts, colors, clipPaths, fontWeight } from "../../designSystem.js";

type TagSize = keyof typeof cardSizes;

/**
 * Renders a single tag with angled clip-path.
 * Pure function: (tag, size) => VNode
 */
export function renderTag(tag: string, size: TagSize = 'medium'): VNode {
  const sizeConfig = cardSizes[size];
  const isLarge = size === 'large';
  
  return h(
    'span.tag',
    {
      key: tag,
      style: {
        fontFamily: fonts.display,
        backgroundColor: 'rgba(0, 0, 0, 0.4)',
        color: colors.grey,
        border: `1px solid ${colors.greyDark}`,
        padding: sizeConfig.tagPadding,
        fontSize: sizeConfig.tagFontSize,
        fontWeight: fontWeight.semibold,
        letterSpacing: '0.08em',
        textTransform: 'uppercase',
        clipPath: isLarge ? clipPaths.tagAngled : 'none',
        borderRadius: isLarge ? '0' : sizeConfig.tagBorderRadius,
        transition: 'all 0.2s',
      },
    },
    tag.toUpperCase()
  );
}

/**
 * Renders a collection of tags.
 * Pure function: (tags, size) => VNode | null
 */
export function renderTags(
  tags: ReadonlyArray<string>,
  size: TagSize = 'medium',
  gap: string = '6px'
): VNode | null {
  if (tags.length === 0) return null;
  
  return h(
    'div.tags',
    {
      style: {
        display: 'flex',
        flexWrap: 'wrap',
        gap,
      },
    },
    tags.map((tag) => renderTag(tag, size))
  );
}
