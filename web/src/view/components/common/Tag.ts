/**
 * Tag Component
 * 
 * Pure function for rendering tags.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import { cardSizes } from "../../designSystem.js";

type TagSize = keyof typeof cardSizes;

/**
 * Renders a single tag.
 * Pure function: (tag, size) => VNode
 */
export function renderTag(tag: string, size: TagSize = 'medium'): VNode {
  const sizeConfig = cardSizes[size];
  
  return h(
    'span.tag',
    {
      key: tag,
      style: {
        backgroundColor: 'var(--tag-bg)',
        color: 'var(--tag-text)',
        padding: sizeConfig.tagPadding,
        borderRadius: sizeConfig.tagBorderRadius,
        fontSize: sizeConfig.tagFontSize,
      },
    },
    `#${tag}`
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
