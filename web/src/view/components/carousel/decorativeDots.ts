/**
 * Decorative dots for debugging carousel positioning.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";

/**
 * Render decorative dots around the carousel for debugging positioning.
 * Shows a circle of dots to visualize the carousel's circular layout.
 */
export function renderDecorativeDots(
  radius: number,
  count: number = 100
): ReadonlyArray<VNode> {
  return Array.from({ length: count }, (_, i) => {
    const angle = i * (360 / count);
    const angleRad = (angle * Math.PI) / 180;
    const x = Math.cos(angleRad) * (radius / 3);
    const y = Math.sin(angleRad) * (radius / 3);

    return h('div', {
      key: `dot-${i}`,
      style: {
        position: 'absolute',
        width: '8px',
        height: '8px',
        borderRadius: '50%',
        backgroundColor: 'cyan',
        transformStyle: 'preserve-3d',
        transform: `translate3d(${x}px, ${y}px, 0px)`,
        marginLeft: '-4px',
        marginTop: '-4px',
      },
    });
  });
}
