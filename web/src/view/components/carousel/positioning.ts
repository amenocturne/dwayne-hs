/**
 * Pure functions for 3D carousel positioning and visibility calculations.
 */

import { carousel3DConfig } from "../../constants.js";

/**
 * Determine if a card should be visible and its opacity based on carousel rotation.
 */
export function calculateCardVisibility(
  cardAngle: number,
  rotation: number
): { readonly visible: boolean; readonly opacity: number } {
  const { visibleAngleRange, fadeTransitionAngle } = carousel3DConfig;
  const viewportAngle = cardAngle + rotation;
  const absAngle = Math.abs(viewportAngle);
  const maxVisibleAngle = visibleAngleRange / 2;

  if (absAngle > maxVisibleAngle + fadeTransitionAngle) {
    return { visible: false, opacity: 0 };
  }

  if (absAngle <= maxVisibleAngle) {
    return { visible: true, opacity: 1 };
  }

  const fadeProgress = (absAngle - maxVisibleAngle) / fadeTransitionAngle;
  const opacity = 1 - fadeProgress;

  return { visible: true, opacity };
}
