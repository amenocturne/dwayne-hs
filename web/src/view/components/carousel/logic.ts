/**
 * Pure business logic for carousel behavior.
 * All functions are pure - no side effects, no mutations.
 */

/**
 * Calculate the rotation delta for a wheel event.
 * Returns null if the rotation would be out of bounds.
 */
export function calculateRotationDelta(
  currentRotation: number,
  wheelDeltaY: number,
  rotationSpeed: number,
  bounds: { readonly min: number; readonly max: number }
): number | null {
  const delta = wheelDeltaY * rotationSpeed;
  const newRotation = currentRotation + delta;
  
  if (newRotation < bounds.min || newRotation > bounds.max) {
    return null;
  }
  
  return delta;
}

/**
 * Determine if we should trigger load more based on carousel position.
 */
export function shouldLoadMore(
  rotation: number,
  anglePerCard: number,
  pagesLoaded: number,
  pageSize: number,
  hasMore: boolean,
  loadingMore: boolean
): boolean {
  if (!hasMore || loadingMore) {
    return false;
  }
  
  const currentCardIndex = Math.floor(Math.abs(rotation) / anglePerCard);
  const loadTriggerIndex = (pagesLoaded - 1) * pageSize;
  
  return currentCardIndex >= loadTriggerIndex;
}

/**
 * Calculate min/max rotation bounds for the carousel.
 */
export function calculateCarouselBounds(
  totalCards: number,
  anglePerCard: number
): { readonly minRotation: number; readonly maxRotation: number } {
  const totalSpan = (totalCards - 1) * anglePerCard;
  return {
    minRotation: -totalSpan,
    maxRotation: 0,
  };
}
