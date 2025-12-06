/**
 * TaskCard.ts - Re-exports for backward compatibility
 * 
 * This file re-exports components from their new locations.
 * Direct imports from the new locations are preferred:
 * - Card components: ./card/TaskCard.js
 * - Carousel: ./carousel/Carousel3D.js
 */

// Re-export card components
export { 
  renderTaskCard, 
  renderSubtaskCard, 
  renderTaskNodeCard,
  type TaskCardCallbacks,
  type CardVariant 
} from "./card/TaskCard.js";

// Re-export carousel components
export { 
  renderCarousel3D,
  renderCarousel3D as renderTaskGrid,  // Alias for backward compatibility
  type CarouselCallbacks 
} from "./carousel/Carousel3D.js";
