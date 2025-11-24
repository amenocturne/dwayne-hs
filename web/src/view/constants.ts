/**
 * View Constants
 *
 * Central configuration for UI behavior and interaction parameters.
 * Immutable constants for easy tuning of scroll speeds, animations, and layouts.
 */

/**
 * 3D Carousel configuration
 * Based on circular rotation approach (not arc-based math)
 */
export const carousel3DConfig = {
  // 3D positioning
  radius: 550,              // Circle radius in pixels (Z-direction distance)
  perspective: 1000,        // CSS perspective value (distance to screen)
  
  // Rotation
  rotationSpeed: 0.3,       // Wheel scroll to rotation multiplier (degrees per deltaY)
  
  // Animation
  animationDuration: 20000, // Auto-rotation duration in ms (20 seconds)
  
  // Card dimensions (must match cardSizes.large from designSystem)
  cardWidth: 320,           // From designSystem.cardSizes.large.width
  cardHeight: 450,          // From designSystem.cardSizes.large.height
} as const;

/**
 * Carousel scroll configuration (horizontal fallback)
 */
export const carouselConfig = {
  // Scroll speed multipliers
  dragSpeed: 0.8,           // Mouse drag sensitivity (lower = slower, more controlled)
  wheelScrollSpeed: 10.0,    // Mouse wheel/vertical trackpad scroll speed multiplier
  trackpadScrollSpeed: 10.0, // Horizontal trackpad swipe (1.0 = native feel)

  // Layout
  cardGap: '32px',          // Gap between cards in carousel
  trackPadding: '20px',     // Left/right padding of carousel track

  // Smooth scroll behavior
  smoothScrollDuration: '1.5s', // CSS transition duration for smooth scroll
} as const;

/**
 * Card animation configuration
 */
export const cardAnimationConfig = {
  hoverTranslateY: '-8px',  // How far cards lift on hover
  hoverTransition: '0.4s cubic-bezier(0.4, 0, 0.2, 1)', // Hover animation timing
  accentFadeSpeed: '0.3s',  // Speed of accent bar fade in/out
  cornerAccentOpacity: {
    default: 0.3,
    priority: 0.5,
    running: 0.5,
  },
} as const;

/**
 * Typography configuration
 */
export const typographyConfig = {
  headerSkew: '-3deg',      // Title skew angle for street racing aesthetic
  badgeLetterSpacing: '0.12em',
  tagLetterSpacing: '0.08em',
  metaLetterSpacing: '0.1em',
} as const;

/**
 * Visual effects configuration
 */
export const effectsConfig = {
  noiseOpacity: 0.06,       // Background noise texture opacity
  vignetteIntensity: 0.6,   // Radial vignette darkness (0-1)
  glowBlur: '20px',         // Card hover glow blur radius
  glowSpread: '30px',       // Card hover glow spread
} as const;

/**
 * Responsive breakpoints
 */
export const breakpoints = {
  mobile: '768px',
  tablet: '1024px',
  desktop: '1400px',
} as const;
