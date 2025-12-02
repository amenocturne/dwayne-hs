export const carousel3DConfig = {
  radius: 900,
  perspective: 1000,
  rotationSpeed: 0.3,
  anglePerCard: 20,
  animationDuration: 20000,
  cardWidth: 320,
  cardHeight: 450,
} as const;

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

export const typographyConfig = {
  headerSkew: '-3deg',      // Title skew angle for street racing aesthetic
  badgeLetterSpacing: '0.12em',
  tagLetterSpacing: '0.08em',
  metaLetterSpacing: '0.1em',
} as const;

export const effectsConfig = {
  noiseOpacity: 0.06,       // Background noise texture opacity
  vignetteIntensity: 0.6,   // Radial vignette darkness (0-1)
  glowBlur: '20px',         // Card hover glow blur radius
  glowSpread: '30px',
} as const;

export const breakpoints = {
  mobile: '768px',
  tablet: '1024px',
  desktop: '1400px',
} as const;
