/**
 * Design System
 *
 * Central design tokens for consistent styling across the application.
 * Immutable constants following the philosophy of separating data from logic.
 */

export const spacing = {
  xs: '4px',
  sm: '8px',
  md: '12px',
  lg: '16px',
  xl: '20px',
  xxl: '24px',
} as const;

export const borderRadius = {
  sm: '3px',
  md: '4px',
  lg: '6px',
  xl: '8px',
  xxl: '12px',
} as const;

export const fontSize = {
  xs: '0.65rem',
  sm: '0.75rem',
  md: '0.875rem',
  base: '1rem',
  lg: '1.125rem',
  xl: '1.25rem',
  xxl: '2.5rem',
} as const;

export const fontWeight = {
  normal: '400',
  medium: '500',
  semibold: '600',
  bold: '700',
} as const;

export const lineHeight = {
  tight: '1',
  normal: '1.5',
  relaxed: '1.6',
} as const;

export const transitions = {
  fast: '0.1s',
  normal: '0.2s',
  slow: '0.3s',
} as const;

/**
 * Card configuration by size variant.
 */
export const cardSizes = {
  large: {
    padding: spacing.lg,
    titleSize: fontSize.lg,
    badgePadding: '4px 8px',
    badgeFontSize: fontSize.sm,
    badgeBorderRadius: borderRadius.md,
    tagPadding: '2px 8px',
    tagFontSize: fontSize.sm,
    tagBorderRadius: borderRadius.xxl,
    gap: spacing.md,
    descriptionLineClamp: '3',
    height: '280px',
  },
  medium: {
    padding: spacing.md,
    titleSize: fontSize.md,
    badgePadding: '3px 6px',
    badgeFontSize: fontSize.xs,
    badgeBorderRadius: borderRadius.sm,
    tagPadding: '2px 6px',
    tagFontSize: fontSize.xs,
    tagBorderRadius: borderRadius.xl,
    gap: spacing.sm,
    descriptionLineClamp: '2',
    height: 'auto',
  },
  small: {
    padding: spacing.md,
    titleSize: fontSize.sm,
    badgePadding: '3px 6px',
    badgeFontSize: fontSize.xs,
    badgeBorderRadius: borderRadius.sm,
    tagPadding: '2px 6px',
    tagFontSize: fontSize.xs,
    tagBorderRadius: borderRadius.xl,
    gap: spacing.sm,
    descriptionLineClamp: '1',
    height: 'auto',
  },
} as const;

/**
 * Priority colors
 */
export const priorityColors = {
  0: '#ef4444',
  1: '#f59e0b',
  2: '#f59e0b',
} as const;

/**
 * Shadow values
 */
export const shadows = {
  sm: '0 2px 8px rgba(0, 0, 0, 0.1)',
  md: '0 4px 12px rgba(0, 0, 0, 0.15)',
  lg: '0 8px 24px rgba(0, 0, 0, 0.2)',
} as const;

/**
 * Common CSS class names for declarative styling
 */
export const cssClasses = {
  hoverable: 'hoverable',
  hoverableSubtle: 'hoverable-subtle',
  hoverableProject: 'hoverable-project',
  hoverableButton: 'hoverable-button',
} as const;
