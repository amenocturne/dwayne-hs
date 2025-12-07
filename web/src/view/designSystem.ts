export const colors = {
  void: '#0a0a0a',
  asphalt: '#121212',
  concrete: '#1a1a1a',
  concreteDark: '#2a2a2a',
  white: '#e8e8e8',
  greyLight: '#888888',
  grey: '#555555',
  greyDark: '#2a2a2a',
  cyanBright: '#00e5ff',
  pinkBright: '#ff0099',
  greenBright: '#b0ff00',
  redBright: '#ff3333',
  cyanDim: '#4a9aa8',
  pinkDim: '#994466',
  greenDim: '#77aa33',
} as const;

export const fonts = {
  display: 'Rajdhani',
  body: 'Rubik',
} as const;

export const spacing = {
  xs: '4px',
  sm: '8px',
  md: '12px',
  lg: '16px',
  xl: '20px',
  xxl: '24px',
  xxxl: '32px',
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
  verySlow: '0.4s',
} as const;

export const cardSizes = {
  large: {
    width: '320px',
    padding: spacing.xxxl,
    titleSize: fontSize.xl,
    badgePadding: '6px 12px',
    badgeFontSize: fontSize.sm,
    badgeBorderRadius: borderRadius.md,
    tagPadding: '4px 10px',
    tagFontSize: fontSize.sm,
    tagBorderRadius: borderRadius.md,
    gap: spacing.lg,
    descriptionLineClamp: '3',
    height: '450px',
    cardNumberSize: fontSize.xs,
  },
  medium: {
    width: 'auto',
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
    cardNumberSize: fontSize.xs,
  },
  small: {
    width: 'auto',
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
    cardNumberSize: fontSize.xs,
  },
} as const;

export const priorityColors = {
  0: colors.redBright,
  1: colors.greenBright,
  2: colors.greenBright,
} as const;

export const shadows = {
  sm: '0 2px 8px rgba(0, 0, 0, 0.3)',
  md: '0 4px 12px rgba(0, 0, 0, 0.4)',
  lg: '0 8px 24px rgba(0, 0, 0, 0.5)',
  glowCyan: '0 0 20px rgba(0, 229, 255, 0.3)',
  glowPink: '0 0 20px rgba(255, 0, 153, 0.3)',
  glowRed: '0 0 20px rgba(255, 51, 51, 0.3)',
} as const;

export const clipPaths = {
  cardDefault: 'polygon(0 0, calc(100% - 24px) 0, 100% 24px, 100% 100%, 24px 100%, 0 calc(100% - 24px))',
  badgeHex: 'polygon(15% 0%, 85% 0%, 100% 50%, 85% 100%, 15% 100%, 0% 50%)',
  tagAngled: 'polygon(8px 0%, 100% 0%, calc(100% - 8px) 100%, 0% 100%)',
} as const;

export const cssClasses = {
  hoverable: 'hoverable',
  hoverableSubtle: 'hoverable-subtle',
  hoverableProject: 'hoverable-project',
  hoverableButton: 'hoverable-button',
} as const;

// Todo keyword colors
export const todoKeywordColors = {
  TODO: "#646cff",
  INBOX: "#888",
  WAITING: "#f59e0b",
  DONE: "#10b981",
  PROJECT: "#8b5cf6",
  SOMEDAY: "#6b7280",
  RELEVANT: "#3b82f6",
  NOTES: "#ec4899",
  LIST: "#14b8a6",
  TRASH: "#ef4444",
} as const satisfies Record<string, string>;

export function getTodoKeywordColor(keyword: string): string {
  return (todoKeywordColors as Record<string, string>)[keyword] ?? "#888";
}
