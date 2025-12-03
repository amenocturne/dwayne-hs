import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer, TaskNode } from "../../types/domain.js";
import { getTodoKeywordColor, renderTextNodes, calculateCarouselPosition } from "../helpers.js";
import { renderKeywordBadge, renderPriorityBadge } from "./common/Badge.js";
import { renderTags } from "./common/Tag.js";
import { renderCompactDates } from "./common/DateDisplay.js";
import {
  cardSizes,
  priorityColors,
  spacing,
  cssClasses,
  fontWeight,
  colors,
  fonts,
  clipPaths,
  transitions,
} from "../designSystem.js";
import { carousel3DConfig } from "../constants.js";

const PAGE_SIZE = 100;

export interface TaskCardCallbacks {
  readonly onTaskClick: (task: TaskWithPointer) => void;
}

type CardSize = keyof typeof cardSizes;

interface CardConfig {
  readonly size: CardSize;
  readonly showDescription: boolean;
  readonly showDates: boolean;
  readonly depth?: number;
}

function renderUnifiedCard(
  taskWithPointer: TaskWithPointer,
  config: CardConfig,
  callbacks: TaskCardCallbacks
): VNode {
  const { task, pointer } = taskWithPointer;
  const sizeConfig = cardSizes[config.size];
  const keywordColor = getTodoKeywordColor(task.todoKeyword);
  const titleNodes = renderTextNodes(task.title);
  const priorityColor = task.priority !== null ? priorityColors[task.priority as keyof typeof priorityColors] : null;

  const cardClass = config.size === 'large' ? 'task-card' : 'subtask-card';
  const hoverClass = config.size === 'large' ? cssClasses.hoverable : cssClasses.hoverableSubtle;
  const isPriority = task.priority === 0;
  const isRunning = task.todoKeyword === 'DOING' || task.todoKeyword === 'NEXT';
  const isDone = task.todoKeyword === 'DONE';
  const cardNumber = `${pointer.taskIndex + 1}`.padStart(3, '0');
  const descriptionNodes = config.showDescription && task.description.length > 0
    ? h('p', {
        style: {
          margin: '0',
          fontFamily: fonts.body,
          fontSize: config.size === 'large' ? '0.875rem' : sizeConfig.titleSize,
          fontWeight: '400',
          color: colors.greyLight,
          lineHeight: '1.7',
          overflow: 'hidden',
          display: '-webkit-box',
          WebkitLineClamp: sizeConfig.descriptionLineClamp,
          WebkitBoxOrient: 'vertical',
          flexShrink: '1',
        },
      }, renderTextNodes(task.description))
    : null;
  const contentWrapper = config.size === 'large'
    ? (children: ReadonlyArray<VNode | null>) => h('div', {
        style: {
          display: 'flex',
          flexDirection: 'column',
          gap: sizeConfig.gap,
          flex: '1',
          minHeight: '0',
          overflow: 'hidden',
          position: 'relative',
        },
      }, [
        ...children,
        h('div', {
          style: {
            position: 'absolute',
            bottom: '0',
            left: '0',
            right: '0',
            height: '40px',
            background: 'linear-gradient(to bottom, transparent, var(--card-bg))',
            pointerEvents: 'none',
          },
        }),
      ].filter(Boolean))
    : (children: ReadonlyArray<VNode | null>) => h('div', {
        style: {
          display: 'flex',
          flexDirection: 'column',
          gap: sizeConfig.gap,
        },
      }, children.filter(Boolean));

  const cardContent = [
    config.size === 'large' ? h('div', {
      style: {
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'flex-start',
        gap: spacing.md,
        flexShrink: '0',
        marginBottom: spacing.sm,
      },
    }, [
      h('span', {
        style: {
          fontFamily: fonts.display,
          fontSize: sizeConfig.cardNumberSize,
          fontWeight: fontWeight.bold,
          color: colors.greyDark,
          letterSpacing: '0.1em',
          textTransform: 'uppercase',
        },
      }, cardNumber),
      renderKeywordBadge(task.todoKeyword, keywordColor, config.size),
    ]) : h('div', {
      style: {
        display: 'flex',
        alignItems: 'center',
        gap: spacing.sm,
        flexShrink: '0',
      },
    }, [
      renderKeywordBadge(task.todoKeyword, keywordColor, config.size),
      task.priority !== null && priorityColor
        ? renderPriorityBadge(task.priority, priorityColor, config.size)
        : null,
      ].filter(Boolean)),

    h(config.size === 'large' ? 'h3' : 'div', {
      style: config.size === 'large' ? {
        margin: '0',
        fontFamily: fonts.body,
        fontSize: sizeConfig.titleSize,
        fontWeight: fontWeight.bold,
        letterSpacing: '-0.02em',
        color: colors.white,
        textDecoration: isDone ? 'line-through' : 'none',
        opacity: isDone ? '0.7' : '1',
        flexShrink: '0',
        lineHeight: '1.3',
      } : {
        margin: '0',
        fontSize: sizeConfig.titleSize,
        fontWeight: fontWeight.medium,
        color: 'var(--text-primary)',
        textDecoration: isDone ? 'line-through' : 'none',
        opacity: isDone ? '0.7' : '1',
      },
    }, titleNodes),

    descriptionNodes,
    config.size === 'large' ? h('div', {
      style: {
        display: 'flex',
        flexDirection: 'column',
        gap: spacing.md,
        marginTop: 'auto',
      },
    }, [
      renderTags(task.tags, config.size, '6px'),
    ]) : renderTags(task.tags, config.size, '4px'),
  ];

  const cardBody = config.size === 'large'
    ? [
        contentWrapper(cardContent),
        config.showDates
          ? renderCompactDates(task.scheduled, task.deadline, task.closed, task.createdProp)
          : null,
      ]
    : cardContent;
  const cardStyle: Record<string, string> = {
    backgroundColor: colors.asphalt,
    border: `2px solid ${colors.greyDark}`,
    outline: `1px solid ${colors.void}`,
    outlineOffset: '4px',
    padding: sizeConfig.padding,
    display: 'flex',
    flexDirection: 'column',
    gap: sizeConfig.gap,
    cursor: 'pointer',
    height: sizeConfig.height,
    position: 'relative',
    transition: `all ${transitions.verySlow} cubic-bezier(0.4, 0, 0.2, 1)`,
    flexShrink: '0',
  };

  if (config.size === 'large') {
    cardStyle['width'] = sizeConfig.width;
    cardStyle['clipPath'] = clipPaths.cardDefault;
  } else {
    cardStyle['borderRadius'] = '6px';
  }

  if (isPriority && config.size === 'large') {
    cardStyle['borderColor'] = colors.redBright;
    cardStyle['outlineColor'] = 'rgba(255, 51, 51, 0.3)';
  } else if (isRunning && config.size === 'large') {
    cardStyle['borderColor'] = colors.pinkBright;
    cardStyle['outlineColor'] = 'rgba(255, 0, 153, 0.3)';
  } else if (config.size === 'large') {
    cardStyle['borderColor'] = colors.greyDark;
    cardStyle['outlineColor'] = colors.void;
  }

  if (isDone) {
    cardStyle['opacity'] = '0.5';
  }

  if (config.depth !== undefined) {
    cardStyle['marginLeft'] = `${config.depth * 16}px`;
    cardStyle['marginBottom'] = spacing.sm;
  }
  const cornerAccent = config.size === 'large' ? h('div', {
    style: {
      position: 'absolute',
      top: '0',
      right: '0',
      width: '24px',
      height: '24px',
      background: `linear-gradient(135deg, ${isPriority ? colors.redBright : isRunning ? colors.pinkBright : colors.cyanDim} 0%, transparent 50%)`,
      clipPath: 'polygon(0 0, 100% 0, 100% 100%)',
      opacity: isPriority || isRunning ? '0.5' : '0.3',
      transition: `opacity ${transitions.normal}`,
      pointerEvents: 'none',
    },
  }) : null;
  const leftAccent = config.size === 'large' ? h('div', {
    class: {
      'task-accent': true,
    },
    style: {
      position: 'absolute',
      left: '0',
      top: '20%',
      bottom: '20%',
      width: '6px',
      background: isPriority ? colors.redBright : isRunning ? colors.pinkBright : colors.cyanBright,
      boxShadow: `0 0 15px ${isPriority ? colors.redBright : isRunning ? colors.pinkBright : colors.cyanBright}`,
      clipPath: 'polygon(0 0, 100% 0, 100% 40%, 0 45%, 0 55%, 100% 60%, 100% 100%, 0 100%)',
      opacity: '0',
      transition: `opacity ${transitions.normal}`,
      pointerEvents: 'none',
    },
  }) : null;

  const cardWithAccents = [
    cornerAccent,
    leftAccent,
    ...cardBody.filter(Boolean),
  ].filter(Boolean);

  return h(`div.${cardClass}.${hoverClass}`, {
    key: `${pointer.file}-${pointer.taskIndex}`,
    class: {
      'priority': isPriority && config.size === 'large',
      'running': isRunning && config.size === 'large',
      'completed': isDone,
    },
    style: cardStyle,
    on: {
      click: () => callbacks.onTaskClick(taskWithPointer),
    },
  }, cardWithAccents);
}

export function renderTaskCard(
  taskWithPointer: TaskWithPointer,
  callbacks: TaskCardCallbacks
): VNode {
  return renderUnifiedCard(
    taskWithPointer,
    { size: 'large', showDescription: true, showDates: true },
    callbacks
  );
}

export function renderSubtaskCard(
  taskWithPointer: TaskWithPointer,
  callbacks: TaskCardCallbacks
): VNode {
  return renderUnifiedCard(
    taskWithPointer,
    { size: 'medium', showDescription: false, showDates: false },
    callbacks
  );
}

export function renderTaskNodeCard(
  node: TaskNode,
  depth: number,
  callbacks: TaskCardCallbacks
): VNode {
  const { task, pointer, children } = node;

  return h('div', {
    key: `${pointer.file}-${pointer.taskIndex}`,
  }, [
    renderUnifiedCard(
      { task, pointer },
      { size: 'medium', showDescription: false, showDates: false, depth },
      callbacks
    ),
    ...children.map((child) => renderTaskNodeCard(child, depth + 1, callbacks)),
  ]);
}

function calculateCardVisibility(
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

export interface CarouselCallbacks {
  readonly onRotate: (delta: number) => void;
  readonly onLoadMore: () => void;
}

interface CarouselData {
  rotation: number;
  minRotation: number;
  maxRotation: number;
  hasMore: boolean;
  loadingMore: boolean;
  pagesLoaded: number;
}

export function renderTaskGrid(
  tasks: ReadonlyArray<TaskWithPointer>,
  rotation: number,
  callbacks: TaskCardCallbacks,
  carouselCallbacks: CarouselCallbacks,
  hasMore: boolean,
  loadingMore: boolean,
  pagesLoaded: number
): VNode {
  const totalCards = tasks.length;
  const { radius, perspective, anglePerCard, visibleAngleRange } = carousel3DConfig;

  const totalSpan = (totalCards - 1) * anglePerCard;
  const minRotation = -totalSpan;
  const maxRotation = 0;

  const visibleCards = tasks.filter((_, index) => {
    const cardAngle = index * anglePerCard;
    return calculateCardVisibility(cardAngle, rotation).visible;
  }).length;

  return h('div.carousel-scene', {
    style: {
      position: 'absolute',
      top: '0',
      left: '0',
      width: '100%',
      height: '100%',
      perspective: `${perspective}px`,
      perspectiveOrigin: '50% 100%',
      overflow: 'visible',
      pointerEvents: 'auto',
    },
    hook: {
      insert: (vnode) => {
        const elm = vnode.elm as HTMLElement & { __carouselData?: CarouselData };
        // Store initial values
        elm.__carouselData = { rotation, minRotation, maxRotation, hasMore, loadingMore, pagesLoaded };

        elm.addEventListener('wheel', (e: WheelEvent) => {
          e.preventDefault();
          const data = elm.__carouselData!;
          const delta = e.deltaY * carousel3DConfig.rotationSpeed;
          const newRotation = data.rotation + delta;

          if (newRotation < data.minRotation || newRotation > data.maxRotation) {
            return;
          }

          carouselCallbacks.onRotate(delta);

          // Calculate which card is currently in view
          const currentCardIndex = Math.floor(Math.abs(newRotation) / carousel3DConfig.anglePerCard);

          // Trigger load when viewing a card in the last loaded page
          // e.g., pagesLoaded=2 (200 tasks), trigger when currentCardIndex >= 100
          const loadTriggerIndex = (data.pagesLoaded - 1) * PAGE_SIZE;
          if (currentCardIndex >= loadTriggerIndex && data.hasMore && !data.loadingMore) {
            carouselCallbacks.onLoadMore();
          }
        }, { passive: false });
      },
      update: (_oldVnode, vnode) => {
        const elm = vnode.elm as HTMLElement & { __carouselData?: CarouselData };
        // Update values on each render so the wheel handler uses fresh data
        elm.__carouselData = { rotation, minRotation, maxRotation, hasMore, loadingMore, pagesLoaded };
      },
    },
  }, [
    h('div.carousel-container', {
      style: {
        position: 'absolute',
        bottom: '0px',
        left: '50%',
        transformStyle: 'preserve-3d',
        transform: `rotateZ(-90deg) rotateY(56deg) rotateZ(${rotation}deg)`,
        zIndex: '50',
      },
    }, [
      ...Array.from({ length: 100 }, (_, i) => {
        const angle = i * (360 / 100);
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
      }),

      ...tasks
        .map((taskWithPointer, index) => {
          const cardAngle = index * anglePerCard;
          const { visible, opacity } = calculateCardVisibility(cardAngle, rotation);

          if (!visible) {
            return null;
          }

          const angleRad = (cardAngle * Math.PI) / 180;
          const x = Math.cos(angleRad) * radius;
          const y = Math.sin(angleRad) * radius;

          return h('div.carousel-card-wrapper', {
            key: `${taskWithPointer.pointer.file}-${taskWithPointer.pointer.taskIndex}`,
            style: {
              position: 'absolute',
              transformStyle: 'preserve-3d',
              transform: `translate3d(${x}px, ${y}px, 0px) rotateZ(90deg) rotateX(-90deg) rotateY(${-cardAngle}deg)`,
              marginLeft: `-${carousel3DConfig.cardWidth / 2}px`,
              marginTop: `-${carousel3DConfig.cardHeight / 2}px`,
              opacity: `${opacity}`,
              transition: 'opacity 0.3s ease-out',
            },
          }, [
            renderTaskCard(taskWithPointer, callbacks),
          ]);
        })
        .filter((vnode): vnode is VNode => vnode !== null)
    ]),
  ]);
}
