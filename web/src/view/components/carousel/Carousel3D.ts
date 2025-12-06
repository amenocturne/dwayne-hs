/**
 * 3D Carousel component for rendering tasks in a circular carousel layout.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer } from "../../../types/domain.js";
import { carousel3DConfig } from "../../constants.js";
import { renderTaskCard, type TaskCardCallbacks } from "../card/TaskCard.js";
import { calculateCardVisibility } from "./positioning.js";
import { calculateCarouselBounds, calculateRotationDelta, shouldLoadMore } from "./logic.js";
import { renderDecorativeDots } from "./decorativeDots.js";

const PAGE_SIZE = 100;

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

export function renderCarousel3D(
  tasks: ReadonlyArray<TaskWithPointer>,
  rotation: number,
  callbacks: TaskCardCallbacks,
  carouselCallbacks: CarouselCallbacks,
  hasMore: boolean,
  loadingMore: boolean,
  pagesLoaded: number
): VNode {
  const totalCards = tasks.length;
  const { radius, perspective, anglePerCard } = carousel3DConfig;

  const { minRotation, maxRotation } = calculateCarouselBounds(totalCards, anglePerCard);

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
          
          // Use pure function for rotation calculation
          const delta = calculateRotationDelta(
            data.rotation,
            e.deltaY,
            carousel3DConfig.rotationSpeed,
            { min: data.minRotation, max: data.maxRotation }
          );
          
          if (delta === null) return; // Out of bounds
          
          carouselCallbacks.onRotate(delta);
          
          // Use pure function for load-more check
          if (shouldLoadMore(
            data.rotation + delta,
            carousel3DConfig.anglePerCard,
            data.pagesLoaded,
            PAGE_SIZE,
            data.hasMore,
            data.loadingMore
          )) {
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
      ...(carousel3DConfig.showDebugDots ? renderDecorativeDots(radius) : []),

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
