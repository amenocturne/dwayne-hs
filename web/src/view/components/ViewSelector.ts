import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { ViewName } from "../../types/domain.js";
import { VIEW_ORDER } from "../../types/domain.js";

export const VIEW_LABELS: Readonly<Record<ViewName, string>> = {
  all: "All",
  inbox: "Inbox",
  relevant: "Relevant",
  someday: "Someday",
  notes: "Notes",
  list: "List",
  waiting: "Waiting",
  project: "Projects",
  todo: "TODO",
  done: "Done",
  trash: "Trash",
};

export interface ViewSelectorCallbacks {
  readonly onViewChange: (view: ViewName) => void;
}

function clamp(value: number, min: number, max: number): number {
  return Math.max(min, Math.min(max, value));
}

function renderHorizontalArrow(direction: "left" | "right", onClick: () => void, disabled: boolean): VNode {
  const pathD = direction === "left" ? "M10 1L3 9L10 17" : "M3 1L10 9L3 17";
  const pathD2 = direction === "left" ? "M15 1L8 9L15 17" : "M-2 1L5 9L-2 17";

  return h(
    "button.view-selector-arrow",
    {
      style: {
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
        width: "32px",
        height: "32px",
        background: "transparent",
        border: "none",
        cursor: disabled ? "default" : "pointer",
        color: disabled ? "rgba(255,255,255,0.2)" : "#ffffff",
        transition: "transform 0.15s, color 0.15s",
        flexShrink: "0",
        padding: "0",
        opacity: disabled ? "0.3" : "1",
      },
      class: {
        "arrow-interactive": !disabled,
      },
      on: disabled ? {} : {
        click: onClick,
      },
    },
    [
      h("svg", { attrs: { width: "18", height: "18", viewBox: "0 0 18 18", fill: "none" } }, [
        h("path", {
          attrs: { d: pathD, stroke: "currentColor", "stroke-width": "2.5", "stroke-linecap": "round", "stroke-linejoin": "round" },
        }),
        h("path", {
          attrs: { d: pathD2, stroke: "currentColor", "stroke-width": "2.5", "stroke-linecap": "round", "stroke-linejoin": "round", opacity: "0.4" },
        }),
      ]),
    ],
  );
}

export function renderViewSelector(
  currentView: ViewName,
  callbacks: ViewSelectorCallbacks,
): VNode {
  const currentIndex = VIEW_ORDER.indexOf(currentView);
  const maxIndex = VIEW_ORDER.length - 1;

  const navigateTo = (index: number) => {
    const clampedIndex = clamp(index, 0, maxIndex);
    const view = VIEW_ORDER[clampedIndex];
    if (view !== undefined) {
      callbacks.onViewChange(view);
    }
  };

  const canGoLeft = currentIndex > 0;
  const canGoRight = currentIndex < maxIndex;

  // Always show 5 slots centered around current
  const VISIBLE_SLOTS = 5;
  const visibleViews: Array<{ view: ViewName | null; index: number; slot: number }> = [];
  for (let slot = 0; slot < VISIBLE_SLOTS; slot++) {
    const offset = slot - 2; // -2, -1, 0, 1, 2
    const idx = currentIndex + offset;
    if (idx >= 0 && idx < VIEW_ORDER.length) {
      visibleViews.push({ view: VIEW_ORDER[idx] ?? null, index: idx, slot });
    } else {
      visibleViews.push({ view: null, index: idx, slot });
    }
  }

  return h(
    "div.view-selector-container",
    {
      style: {
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        pointerEvents: "auto",
      },
    },
    [
      h(
        "div.view-selector-bar",
        {
          style: {
            display: "flex",
            alignItems: "center",
            gap: "8px",
            position: "relative",
            padding: "12px 0",
          },
        },
        [
          // Fading background
          h("div.selector-bg", {
            style: {
              position: "absolute",
              top: "0",
              bottom: "0",
              left: "-50px",
              right: "-50px",
              background: "linear-gradient(90deg, transparent 0%, rgba(15, 15, 15, 0.9) 15%, rgba(15, 15, 15, 0.95) 50%, rgba(15, 15, 15, 0.9) 85%, transparent 100%)",
              borderTop: "1px solid rgba(255, 255, 255, 0.1)",
              borderBottom: "1px solid rgba(255, 255, 255, 0.1)",
              pointerEvents: "none",
              zIndex: "0",
            },
          }),

          // Left arrow
          h("div", { style: { position: "relative", zIndex: "1" } }, [
            renderHorizontalArrow("left", () => navigateTo(currentIndex - 1), !canGoLeft),
          ]),

          // Items container - fixed width
          h(
            "div.view-selector-items",
            {
              style: {
                display: "flex",
                alignItems: "center",
                position: "relative",
                zIndex: "1",
                width: `${VISIBLE_SLOTS * 110}px`,
              },
            },
            visibleViews.map(({ view, index, slot }) => {
              const distance = Math.abs(slot - 2);
              const isCenter = slot === 2;
              const isEmpty = view === null;

              return h(
                "div.view-selector-slot",
                {
                  key: `slot-${slot}`,
                  style: {
                    display: "flex",
                    alignItems: "center",
                    justifyContent: "center",
                    width: "110px",
                    height: "40px",
                    flexShrink: "0",
                  },
                },
                isEmpty ? [] : [
                  h(
                    "button.view-selector-item",
                    {
                      style: {
                        display: "flex",
                        alignItems: "center",
                        justifyContent: "center",
                        padding: "8px 12px",
                        background: "transparent",
                        border: "none",
                        cursor: "pointer",
                        position: "relative",
                        opacity: String(1 - distance * 0.35),
                        transform: `scale(${1 - distance * 0.08})`,
                        transition: "all 0.2s ease-out",
                      },
                      on: {
                        click: () => navigateTo(index),
                      },
                    },
                    [
                      h("span", {
                        style: {
                          fontFamily: "var(--font-body)",
                          fontSize: isCenter ? "1rem" : "0.8rem",
                          fontWeight: isCenter ? "700" : "500",
                          letterSpacing: "0.08em",
                          textTransform: "uppercase",
                          color: isCenter ? "#ffffff" : "var(--text-secondary)",
                          textShadow: isCenter ? "0 0 20px rgba(255, 255, 255, 0.4)" : "none",
                          transition: "all 0.2s ease-out",
                          whiteSpace: "nowrap",
                        },
                      }, VIEW_LABELS[view]),
                    ],
                  ),
                ],
              );
            }),
          ),

          // Right arrow
          h("div", { style: { position: "relative", zIndex: "1" } }, [
            renderHorizontalArrow("right", () => navigateTo(currentIndex + 1), !canGoRight),
          ]),
        ],
      ),
    ],
  );
}
