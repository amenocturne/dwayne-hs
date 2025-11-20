/**
 * Search Bar Component
 * 
 * Pure function rendering the search input with debouncing logic.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";

export interface SearchBarCallbacks {
  readonly onSearchChange: (query: string) => void;
  readonly onClearSearch: () => void;
}

/**
 * Renders the search bar.
 * Pure function: (string, SearchBarCallbacks) => VNode
 */
export function renderSearchBar(
  searchQuery: string,
  callbacks: SearchBarCallbacks,
): VNode {
  return h(
    "div.search-bar",
    {
      style: {
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        gap: "12px",
        maxWidth: "800px",
        margin: "0 auto 24px auto",
      },
    },
    [
      h(
        "div",
        {
          style: {
            position: "relative",
            flex: "1",
          },
        },
        [
          h("input", {
            props: {
              type: "text",
              placeholder: "Search tasks...",
              value: searchQuery,
              "aria-label": "Search tasks",
            },
            style: {
              width: "100%",
              padding: "12px 40px 12px 16px",
              borderRadius: "8px",
              border: "1px solid var(--card-border)",
              backgroundColor: "var(--card-bg)",
              color: "var(--text-primary)",
              fontSize: "1rem",
              outline: "none",
              transition: "border-color 0.2s",
            },
            on: {
              input: (e: Event) => {
                const target = e.target as HTMLInputElement;
                callbacks.onSearchChange(target.value);
              },
              focus: (e: Event) => {
                const target = e.target as HTMLInputElement;
                target.style.borderColor = "#646cff";
              },
              blur: (e: Event) => {
                const target = e.target as HTMLInputElement;
                target.style.borderColor = "var(--card-border)";
              },
            },
          }),
          searchQuery.trim() !== ""
            ? h(
                "button",
                {
                  props: {
                    type: "button",
                    "aria-label": "Clear search",
                  },
                  style: {
                    position: "absolute",
                    right: "8px",
                    top: "50%",
                    transform: "translateY(-50%)",
                    padding: "4px 8px",
                    border: "none",
                    background: "transparent",
                    color: "var(--text-secondary)",
                    cursor: "pointer",
                    fontSize: "1.25rem",
                    lineHeight: "1",
                    transition: "color 0.2s",
                  },
                  on: {
                    click: callbacks.onClearSearch,
                    mouseenter: (e: Event) => {
                      const target = e.target as HTMLElement;
                      target.style.color = "var(--text-primary)";
                    },
                    mouseleave: (e: Event) => {
                      const target = e.target as HTMLElement;
                      target.style.color = "var(--text-secondary)";
                    },
                  },
                },
                "×",
              )
            : null,
        ],
      ),
    ],
  );
}
