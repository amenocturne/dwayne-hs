import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";

export interface SearchBarCallbacks {
  readonly onSearchChange: (query: string) => void;
  readonly onClearSearch: () => void;
}

export function renderSearchBar(
  searchQuery: string,
  callbacks: SearchBarCallbacks,
): VNode {
  return h(
    "div.search-bar",
    {
      style: {
        alignItems: "center",
        justifyContent: "center",
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
              input: (e: Event) => callbacks.onSearchChange((e.target as HTMLInputElement).value),
              focus: (e: Event) => ((e.target as HTMLInputElement).style.borderColor = "#646cff"),
              blur: (e: Event) => ((e.target as HTMLInputElement).style.borderColor = "var(--card-border)"),
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
                    mouseenter: (e: Event) => ((e.target as HTMLElement).style.color = "var(--text-primary)"),
                    mouseleave: (e: Event) => ((e.target as HTMLElement).style.color = "var(--text-secondary)"),
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
