/**
 * View Selector Component
 * 
 * Pure function rendering view selection buttons.
 */

import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { ViewName } from "../../types/domain.js";

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

/**
 * Renders the view selector buttons.
 * Pure function: (ViewName, ViewSelectorCallbacks) => VNode
 */
export function renderViewSelector(
  currentView: ViewName,
  callbacks: ViewSelectorCallbacks,
): VNode {
  const views: ReadonlyArray<ViewName> = [
    "all",
    "inbox",
    "relevant",
    "todo",
    "waiting",
    "project",
    "someday",
    "notes",
    "list",
    "done",
    "trash",
  ];

  return h(
    "div.view-selector",
    {
      style: {
        display: "flex",
        gap: "8px",
        flexWrap: "wrap",
        justifyContent: "center",
        marginBottom: "32px",
      },
    },
    views.map((view) =>
      h(
        "button",
        {
          key: view,
          style: {
            padding: "8px 16px",
            borderRadius: "6px",
            border:
              currentView === view
                ? "2px solid #646cff"
                : "1px solid var(--card-border)",
            backgroundColor:
              currentView === view
                ? "rgba(100, 108, 255, 0.1)"
                : "var(--card-bg)",
            color: currentView === view ? "#646cff" : "var(--text-primary)",
            fontWeight: currentView === view ? "600" : "400",
            cursor: "pointer",
            transition: "all 0.2s",
            fontSize: "0.875rem",
          },
          on: {
            click: () => callbacks.onViewChange(view),
          },
        },
        VIEW_LABELS[view],
      ),
    ),
  );
}
