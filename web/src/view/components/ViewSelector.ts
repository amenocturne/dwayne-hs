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
        flexDirection: "column",
        gap: "4px",
        pointerEvents: "auto",
      },
    },
    views.map((view) =>
      h(
        "button",
        {
          key: view,
          style: {
            padding: "8px 16px",
            borderRadius: "4px",
            border:
              currentView === view
                ? "2px solid var(--cyan-bright)"
                : "1px solid var(--card-border)",
            backgroundColor:
              currentView === view
                ? "rgba(0, 229, 255, 0.15)"
                : "transparent",
            color: currentView === view ? "var(--cyan-bright)" : "var(--text-primary)",
            fontWeight: currentView === view ? "600" : "400",
            cursor: "pointer",
            transition: "all 0.2s",
            fontSize: "0.875rem",
            textAlign: "left",
            fontFamily: "var(--font-body)",
            pointerEvents: "auto",
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
