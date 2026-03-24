import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer, OrgTime } from "../../types/domain.js";
import type { AppState } from "../../types/state.js";
import type { FilePath, TaskIndex } from "../../types/branded.js";
import { renderTextNodes } from "./common/TextNodes.js";
import {
  colors,
  fonts,
  fontSize,
  fontWeight,
  spacing,
  transitions,
  getTodoKeywordColor,
} from "../designSystem.js";

// --- Types ---

export interface DetailPanelCallbacks {
  readonly onClose: () => void;
  readonly onChangeKeyword: (file: FilePath, taskIndex: TaskIndex, keyword: string) => void;
  readonly onChangePriority: (file: FilePath, taskIndex: TaskIndex, priority: number | null) => void;
  readonly onChangeTitle: (file: FilePath, taskIndex: TaskIndex, title: string) => void;
  readonly onChangeDescription: (file: FilePath, taskIndex: TaskIndex, description: string) => void;
  readonly onChangeTags: (file: FilePath, taskIndex: TaskIndex, tags: ReadonlyArray<string>) => void;
  readonly onChangeScheduled: (file: FilePath, taskIndex: TaskIndex, scheduled: OrgTime | null) => void;
  readonly onChangeDeadline: (file: FilePath, taskIndex: TaskIndex, deadline: OrgTime | null) => void;
}

// --- Constants ---

const PANEL_WIDTH = "420px";
const ALL_KEYWORDS = [
  "INBOX", "TODAY", "TODO", "SOON", "SOMEDAY",
  "WAITING", "PROJECT", "LIST", "DONE", "TRASH",
] as const;

const PRIORITY_LABELS: Record<number, string> = { 0: "[#A]", 1: "[#B]", 2: "[#C]" };
const PRIORITY_COLORS: Record<number, string> = {
  0: colors.redBright,
  1: "#fbbf24",
  2: "#84cc16",
};

// --- Helpers ---

function extractTitleText(title: ReadonlyArray<{ readonly type: string; readonly text?: string; readonly url?: string }>): string {
  return title.map((n) => (n.type === "plain" ? n.text ?? "" : n.url ?? "")).join("");
}

function fuzzyMatch(query: string, target: string): boolean {
  const q = query.toLowerCase();
  const t = target.toLowerCase();
  return t.includes(q);
}

function collectAllTags(state: AppState): ReadonlyArray<string> {
  const tagSet = new Set<string>();
  for (const twp of state.taskList.tasks) {
    for (const tag of twp.task.tags) {
      tagSet.add(tag);
    }
  }
  return [...tagSet].sort();
}

// --- Section divider ---

function renderSectionDivider(label: string): VNode {
  return h("div.panel-section-divider", {
    style: {
      display: "flex",
      alignItems: "center",
      gap: spacing.sm,
      margin: `${spacing.lg} 0 ${spacing.sm} 0`,
    },
  }, [
    h("span", {
      style: {
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        fontWeight: fontWeight.bold,
        color: colors.grey,
        textTransform: "uppercase",
        letterSpacing: "0.1em",
        whiteSpace: "nowrap",
      },
    }, `\u2500\u2500\u2500 ${label} `),
    h("span", {
      style: {
        flex: "1",
        height: "1px",
        background: colors.concreteDark,
      },
    }),
  ]);
}

// --- Keyword selector ---

function renderKeywordSelector(
  currentKeyword: string,
  file: FilePath,
  taskIndex: TaskIndex,
  callbacks: DetailPanelCallbacks,
): VNode {
  // State is managed via DOM (simple approach for snabbdom)
  return h("div.panel-keyword-selector", {
    style: {
      display: "flex",
      flexDirection: "column",
      gap: spacing.xs,
    },
  }, [
    // Input for fuzzy matching
    h("input.keyword-search-input", {
      attrs: {
        type: "text",
        placeholder: "Type to filter keywords...",
      },
      style: {
        width: "100%",
        padding: `${spacing.xs} ${spacing.sm}`,
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.sm,
        color: colors.white,
        backgroundColor: colors.concrete,
        border: `1px solid ${colors.concreteDark}`,
        borderRadius: "3px",
        outline: "none",
        boxSizing: "border-box",
      },
      on: {
        input: (e: Event) => {
          const input = e.target as HTMLInputElement;
          const query = input.value;
          const container = input.parentElement;
          if (!container) return;
          const options = container.querySelectorAll(".keyword-option");
          options.forEach((opt) => {
            const kw = (opt as HTMLElement).getAttribute("data-keyword") ?? "";
            (opt as HTMLElement).style.display = fuzzyMatch(query, kw) ? "flex" : "none";
          });
        },
        keydown: (e: KeyboardEvent) => {
          if (e.key === "Enter") {
            const input = e.target as HTMLInputElement;
            const query = input.value.toLowerCase();
            const match = ALL_KEYWORDS.find((kw) => kw.toLowerCase().includes(query));
            if (match && match !== currentKeyword) {
              callbacks.onChangeKeyword(file, taskIndex, match);
            }
            input.value = "";
          }
          if (e.key === "Escape") {
            (e.target as HTMLElement).blur();
          }
        },
      },
    }),
    // Keyword options grid
    h("div.keyword-options", {
      style: {
        display: "flex",
        flexWrap: "wrap",
        gap: spacing.xs,
      },
    }, ALL_KEYWORDS.map((kw) => {
      const color = getTodoKeywordColor(kw);
      const isActive = kw === currentKeyword;
      return h("button.keyword-option", {
        key: kw,
        attrs: { "data-keyword": kw },
        style: {
          padding: `${spacing.xs} ${spacing.sm}`,
          fontFamily: `'${fonts.mono}', monospace`,
          fontSize: fontSize.xs,
          fontWeight: fontWeight.bold,
          letterSpacing: "0.08em",
          textTransform: "uppercase",
          color: isActive ? colors.void : color,
          backgroundColor: isActive ? color : "transparent",
          border: `1px solid ${color}`,
          borderRadius: "3px",
          cursor: isActive ? "default" : "pointer",
          opacity: isActive ? "1" : "0.7",
          transition: `all ${transitions.fast}`,
          whiteSpace: "nowrap",
        },
        on: isActive ? {} : {
          click: (e: Event) => {
            e.stopPropagation();
            callbacks.onChangeKeyword(file, taskIndex, kw);
          },
        },
      }, kw);
    })),
  ]);
}

// --- Priority selector ---

function renderPrioritySelector(
  currentPriority: number | null,
  file: FilePath,
  taskIndex: TaskIndex,
  callbacks: DetailPanelCallbacks,
): VNode {
  // Cycle: null -> 0 -> 1 -> 2 -> null
  const nextPriority = (): number | null => {
    if (currentPriority === null) return 0;
    if (currentPriority === 0) return 1;
    if (currentPriority === 1) return 2;
    return null;
  };

  const label = currentPriority !== null
    ? (PRIORITY_LABELS[currentPriority] ?? `[#${currentPriority}]`)
    : "[#_]";
  const color = currentPriority !== null
    ? (PRIORITY_COLORS[currentPriority] ?? colors.greyLight)
    : colors.grey;

  return h("button.panel-priority", {
    style: {
      fontFamily: `'${fonts.mono}', monospace`,
      fontSize: fontSize.md,
      fontWeight: fontWeight.bold,
      color,
      backgroundColor: "transparent",
      border: `1px solid ${color}`,
      borderRadius: "3px",
      padding: `${spacing.xs} ${spacing.sm}`,
      cursor: "pointer",
      transition: `all ${transitions.fast}`,
      opacity: currentPriority !== null ? "1" : "0.5",
    },
    on: {
      click: (e: Event) => {
        e.stopPropagation();
        callbacks.onChangePriority(file, taskIndex, nextPriority());
      },
    },
  }, `${label}  click to cycle`);
}

// --- Tags editor ---

function renderTagsEditor(
  tags: ReadonlyArray<string>,
  allTags: ReadonlyArray<string>,
  file: FilePath,
  taskIndex: TaskIndex,
  callbacks: DetailPanelCallbacks,
): VNode {
  // Tag pills
  const tagPills = tags.map((tag) =>
    h("span.panel-tag", {
      key: tag,
      style: {
        display: "inline-flex",
        alignItems: "center",
        gap: "4px",
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        color: colors.greyLight,
        border: `1px solid ${colors.grey}`,
        borderRadius: "3px",
        padding: `2px ${spacing.sm}`,
      },
    }, [
      `:${tag}:`,
      h("button.tag-remove", {
        style: {
          background: "none",
          border: "none",
          color: colors.grey,
          cursor: "pointer",
          fontSize: fontSize.xs,
          padding: "0 0 0 2px",
          lineHeight: "1",
        },
        on: {
          click: (e: Event) => {
            e.stopPropagation();
            const newTags = tags.filter((t) => t !== tag);
            callbacks.onChangeTags(file, taskIndex, newTags);
          },
        },
      }, "\u00d7"),
    ])
  );

  // Add tag input with autocomplete
  const addInput = h("div.tag-add-container", {
    style: {
      position: "relative",
      display: "inline-block",
    },
  }, [
    h("input.tag-add-input", {
      attrs: {
        type: "text",
        placeholder: "[+] add tag",
      },
      style: {
        padding: `2px ${spacing.sm}`,
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        color: colors.white,
        backgroundColor: "transparent",
        border: `1px dashed ${colors.grey}`,
        borderRadius: "3px",
        outline: "none",
        width: "100px",
      },
      on: {
        input: (e: Event) => {
          const input = e.target as HTMLInputElement;
          const query = input.value.toLowerCase();
          const container = input.parentElement;
          if (!container) return;
          const dropdown = container.querySelector(".tag-autocomplete") as HTMLElement | null;
          if (!dropdown) return;

          if (query.length === 0) {
            dropdown.style.display = "none";
            return;
          }

          // Filter available tags (not already assigned)
          const suggestions = allTags
            .filter((t) => !tags.includes(t) && t.toLowerCase().includes(query))
            .slice(0, 6);

          dropdown.innerHTML = "";
          if (suggestions.length > 0) {
            dropdown.style.display = "block";
            suggestions.forEach((s) => {
              const opt = document.createElement("div");
              opt.textContent = s;
              opt.style.cssText = `
                padding: 4px 8px;
                cursor: pointer;
                font-family: '${fonts.mono}', monospace;
                font-size: ${fontSize.xs};
                color: ${colors.greyLight};
              `;
              opt.addEventListener("mouseenter", () => {
                opt.style.backgroundColor = colors.rowHover;
              });
              opt.addEventListener("mouseleave", () => {
                opt.style.backgroundColor = "transparent";
              });
              opt.addEventListener("mousedown", (ev) => {
                ev.preventDefault();
                const newTags = [...tags, s];
                callbacks.onChangeTags(file, taskIndex, newTags);
                input.value = "";
                dropdown.style.display = "none";
              });
              dropdown.appendChild(opt);
            });
          } else {
            dropdown.style.display = "none";
          }
        },
        keydown: (e: KeyboardEvent) => {
          if (e.key === "Enter") {
            const input = e.target as HTMLInputElement;
            const value = input.value.trim();
            if (value.length === 0) return;

            // Check for autocomplete match first
            const match = allTags
              .filter((t) => !tags.includes(t))
              .find((t) => t.toLowerCase().includes(value.toLowerCase()));
            const tagToAdd = match ?? value;

            if (!tags.includes(tagToAdd)) {
              const newTags = [...tags, tagToAdd];
              callbacks.onChangeTags(file, taskIndex, newTags);
            }
            input.value = "";
            const container = input.parentElement;
            const dropdown = container?.querySelector(".tag-autocomplete") as HTMLElement | null;
            if (dropdown) dropdown.style.display = "none";
          }
          if (e.key === "Escape") {
            (e.target as HTMLElement).blur();
          }
        },
        blur: (e: Event) => {
          // Delay to allow mousedown on dropdown to fire
          setTimeout(() => {
            const input = e.target as HTMLInputElement;
            const container = input.parentElement;
            const dropdown = container?.querySelector(".tag-autocomplete") as HTMLElement | null;
            if (dropdown) dropdown.style.display = "none";
          }, 150);
        },
      },
    }),
    // Autocomplete dropdown (managed via DOM)
    h("div.tag-autocomplete", {
      style: {
        display: "none",
        position: "absolute",
        top: "100%",
        left: "0",
        minWidth: "120px",
        backgroundColor: colors.asphalt,
        border: `1px solid ${colors.concreteDark}`,
        borderRadius: "3px",
        zIndex: "10",
        maxHeight: "150px",
        overflowY: "auto",
      },
    }),
  ]);

  return h("div.panel-tags-editor", {
    style: {
      display: "flex",
      flexWrap: "wrap",
      gap: spacing.xs,
      alignItems: "center",
    },
  }, [...tagPills, addInput]);
}

// --- Date field ---

function renderDateField(
  label: string,
  value: OrgTime | null,
  file: FilePath,
  taskIndex: TaskIndex,
  onChange: (file: FilePath, taskIndex: TaskIndex, date: OrgTime | null) => void,
): VNode {
  return h("div.panel-date-field", {
    style: {
      display: "flex",
      alignItems: "center",
      gap: spacing.sm,
    },
  }, [
    h("span", {
      style: {
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.sm,
        color: colors.greyLight,
        width: "80px",
        textTransform: "uppercase",
      },
    }, label),
    h("input", {
      attrs: {
        type: "date",
        value: value?.date ?? "",
      },
      style: {
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.sm,
        color: colors.white,
        backgroundColor: colors.concrete,
        border: `1px solid ${colors.concreteDark}`,
        borderRadius: "3px",
        padding: `${spacing.xs} ${spacing.sm}`,
        outline: "none",
        colorScheme: "dark",
      },
      on: {
        change: (e: Event) => {
          const input = e.target as HTMLInputElement;
          if (input.value) {
            const orgTime: OrgTime = {
              date: input.value,
              time: null,
              repeater: null,
              delay: null,
            };
            onChange(file, taskIndex, orgTime);
          } else {
            onChange(file, taskIndex, null);
          }
        },
      },
    }),
    // Clear button
    ...(value ? [
      h("button.date-clear", {
        style: {
          background: "none",
          border: "none",
          color: colors.grey,
          cursor: "pointer",
          fontSize: fontSize.sm,
          padding: "0",
        },
        on: {
          click: (e: Event) => {
            e.stopPropagation();
            onChange(file, taskIndex, null);
          },
        },
      }, "\u00d7"),
    ] : []),
  ]);
}

// --- Editable title ---

function renderEditableTitle(
  task: TaskWithPointer,
  callbacks: DetailPanelCallbacks,
): VNode {
  const titleText = extractTitleText(task.task.title);
  const { file, taskIndex } = task.pointer;

  return h("div.panel-title-wrapper", {
    style: {
      position: "relative",
      cursor: "text",
    },
    hook: {
      insert: (vnode) => {
        const el = vnode.elm as HTMLElement;
        el.addEventListener("click", () => {
          // Replace display span with an input
          const existing = el.querySelector(".panel-title-input") as HTMLInputElement | null;
          if (existing) return; // Already editing

          const display = el.querySelector(".panel-title-display") as HTMLElement;
          if (display) display.style.display = "none";

          const input = document.createElement("input");
          input.className = "panel-title-input";
          input.type = "text";
          input.value = titleText;
          input.style.cssText = `
            width: 100%;
            font-family: '${fonts.body}', sans-serif;
            font-size: ${fontSize.lg};
            font-weight: ${fontWeight.normal};
            color: ${colors.white};
            background-color: ${colors.concrete};
            border: 1px solid ${colors.concreteDark};
            border-radius: 3px;
            padding: ${spacing.xs} ${spacing.sm};
            outline: none;
            box-sizing: border-box;
          `;

          input.addEventListener("keydown", (ev) => {
            if (ev.key === "Enter") {
              const newTitle = input.value.trim();
              if (newTitle && newTitle !== titleText) {
                callbacks.onChangeTitle(file, taskIndex, newTitle);
              }
              input.remove();
              if (display) display.style.display = "";
            }
            if (ev.key === "Escape") {
              input.remove();
              if (display) display.style.display = "";
            }
          });

          input.addEventListener("blur", () => {
            input.remove();
            if (display) display.style.display = "";
          });

          el.appendChild(input);
          input.focus();
          input.select();
        });
      },
    },
  }, [
    h("span.panel-title-display", {
      style: {
        fontFamily: `'${fonts.body}', sans-serif`,
        fontSize: fontSize.lg,
        fontWeight: fontWeight.normal,
        color: colors.white,
        lineHeight: "1.4",
        wordBreak: "break-word",
      },
    }, renderTextNodes(task.task.title)),
  ]);
}

// --- Main render ---

export function renderDetailPanel(
  state: AppState,
  callbacks: DetailPanelCallbacks,
): VNode {
  const { detailPanel } = state;

  if (!detailPanel.open || !detailPanel.task) {
    return h("div.detail-panel-container", {
      style: { display: "none" },
    });
  }

  const twp = detailPanel.task;
  const { task, pointer } = twp;
  const { file, taskIndex } = pointer;
  const allTags = collectAllTags(state);

  const keywordColor = getTodoKeywordColor(task.todoKeyword);

  // Description text
  const descriptionText = task.description
    .map((n) => (n.type === "plain" ? n.text : n.url))
    .join("");

  // Created date
  const createdDate = task.createdProp?.date ?? null;

  // File location (extract filename from path)
  const fileStr = String(pointer.file);
  const fileName = fileStr.split("/").pop() ?? fileStr;

  // Priority display for the header line
  const priorityLabel = task.priority !== null
    ? (PRIORITY_LABELS[task.priority] ?? "")
    : "";
  const priorityColor = task.priority !== null
    ? (PRIORITY_COLORS[task.priority] ?? colors.greyLight)
    : "";

  // Tag string for header line
  const tagStr = task.tags.length > 0
    ? task.tags.map((t) => `:${t}:`).join("")
    : "";

  // --- Panel content ---
  const panelContent = h("div.detail-panel", {
    style: {
      position: "fixed",
      top: "0",
      right: "0",
      width: PANEL_WIDTH,
      height: "100vh",
      backgroundColor: colors.asphalt,
      borderLeft: `1px solid ${colors.concreteDark}`,
      zIndex: "1001",
      display: "flex",
      flexDirection: "column",
      transform: "translateX(0)",
      transition: `transform ${transitions.normal} ease-out`,
      overflowY: "auto",
      overflowX: "hidden",
    },
    hook: {
      insert: (vnode) => {
        const el = vnode.elm as HTMLElement;
        // Slide-in animation
        el.style.transform = "translateX(100%)";
        requestAnimationFrame(() => {
          requestAnimationFrame(() => {
            el.style.transform = "translateX(0)";
          });
        });
      },
    },
  }, [
    // Close button
    h("div.panel-close-row", {
      style: {
        display: "flex",
        justifyContent: "flex-end",
        padding: `${spacing.md} ${spacing.lg}`,
      },
    }, [
      h("button.panel-close-btn", {
        style: {
          background: "none",
          border: "none",
          color: colors.greyLight,
          fontSize: fontSize.lg,
          cursor: "pointer",
          padding: spacing.xs,
          transition: `color ${transitions.fast}`,
        },
        on: {
          click: (e: Event) => {
            e.stopPropagation();
            callbacks.onClose();
          },
          mouseenter: (e: Event) => {
            (e.currentTarget as HTMLElement).style.color = colors.white;
          },
          mouseleave: (e: Event) => {
            (e.currentTarget as HTMLElement).style.color = colors.greyLight;
          },
        },
      }, "\u2715"),
    ]),

    // Header line (org-mode style)
    h("div.panel-header", {
      style: {
        padding: `0 ${spacing.xxl} ${spacing.lg}`,
        display: "flex",
        flexWrap: "wrap",
        alignItems: "baseline",
        gap: spacing.sm,
      },
    }, [
      // Keyword badge
      h("span.panel-keyword-badge", {
        style: {
          fontFamily: `'${fonts.mono}', monospace`,
          fontSize: fontSize.sm,
          fontWeight: fontWeight.bold,
          letterSpacing: "0.08em",
          textTransform: "uppercase",
          color: keywordColor,
          cursor: "default",
        },
      }, task.todoKeyword),

      // Priority (if set)
      ...(priorityLabel
        ? [h("span.panel-header-priority", {
            style: {
              fontFamily: `'${fonts.mono}', monospace`,
              fontSize: fontSize.sm,
              fontWeight: fontWeight.bold,
              color: priorityColor,
            },
          }, priorityLabel)]
        : []),

      // Title (editable)
      renderEditableTitle(twp, callbacks),

      // Tags (right-aligned)
      ...(tagStr
        ? [h("span.panel-header-tags", {
            style: {
              fontFamily: `'${fonts.mono}', monospace`,
              fontSize: fontSize.xs,
              color: colors.grey,
              marginLeft: "auto",
            },
          }, tagStr)]
        : []),
    ]),

    // Description (click to edit)
    h("div.panel-description", {
      style: {
        padding: `0 ${spacing.xxl}`,
        fontFamily: `'${fonts.body}', sans-serif`,
        fontSize: fontSize.md,
        color: descriptionText.trim() ? colors.greyLight : colors.grey,
        whiteSpace: "pre-wrap",
        lineHeight: "1.6",
        wordBreak: "break-word",
        cursor: "pointer",
        minHeight: "24px",
        borderRadius: "3px",
        transition: `background ${transitions.fast}`,
      },
      on: {
        click: (e: Event) => {
          e.stopPropagation();
          const el = (e.target as HTMLElement).closest(".panel-description") as HTMLElement;
          if (!el || el.querySelector("textarea")) return;
          const display = el.querySelector(".desc-display") as HTMLElement;
          if (display) display.style.display = "none";
          const textarea = document.createElement("textarea");
          textarea.value = descriptionText;
          textarea.style.cssText = `
            width: 100%; min-height: 80px; padding: 8px;
            font-family: '${fonts.body}', sans-serif; font-size: ${fontSize.md};
            color: ${colors.white}; background: rgba(255,255,255,0.05);
            border: 1px solid ${colors.cyanBright}; border-radius: 3px;
            resize: vertical; outline: none; line-height: 1.6;
          `;
          el.appendChild(textarea);
          textarea.focus();
          textarea.addEventListener("keydown", (ev) => {
            if (ev.key === "Escape") {
              textarea.remove();
              if (display) display.style.display = "";
            }
          });
          textarea.addEventListener("blur", () => {
            const newDesc = textarea.value;
            if (newDesc !== descriptionText) {
              callbacks.onChangeDescription(file, taskIndex, newDesc);
            }
            textarea.remove();
            if (display) display.style.display = "";
          });
        },
      },
    }, [h("span.desc-display", {}, descriptionText.trim() || "Click to add description...")]),

    // --- STATE section ---
    h("div.panel-body", {
      style: {
        padding: `0 ${spacing.xxl} ${spacing.xxl}`,
      },
    }, [
      renderSectionDivider("STATE"),
      renderKeywordSelector(task.todoKeyword, file, taskIndex, callbacks),

      // --- PRIORITY section ---
      renderSectionDivider("PRIORITY"),
      renderPrioritySelector(task.priority, file, taskIndex, callbacks),

      // --- TAGS section ---
      renderSectionDivider("TAGS"),
      renderTagsEditor(task.tags, allTags, file, taskIndex, callbacks),

      // --- DATES section ---
      renderSectionDivider("DATES"),
      h("div.panel-dates", {
        style: {
          display: "flex",
          flexDirection: "column",
          gap: spacing.sm,
        },
      }, [
        renderDateField("Scheduled", task.scheduled, file, taskIndex, callbacks.onChangeScheduled),
        renderDateField("Deadline", task.deadline, file, taskIndex, callbacks.onChangeDeadline),
      ]),

      // --- META section ---
      renderSectionDivider("META"),
      h("div.panel-meta", {
        style: {
          display: "flex",
          flexDirection: "column",
          gap: spacing.xs,
          fontFamily: `'${fonts.mono}', monospace`,
          fontSize: fontSize.xs,
          color: colors.grey,
        },
      }, [
        ...(createdDate
          ? [h("div", `Created: ${createdDate}`)]
          : []),
        h("div", `File: ${fileName} : ${String(pointer.taskIndex)}`),
      ]),
    ]),
  ]);

  // --- Scrim + panel ---
  return h("div.detail-panel-container", {
    style: {
      position: "fixed",
      top: "0",
      left: "0",
      width: "100vw",
      height: "100vh",
      zIndex: "1000",
      pointerEvents: "auto",
    },
  }, [
    // Scrim
    h("div.detail-panel-scrim", {
      style: {
        position: "absolute",
        top: "0",
        left: "0",
        width: "100%",
        height: "100%",
        backgroundColor: colors.scrim,
        transition: `opacity ${transitions.normal}`,
      },
      on: {
        click: () => callbacks.onClose(),
      },
    }),

    // Panel
    panelContent,
  ]);
}
