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
const PANEL_PADDING = "24px";
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

function tagColor(tag: string): string {
  let hash = 0;
  for (let i = 0; i < tag.length; i++) {
    hash = tag.charCodeAt(i) + ((hash << 5) - hash);
  }
  const hue = Math.abs(hash) % 360;
  return `hsl(${hue}, 50%, 65%)`;
}

function tagBgColor(tag: string): string {
  let hash = 0;
  for (let i = 0; i < tag.length; i++) {
    hash = tag.charCodeAt(i) + ((hash << 5) - hash);
  }
  const hue = Math.abs(hash) % 360;
  return `hsla(${hue}, 50%, 65%, 0.12)`;
}

// --- Keyword dropdown (triggered by clicking the badge) ---

function renderKeywordBadge(
  currentKeyword: string,
  file: FilePath,
  taskIndex: TaskIndex,
  callbacks: DetailPanelCallbacks,
): VNode {
  const keywordColor = getTodoKeywordColor(currentKeyword);

  return h("div.panel-keyword-badge-wrapper", {
    style: {
      position: "relative",
      display: "inline-block",
    },
    hook: {
      insert: (vnode) => {
        const wrapper = vnode.elm as HTMLElement;
        const badge = wrapper.querySelector(".panel-keyword-badge") as HTMLElement;
        if (!badge) return;

        badge.addEventListener("click", (e) => {
          e.stopPropagation();
          // Already open?
          if (wrapper.querySelector(".keyword-dropdown")) return;

          const dropdown = document.createElement("div");
          dropdown.className = "keyword-dropdown";
          dropdown.style.cssText = `
            position: absolute;
            top: calc(100% + 4px);
            left: 0;
            min-width: 180px;
            background-color: ${colors.concrete};
            border: 1px solid ${colors.concreteDark};
            border-radius: 6px;
            z-index: 20;
            overflow: hidden;
            box-shadow: 0 8px 24px rgba(0,0,0,0.5);
          `;

          // Filter input
          const input = document.createElement("input");
          input.type = "text";
          input.placeholder = "Filter...";
          input.style.cssText = `
            width: 100%;
            padding: 8px 10px;
            font-family: '${fonts.mono}', monospace;
            font-size: ${fontSize.sm};
            color: ${colors.white};
            background-color: ${colors.void};
            border: none;
            border-bottom: 1px solid ${colors.concreteDark};
            outline: none;
            box-sizing: border-box;
          `;

          const list = document.createElement("div");
          list.style.cssText = `
            max-height: 240px;
            overflow-y: auto;
          `;

          const renderOptions = (query: string) => {
            list.innerHTML = "";
            ALL_KEYWORDS.forEach((kw) => {
              if (!fuzzyMatch(query, kw)) return;
              const opt = document.createElement("div");
              const kwColor = getTodoKeywordColor(kw);
              const isActive = kw === currentKeyword;
              opt.textContent = kw;
              opt.style.cssText = `
                padding: 6px 10px;
                cursor: ${isActive ? "default" : "pointer"};
                font-family: '${fonts.mono}', monospace;
                font-size: ${fontSize.sm};
                font-weight: 700;
                letter-spacing: 0.08em;
                color: ${isActive ? colors.void : kwColor};
                background-color: ${isActive ? kwColor : "transparent"};
              `;
              if (!isActive) {
                opt.addEventListener("mouseenter", () => {
                  opt.style.backgroundColor = colors.rowHover;
                });
                opt.addEventListener("mouseleave", () => {
                  opt.style.backgroundColor = "transparent";
                });
                opt.addEventListener("mousedown", (ev) => {
                  ev.preventDefault();
                  callbacks.onChangeKeyword(file, taskIndex, kw);
                  dropdown.remove();
                });
              }
              list.appendChild(opt);
            });
          };

          input.addEventListener("input", () => renderOptions(input.value));
          input.addEventListener("keydown", (ev) => {
            if (ev.key === "Enter") {
              const query = input.value.toLowerCase();
              const match = ALL_KEYWORDS.find((kw) => kw.toLowerCase().includes(query));
              if (match && match !== currentKeyword) {
                callbacks.onChangeKeyword(file, taskIndex, match);
              }
              dropdown.remove();
            }
            if (ev.key === "Escape") {
              dropdown.remove();
            }
          });

          renderOptions("");
          dropdown.appendChild(input);
          dropdown.appendChild(list);
          wrapper.appendChild(dropdown);
          input.focus();

          // Close on outside click
          const closeHandler = (ev: MouseEvent) => {
            if (!wrapper.contains(ev.target as Node)) {
              dropdown.remove();
              document.removeEventListener("mousedown", closeHandler);
            }
          };
          setTimeout(() => document.addEventListener("mousedown", closeHandler), 0);
        });
      },
    },
  }, [
    h("span.panel-keyword-badge", {
      style: {
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.sm,
        fontWeight: fontWeight.bold,
        letterSpacing: "0.08em",
        textTransform: "uppercase",
        color: colors.void,
        backgroundColor: keywordColor,
        padding: "2px 8px",
        borderRadius: "3px",
        cursor: "pointer",
        userSelect: "none",
        transition: `opacity ${transitions.fast}`,
      },
    }, currentKeyword),
  ]);
}

// --- Priority (inline, click-to-cycle) ---

function renderInlinePriority(
  currentPriority: number | null,
  file: FilePath,
  taskIndex: TaskIndex,
  callbacks: DetailPanelCallbacks,
): VNode {
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

  // Always visible when set, muted placeholder when unset
  const isSet = currentPriority !== null;

  return h("span.panel-priority-inline", {
    style: {
      fontFamily: `'${fonts.mono}', monospace`,
      fontSize: fontSize.sm,
      fontWeight: fontWeight.bold,
      color,
      cursor: "pointer",
      userSelect: "none",
      opacity: isSet ? "1" : "0.35",
      transition: `opacity ${transitions.fast}, color ${transitions.fast}`,
    },
    on: {
      click: (e: Event) => {
        e.stopPropagation();
        callbacks.onChangePriority(file, taskIndex, nextPriority());
      },
      mouseenter: (e: Event) => {
        if (!isSet) (e.currentTarget as HTMLElement).style.opacity = "0.7";
      },
      mouseleave: (e: Event) => {
        if (!isSet) (e.currentTarget as HTMLElement).style.opacity = "0.35";
      },
    },
  }, label);
}

// --- Editable title ---

function renderEditableTitle(
  task: TaskWithPointer,
  callbacks: DetailPanelCallbacks,
): VNode {
  const titleText = extractTitleText(task.task.title);
  const { file, taskIndex } = task.pointer;

  return h("span.panel-title-wrapper", {
    style: {
      cursor: "text",
      flex: "1",
      minWidth: "0",
    },
    hook: {
      insert: (vnode) => {
        const el = vnode.elm as HTMLElement;
        el.addEventListener("click", () => {
          const existing = el.querySelector(".panel-title-input") as HTMLInputElement | null;
          if (existing) return;

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
            padding: 2px 6px;
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

// --- Tags line ---

function renderTagsLine(
  tags: ReadonlyArray<string>,
  allTags: ReadonlyArray<string>,
  file: FilePath,
  taskIndex: TaskIndex,
  callbacks: DetailPanelCallbacks,
): VNode {
  const tagPills = tags.map((tag) => {
    const tc = tagColor(tag);
    const bg = tagBgColor(tag);
    return h("span.panel-tag-pill", {
      key: tag,
      style: {
        display: "inline-flex",
        alignItems: "center",
        gap: "3px",
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        color: tc,
        backgroundColor: bg,
        borderRadius: "3px",
        padding: "2px 8px",
        lineHeight: "1.6",
      },
    }, [
      `:${tag}:`,
      h("button.tag-remove", {
        style: {
          background: "none",
          border: "none",
          color: tc,
          cursor: "pointer",
          fontSize: fontSize.xs,
          padding: "0 0 0 2px",
          lineHeight: "1",
          opacity: "0.6",
        },
        on: {
          click: (e: Event) => {
            e.stopPropagation();
            const newTags = tags.filter((t) => t !== tag);
            callbacks.onChangeTags(file, taskIndex, newTags);
          },
        },
      }, "\u00d7"),
    ]);
  });

  // Add tag button/input
  const addInput = h("div.tag-add-container", {
    style: {
      position: "relative",
      display: "inline-block",
    },
  }, [
    h("input.tag-add-input", {
      attrs: {
        type: "text",
        placeholder: "[+]",
      },
      style: {
        padding: "2px 8px",
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        color: colors.greyLight,
        backgroundColor: "transparent",
        border: `1px dashed ${colors.grey}`,
        borderRadius: "3px",
        outline: "none",
        width: "48px",
        transition: `width ${transitions.fast}`,
      },
      on: {
        focus: (e: Event) => {
          const input = e.target as HTMLInputElement;
          input.style.width = "100px";
          input.placeholder = "add tag";
        },
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

          const suggestions = allTags
            .filter((t) => !tags.includes(t) && t.toLowerCase().includes(query))
            .slice(0, 6);

          dropdown.innerHTML = "";
          if (suggestions.length > 0) {
            dropdown.style.display = "block";
            suggestions.forEach((s) => {
              const tc = tagColor(s);
              const opt = document.createElement("div");
              opt.textContent = `:${s}:`;
              opt.style.cssText = `
                padding: 4px 8px;
                cursor: pointer;
                font-family: '${fonts.mono}', monospace;
                font-size: ${fontSize.xs};
                color: ${tc};
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
          setTimeout(() => {
            const input = e.target as HTMLInputElement;
            input.style.width = "48px";
            input.placeholder = "[+]";
            input.value = "";
            const container = input.parentElement;
            const dropdown = container?.querySelector(".tag-autocomplete") as HTMLElement | null;
            if (dropdown) dropdown.style.display = "none";
          }, 150);
        },
      },
    }),
    h("div.tag-autocomplete", {
      style: {
        display: "none",
        position: "absolute",
        top: "100%",
        left: "0",
        minWidth: "120px",
        backgroundColor: colors.concrete,
        border: `1px solid ${colors.concreteDark}`,
        borderRadius: "3px",
        zIndex: "10",
        maxHeight: "150px",
        overflowY: "auto",
        boxShadow: "0 4px 12px rgba(0,0,0,0.4)",
      },
    }),
  ]);

  return h("div.panel-tags-line", {
    style: {
      display: "flex",
      flexWrap: "wrap",
      gap: "6px",
      alignItems: "center",
    },
  }, [...tagPills, addInput]);
}

// --- Date line ---

function renderDateLine(
  label: string,
  value: OrgTime | null,
  file: FilePath,
  taskIndex: TaskIndex,
  onChange: (file: FilePath, taskIndex: TaskIndex, date: OrgTime | null) => void,
): VNode {
  const displayValue = value?.date ?? "not set";
  const hasValue = value !== null;

  return h("div.panel-date-line", {
    style: {
      display: "flex",
      alignItems: "center",
      gap: "6px",
      fontFamily: `'${fonts.mono}', monospace`,
      fontSize: fontSize.sm,
      lineHeight: "1.6",
    },
  }, [
    h("span", {
      style: {
        color: colors.greyLight,
        fontWeight: fontWeight.bold,
        textTransform: "uppercase",
      },
    }, `${label}:`),
    h("span.date-value-wrapper", {
      style: {
        position: "relative",
        display: "inline-block",
      },
    }, [
      h("span.date-display", {
        style: {
          color: hasValue ? colors.white : colors.grey,
          cursor: "pointer",
          borderBottom: `1px dashed ${hasValue ? "transparent" : colors.grey}`,
          transition: `border-color ${transitions.fast}`,
        },
        hook: {
          insert: (vnode) => {
            const el = vnode.elm as HTMLElement;
            const wrapper = el.parentElement;
            if (!wrapper) return;

            el.addEventListener("click", (e) => {
              e.stopPropagation();
              if (wrapper.querySelector("input")) return;

              el.style.display = "none";

              const input = document.createElement("input");
              input.type = "date";
              input.value = value?.date ?? "";
              input.style.cssText = `
                font-family: '${fonts.mono}', monospace;
                font-size: ${fontSize.sm};
                color: ${colors.white};
                background-color: ${colors.concrete};
                border: 1px solid ${colors.concreteDark};
                border-radius: 3px;
                padding: 2px 6px;
                outline: none;
                color-scheme: dark;
              `;

              input.addEventListener("change", () => {
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
                input.remove();
                el.style.display = "";
              });

              input.addEventListener("keydown", (ev) => {
                if (ev.key === "Escape") {
                  input.remove();
                  el.style.display = "";
                }
              });

              input.addEventListener("blur", () => {
                setTimeout(() => {
                  if (wrapper.contains(input)) {
                    input.remove();
                    el.style.display = "";
                  }
                }, 100);
              });

              wrapper.appendChild(input);
              input.focus();
              input.showPicker?.();
            });
          },
        },
      }, displayValue),
    ]),
    // Clear button (only if value set) — sized to match the text
    ...(hasValue
      ? [h("button.date-clear", {
          style: {
            background: "none",
            border: "none",
            color: colors.grey,
            cursor: "pointer",
            fontSize: fontSize.sm,
            padding: "2px 4px",
            lineHeight: "1",
            opacity: "0.6",
            transition: `opacity ${transitions.fast}`,
          },
          on: {
            click: (e: Event) => {
              e.stopPropagation();
              onChange(file, taskIndex, null);
            },
            mouseenter: (e: Event) => {
              (e.currentTarget as HTMLElement).style.opacity = "1";
            },
            mouseleave: (e: Event) => {
              (e.currentTarget as HTMLElement).style.opacity = "0.6";
            },
          },
        }, "\u00d7")]
      : []),
  ]);
}

// --- Description block ---

function renderDescriptionBlock(
  descriptionText: string,
  file: FilePath,
  taskIndex: TaskIndex,
  callbacks: DetailPanelCallbacks,
): VNode {
  const hasContent = descriptionText.trim().length > 0;

  return h("div.panel-description", {
    style: {
      backgroundColor: colors.void,
      borderRadius: "6px",
      padding: "12px 14px",
      fontFamily: `'${fonts.body}', sans-serif`,
      fontSize: fontSize.md,
      color: hasContent ? colors.greyLight : colors.grey,
      whiteSpace: "pre-wrap",
      lineHeight: "1.6",
      wordBreak: "break-word",
      cursor: "pointer",
      minHeight: "48px",
      transition: `border-color ${transitions.fast}`,
      border: `1px solid transparent`,
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
          width: 100%;
          min-height: 80px;
          padding: 0;
          font-family: '${fonts.body}', sans-serif;
          font-size: ${fontSize.md};
          color: ${colors.white};
          background: transparent;
          border: none;
          resize: vertical;
          outline: none;
          line-height: 1.6;
        `;
        el.style.borderColor = colors.cyanBright;
        el.appendChild(textarea);
        textarea.focus();
        textarea.addEventListener("keydown", (ev) => {
          if (ev.key === "Escape") {
            textarea.remove();
            el.style.borderColor = "transparent";
            if (display) display.style.display = "";
          }
        });
        textarea.addEventListener("blur", () => {
          const newDesc = textarea.value;
          if (newDesc !== descriptionText) {
            callbacks.onChangeDescription(file, taskIndex, newDesc);
          }
          textarea.remove();
          el.style.borderColor = "transparent";
          if (display) display.style.display = "";
        });
      },
      mouseenter: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        if (!el.querySelector("textarea")) {
          el.style.borderColor = colors.concreteDark;
        }
      },
      mouseleave: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        if (!el.querySelector("textarea")) {
          el.style.borderColor = "transparent";
        }
      },
    },
  }, [
    h("span.desc-display", {}, hasContent ? descriptionText : "Click to add description..."),
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

  // Description text
  const descriptionText = task.description
    .map((n) => (n.type === "plain" ? n.text : n.url))
    .join("");

  // Created date
  const createdDate = task.createdProp?.date ?? null;

  // File location
  const fileStr = String(pointer.file);
  const fileName = fileStr.split("/").pop() ?? fileStr;

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
        el.style.transform = "translateX(100%)";
        requestAnimationFrame(() => {
          requestAnimationFrame(() => {
            el.style.transform = "translateX(0)";
          });
        });
      },
    },
  }, [
    // Close button row
    h("div.panel-close-row", {
      style: {
        display: "flex",
        justifyContent: "flex-end",
        padding: `16px ${PANEL_PADDING} 0`,
      },
    }, [
      h("button.panel-close-btn", {
        style: {
          background: "none",
          border: "none",
          color: colors.greyLight,
          fontSize: fontSize.lg,
          cursor: "pointer",
          padding: "4px",
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

    // Main panel body with all content
    h("div.panel-body", {
      style: {
        padding: `16px ${PANEL_PADDING} ${PANEL_PADDING}`,
        display: "flex",
        flexDirection: "column",
        gap: "0",
        flex: "1",
      },
    }, [
      // Header line: KEYWORD [#A] Title — all inline
      h("div.panel-header-line", {
        style: {
          display: "flex",
          alignItems: "baseline",
          gap: "8px",
          flexWrap: "wrap",
          lineHeight: "1.4",
        },
      }, [
        renderKeywordBadge(task.todoKeyword, file, taskIndex, callbacks),
        renderInlinePriority(task.priority, file, taskIndex, callbacks),
        renderEditableTitle(twp, callbacks),
      ]),

      // Tags line (8px below header)
      ...(task.tags.length > 0 || true
        ? [h("div.panel-tags-section", {
            style: {
              marginTop: "8px",
            },
          }, [renderTagsLine(task.tags, allTags, file, taskIndex, callbacks)])]
        : []),

      // Dates (16px below tags)
      h("div.panel-dates-section", {
        style: {
          marginTop: "16px",
          display: "flex",
          flexDirection: "column",
          gap: "2px",
        },
      }, [
        renderDateLine("SCHEDULED", task.scheduled, file, taskIndex, callbacks.onChangeScheduled),
        renderDateLine("DEADLINE", task.deadline, file, taskIndex, callbacks.onChangeDeadline),
      ]),

      // Description (16px below dates)
      h("div.panel-description-section", {
        style: {
          marginTop: "16px",
        },
      }, [
        renderDescriptionBlock(descriptionText, file, taskIndex, callbacks),
      ]),

      // Spacer to push meta to bottom
      h("div.panel-spacer", {
        style: { flex: "1" },
      }),

      // Meta line (16px above bottom, muted)
      h("div.panel-meta-line", {
        style: {
          marginTop: "16px",
          fontFamily: `'${fonts.mono}', monospace`,
          fontSize: fontSize.xs,
          color: colors.grey,
          lineHeight: "1.4",
        },
      }, [
        ...(createdDate
          ? [`Created: ${createdDate}  \u00b7  `]
          : []),
        `${fileName} : ${String(pointer.taskIndex)}`,
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
    panelContent,
  ]);
}
