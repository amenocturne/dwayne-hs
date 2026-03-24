/**
 * TaskDetail — Inline expandable detail content rendered below a task row.
 *
 * Extracted from DetailPanel.ts so both the old slide-in panel and the new
 * inline expansion can share the same editing widgets (keyword dropdown,
 * priority cycling, tags, dates, description).
 */

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

// --- Public callback interface ---

export interface TaskDetailCallbacks {
  readonly onChangeKeyword: (file: FilePath, taskIndex: TaskIndex, keyword: string) => void;
  readonly onChangePriority: (file: FilePath, taskIndex: TaskIndex, priority: number | null) => void;
  readonly onChangeTitle: (file: FilePath, taskIndex: TaskIndex, title: string) => void;
  readonly onChangeDescription: (file: FilePath, taskIndex: TaskIndex, description: string) => void;
  readonly onChangeTags: (file: FilePath, taskIndex: TaskIndex, tags: ReadonlyArray<string>) => void;
  readonly onChangeScheduled: (file: FilePath, taskIndex: TaskIndex, scheduled: OrgTime | null) => void;
  readonly onChangeDeadline: (file: FilePath, taskIndex: TaskIndex, deadline: OrgTime | null) => void;
}

// --- Constants ---

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
  return target.toLowerCase().includes(query.toLowerCase());
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

// --- Keyword dropdown ---

function renderKeywordBadge(
  currentKeyword: string,
  file: FilePath,
  taskIndex: TaskIndex,
  callbacks: TaskDetailCallbacks,
): VNode {
  const keywordColor = getTodoKeywordColor(currentKeyword);

  return h("div.detail-keyword-badge-wrapper", {
    style: {
      position: "relative",
      display: "inline-block",
    },
    hook: {
      insert: (vnode) => {
        const wrapper = vnode.elm as HTMLElement;
        const badge = wrapper.querySelector(".detail-keyword-badge") as HTMLElement;
        if (!badge) return;

        badge.addEventListener("click", (e) => {
          e.stopPropagation();
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
          list.style.cssText = `max-height: 240px; overflow-y: auto;`;

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
                opt.addEventListener("mouseenter", () => { opt.style.backgroundColor = colors.rowHover; });
                opt.addEventListener("mouseleave", () => { opt.style.backgroundColor = "transparent"; });
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
            if (ev.key === "Escape") dropdown.remove();
          });

          renderOptions("");
          dropdown.appendChild(input);
          dropdown.appendChild(list);
          wrapper.appendChild(dropdown);
          input.focus();

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
    h("span.detail-keyword-badge", {
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

// --- Priority (click-to-cycle) ---

function renderInlinePriority(
  currentPriority: number | null,
  file: FilePath,
  taskIndex: TaskIndex,
  callbacks: TaskDetailCallbacks,
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
  const isSet = currentPriority !== null;

  return h("span.detail-priority-inline", {
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
  callbacks: TaskDetailCallbacks,
): VNode {
  const titleText = extractTitleText(task.task.title);
  const { file, taskIndex } = task.pointer;

  return h("span.detail-title-wrapper", {
    style: {
      cursor: "text",
      flex: "1",
      minWidth: "0",
    },
    hook: {
      insert: (vnode) => {
        const el = vnode.elm as HTMLElement;
        el.addEventListener("click", (ev) => {
          ev.stopPropagation();
          const existing = el.querySelector(".detail-title-input") as HTMLInputElement | null;
          if (existing) return;

          const display = el.querySelector(".detail-title-display") as HTMLElement;
          if (display) display.style.display = "none";

          const input = document.createElement("input");
          input.className = "detail-title-input";
          input.type = "text";
          input.value = titleText;
          input.style.cssText = `
            width: 100%;
            font-family: '${fonts.body}', sans-serif;
            font-size: ${fontSize.base};
            font-weight: ${fontWeight.normal};
            color: ${colors.white};
            background-color: ${colors.concrete};
            border: 1px solid ${colors.concreteDark};
            border-radius: 3px;
            padding: 2px 6px;
            outline: none;
            box-sizing: border-box;
          `;

          input.addEventListener("keydown", (kev) => {
            if (kev.key === "Enter") {
              const newTitle = input.value.trim();
              if (newTitle && newTitle !== titleText) {
                callbacks.onChangeTitle(file, taskIndex, newTitle);
              }
              input.remove();
              if (display) display.style.display = "";
            }
            if (kev.key === "Escape") {
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
    h("span.detail-title-display", {
      style: {
        fontFamily: `'${fonts.body}', sans-serif`,
        fontSize: fontSize.base,
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
  callbacks: TaskDetailCallbacks,
): VNode {
  const tagPills = tags.map((tag) => {
    const tc = tagColor(tag);
    const bg = tagBgColor(tag);
    return h("span.detail-tag-pill", {
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

  const addInput = h("div.tag-add-container", {
    style: {
      position: "relative",
      display: "inline-block",
    },
  }, [
    h("input.tag-add-input", {
      attrs: { type: "text", placeholder: "[+]" },
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

          if (query.length === 0) { dropdown.style.display = "none"; return; }

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
              opt.addEventListener("mouseenter", () => { opt.style.backgroundColor = colors.rowHover; });
              opt.addEventListener("mouseleave", () => { opt.style.backgroundColor = "transparent"; });
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
            const match = allTags.filter((t) => !tags.includes(t)).find((t) => t.toLowerCase().includes(value.toLowerCase()));
            const tagToAdd = match ?? value;
            if (!tags.includes(tagToAdd)) {
              callbacks.onChangeTags(file, taskIndex, [...tags, tagToAdd]);
            }
            input.value = "";
            const container = input.parentElement;
            const dropdown = container?.querySelector(".tag-autocomplete") as HTMLElement | null;
            if (dropdown) dropdown.style.display = "none";
          }
          if (e.key === "Escape") (e.target as HTMLElement).blur();
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

  return h("div.detail-tags-line", {
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

  return h("div.detail-date-line", {
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
      style: { position: "relative", display: "inline-block" },
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
                  onChange(file, taskIndex, { date: input.value, time: null, repeater: null, delay: null });
                } else {
                  onChange(file, taskIndex, null);
                }
                input.remove();
                el.style.display = "";
              });
              input.addEventListener("keydown", (ev) => {
                if (ev.key === "Escape") { input.remove(); el.style.display = ""; }
              });
              input.addEventListener("blur", () => {
                setTimeout(() => {
                  if (wrapper.contains(input)) { input.remove(); el.style.display = ""; }
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
            click: (e: Event) => { e.stopPropagation(); onChange(file, taskIndex, null); },
            mouseenter: (e: Event) => { (e.currentTarget as HTMLElement).style.opacity = "1"; },
            mouseleave: (e: Event) => { (e.currentTarget as HTMLElement).style.opacity = "0.6"; },
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
  callbacks: TaskDetailCallbacks,
): VNode {
  const hasContent = descriptionText.trim().length > 0;

  return h("div.detail-description", {
    style: {
      backgroundColor: colors.void,
      borderRadius: "6px",
      padding: "10px 12px",
      fontFamily: `'${fonts.body}', sans-serif`,
      fontSize: fontSize.sm,
      color: hasContent ? colors.greyLight : colors.grey,
      whiteSpace: "pre-wrap",
      lineHeight: "1.6",
      wordBreak: "break-word",
      cursor: "pointer",
      minHeight: "36px",
      transition: `border-color ${transitions.fast}`,
      border: `1px solid transparent`,
    },
    on: {
      click: (e: Event) => {
        e.stopPropagation();
        const el = (e.target as HTMLElement).closest(".detail-description") as HTMLElement;
        if (!el || el.querySelector("textarea")) return;
        const display = el.querySelector(".desc-display") as HTMLElement;
        if (display) display.style.display = "none";
        const textarea = document.createElement("textarea");
        textarea.value = descriptionText;
        textarea.style.cssText = `
          width: 100%;
          min-height: 60px;
          padding: 0;
          font-family: '${fonts.body}', sans-serif;
          font-size: ${fontSize.sm};
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
        if (!el.querySelector("textarea")) el.style.borderColor = colors.concreteDark;
      },
      mouseleave: (e: Event) => {
        const el = e.currentTarget as HTMLElement;
        if (!el.querySelector("textarea")) el.style.borderColor = "transparent";
      },
    },
  }, [
    h("span.desc-display", {}, hasContent ? descriptionText : "Click to add description..."),
  ]);
}

// --- Main render: inline expanded detail ---

export function renderTaskDetail(
  twp: TaskWithPointer,
  state: AppState,
  callbacks: TaskDetailCallbacks,
): VNode {
  const { task, pointer } = twp;
  const { file, taskIndex } = pointer;
  const allTags = collectAllTags(state);

  const descriptionText = task.description
    .map((n) => (n.type === "plain" ? n.text : n.url))
    .join("");

  const createdDate = task.createdProp?.date ?? null;
  const fileStr = String(pointer.file);
  const fileName = fileStr.split("/").pop() ?? fileStr;

  return h("div.task-detail-expanded", {
    key: `detail-${file}-${taskIndex}`,
    style: {
      padding: `${spacing.sm} ${spacing.lg} ${spacing.md} 40px`,
      display: "flex",
      flexDirection: "column",
      gap: "0",
      backgroundColor: colors.asphalt,
      borderBottom: `1px solid rgba(255, 255, 255, 0.03)`,
      overflow: "hidden",
      maxHeight: "500px",
      transition: `max-height 200ms ease-out`,
    },
    hook: {
      insert: (vnode) => {
        const el = vnode.elm as HTMLElement;
        // Animate open: start from 0
        el.style.maxHeight = "0px";
        requestAnimationFrame(() => {
          requestAnimationFrame(() => {
            el.style.maxHeight = "500px";
          });
        });
      },
    },
  }, [
    // Header line: KEYWORD [#A] Full Title
    h("div.detail-header-line", {
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

    // Tags
    h("div.detail-tags-section", {
      style: { marginTop: "8px" },
    }, [renderTagsLine(task.tags, allTags, file, taskIndex, callbacks)]),

    // Dates
    h("div.detail-dates-section", {
      style: {
        marginTop: spacing.md,
        display: "flex",
        gap: spacing.lg,
        flexWrap: "wrap",
      },
    }, [
      renderDateLine("SCHEDULED", task.scheduled, file, taskIndex, callbacks.onChangeScheduled),
      renderDateLine("DEADLINE", task.deadline, file, taskIndex, callbacks.onChangeDeadline),
    ]),

    // Description
    h("div.detail-description-section", {
      style: { marginTop: spacing.md },
    }, [renderDescriptionBlock(descriptionText, file, taskIndex, callbacks)]),

    // Meta
    h("div.detail-meta-line", {
      style: {
        marginTop: spacing.sm,
        fontFamily: `'${fonts.mono}', monospace`,
        fontSize: fontSize.xs,
        color: colors.grey,
        lineHeight: "1.4",
      },
    }, [
      ...(createdDate ? [`Created: ${createdDate}  \u00b7  `] : []),
      `${fileName} : ${String(pointer.taskIndex)}`,
    ]),
  ]);
}
