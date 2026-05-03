import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { CommandBarMode } from "../../types/state.js";
import { colors, fonts, fontSize, spacing, transitions, borderRadius } from "../designSystem.js";

export interface CommandBarCallbacks {
  readonly onCapture: (title: string) => void;
  readonly onSearchChange: (query: string) => void;
  readonly onClearSearch: () => void;
  readonly onModeChange: (mode: CommandBarMode) => void;
}

export function renderCommandBar(
  mode: CommandBarMode,
  searchQuery: string,
  callbacks: CommandBarCallbacks,
): VNode {
  const isSearch = mode === 'search';
  const modeIcon = isSearch ? '\uD83D\uDD0D' : '+';
  const placeholder = isSearch ? 'Search...' : 'Capture...';

  return h(
    "div.command-bar",
    {
      style: {
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: `${spacing.sm} ${spacing.lg}`,
        zIndex: "150",
      },
    },
    [
      h("div.command-bar-inner", {
        style: {
          display: "flex",
          alignItems: "center",
          width: "100%",
          maxWidth: "560px",
          position: "relative",
        },
      }, [
        // Mode indicator button
        h("button.command-bar-mode", {
          style: {
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            width: "32px",
            height: "32px",
            border: "none",
            borderRadius: `${borderRadius.md} 0 0 ${borderRadius.md}`,
            backgroundColor: isSearch ? 'rgba(0, 229, 255, 0.12)' : 'rgba(255, 107, 53, 0.12)',
            color: isSearch ? colors.cyanBright : colors.orangeBright,
            cursor: "pointer",
            fontSize: isSearch ? "14px" : "18px",
            fontWeight: "700",
            transition: `background ${transitions.normal}, color ${transitions.normal}`,
            flexShrink: "0",
            padding: "0",
            lineHeight: "1",
          },
          on: {
            click: () => callbacks.onModeChange(isSearch ? 'capture' : 'search'),
          },
        }, modeIcon),

        // Input field
        h("input.command-bar-input", {
          attrs: {
            type: "text",
            placeholder,
            "aria-label": isSearch ? "Search tasks" : "Capture task",
          },
          props: {
            value: isSearch ? searchQuery : '',
          },
          style: {
            flex: "1",
            padding: `${spacing.sm} ${spacing.md}`,
            border: `1px solid ${colors.concreteDark}`,
            borderLeft: "none",
            borderRadius: `0 ${borderRadius.md} ${borderRadius.md} 0`,
            backgroundColor: "rgba(26, 26, 26, 0.6)",
            color: colors.white,
            fontSize: fontSize.md,
            fontFamily: `'${fonts.body}', sans-serif`,
            outline: "none",
            transition: `border-color ${transitions.normal}`,
          },
          on: {
            input: (e: Event) => {
              if (isSearch) {
                callbacks.onSearchChange((e.target as HTMLInputElement).value);
              }
            },
            keydown: (e: KeyboardEvent) => {
              if (e.key === "Enter") {
                const input = e.target as HTMLInputElement;
                const value = input.value.trim();
                if (!value) return;

                if (isSearch) {
                  // Search is handled on input, Enter is a no-op
                } else {
                  // Capture mode: create task and clear
                  callbacks.onCapture(value);
                  input.value = "";
                }
              }
              if (e.key === "Escape") {
                const input = e.target as HTMLInputElement;
                input.blur();
                if (isSearch) {
                  callbacks.onClearSearch();
                  callbacks.onModeChange('capture');
                }
              }
            },
            focus: (e: Event) => {
              (e.target as HTMLInputElement).style.borderColor = colors.cyanBright;
            },
            blur: (e: Event) => {
              (e.target as HTMLInputElement).style.borderColor = colors.concreteDark;
            },
          },
        }),

        // Clear button (search mode only, when there's a query)
        ...(isSearch && searchQuery.trim() !== "" ? [
          h("button.command-bar-clear", {
            style: {
              position: "absolute",
              right: "8px",
              top: "50%",
              transform: "translateY(-50%)",
              padding: "4px 8px",
              border: "none",
              background: "transparent",
              color: colors.greyLight,
              cursor: "pointer",
              fontSize: "1.25rem",
              lineHeight: "1",
              transition: `color ${transitions.normal}`,
            },
            on: {
              click: () => {
                callbacks.onClearSearch();
                callbacks.onModeChange('capture');
              },
            },
          }, '\u00D7'),
        ] : []),
      ]),
    ],
  );
}
