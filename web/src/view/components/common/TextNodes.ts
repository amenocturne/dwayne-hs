import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { RichText } from "../../../types/domain.js";

export function renderTextNodes(nodes: RichText): Array<VNode | string> {
  return nodes.map((node) => {
    if (node.type === "plain") {
      return node.text;
    } else {
      const displayText = node.title || node.url;
      return h(
        "a",
        {
          attrs: {
            href: node.url,
            target: "_blank",
            rel: "noopener noreferrer",
          },
          style: {
            color: "var(--link-color)",
            textDecoration: "underline",
          },
          on: {
            click: (e: Event) => {
              e.stopPropagation();
            },
          },
        },
        displayText,
      );
    }
  });
}
