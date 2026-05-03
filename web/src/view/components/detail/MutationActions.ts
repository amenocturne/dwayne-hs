import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer } from "../../../types/domain.js";
import type { FilePath, TaskIndex } from "../../../types/branded.js";
import { renderSidebarSection } from "./SidebarSection.js";
import { getTodoKeywordColor, fontSize, fontWeight, fonts, spacing } from "../../designSystem.js";

export interface MutationCallbacks {
  readonly onChangeKeyword: (file: FilePath, taskIndex: TaskIndex, keyword: string) => void;
}

const KEYWORD_OPTIONS = ["INBOX", "TODAY", "TODO", "SOON", "SOMEDAY", "WAITING", "PROJECT", "LIST", "DONE", "TRASH"] as const;

function renderKeywordButton(
  keyword: string,
  isActive: boolean,
  onClick: () => void
): VNode {
  const color = getTodoKeywordColor(keyword);

  return h('button', {
    style: {
      padding: `${spacing.xs} ${spacing.md}`,
      fontSize: fontSize.xs,
      fontFamily: fonts.mono,
      fontWeight: fontWeight.bold,
      letterSpacing: '0.08em',
      textTransform: 'uppercase',
      color: isActive ? '#000' : color,
      backgroundColor: isActive ? color : 'transparent',
      border: `1px solid ${color}`,
      borderRadius: '3px',
      cursor: isActive ? 'default' : 'pointer',
      opacity: isActive ? '1' : '0.7',
      transition: 'all 0.2s',
      whiteSpace: 'nowrap',
    },
    on: isActive ? {} : {
      click: (e: Event) => {
        e.stopPropagation();
        onClick();
      },
    },
  }, keyword);
}

export function renderMutationActions(
  taskWithPointer: TaskWithPointer,
  callbacks: MutationCallbacks
): VNode {
  const { task, pointer } = taskWithPointer;

  const keywordButtons = h('div', {
    style: {
      display: 'flex',
      flexWrap: 'wrap',
      gap: spacing.xs,
    },
  }, KEYWORD_OPTIONS.map((kw) =>
    renderKeywordButton(
      kw,
      task.todoKeyword === kw,
      () => callbacks.onChangeKeyword(pointer.file, pointer.taskIndex, kw)
    )
  ));

  const content = h('div', {
    style: {
      display: 'flex',
      flexDirection: 'column',
    },
  }, [keywordButtons]);

  return renderSidebarSection('Actions', content)!;
}
