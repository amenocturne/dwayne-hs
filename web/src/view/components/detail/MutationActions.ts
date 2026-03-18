import { h } from "snabbdom/build/h.js";
import type { VNode } from "snabbdom/build/vnode.js";
import type { TaskWithPointer } from "../../../types/domain.js";
import type { FilePath, TaskIndex } from "../../../types/branded.js";
import { renderSidebarSection } from "./SidebarSection.js";
import { getTodoKeywordColor, fontSize, fontWeight, fonts, spacing, colors } from "../../designSystem.js";

export interface MutationCallbacks {
  readonly onChangeKeyword: (file: FilePath, taskIndex: TaskIndex, keyword: string) => void;
  readonly onDelete: (file: FilePath, taskIndex: TaskIndex) => void;
}

const KEYWORD_OPTIONS = ["INBOX", "TODO", "WAITING", "SOMEDAY", "DONE", "TRASH"] as const;

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

function renderDeleteButton(onClick: () => void): VNode {
  return h('button', {
    style: {
      padding: `${spacing.sm} ${spacing.lg}`,
      fontSize: fontSize.sm,
      fontFamily: fonts.mono,
      fontWeight: fontWeight.bold,
      letterSpacing: '0.08em',
      textTransform: 'uppercase',
      color: colors.redBright,
      backgroundColor: 'transparent',
      border: `1px solid ${colors.redBright}`,
      borderRadius: '3px',
      cursor: 'pointer',
      opacity: '0.7',
      transition: 'all 0.2s',
      marginTop: spacing.sm,
    },
    on: {
      click: (e: Event) => {
        e.stopPropagation();
        onClick();
      },
    },
  }, 'DELETE TASK');
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

  const deleteButton = renderDeleteButton(
    () => callbacks.onDelete(pointer.file, pointer.taskIndex)
  );

  const content = h('div', {
    style: {
      display: 'flex',
      flexDirection: 'column',
    },
  }, [keywordButtons, deleteButton]);

  return renderSidebarSection('Actions', content)!;
}
