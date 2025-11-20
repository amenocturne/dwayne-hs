
/**
 * Domain Types
 *
 * Core domain types matching the Haskell backend model.
 * All types are immutable (readonly) following functional programming principles.
 *
 * Phase 4: Enhanced with branded types for domain safety.
 */

import type { TaskIndex, FilePath, UrlString } from "./branded.js";

export type TimeUnit = "Hour" | "Day" | "Week" | "Month" | "Year";
export type RepeatType = "NextDate" | "NextFutureDate" | "PlusCompletionDate";
export type DelayType = "AllOccurrences" | "FirstOccurrence";

export interface RepeatInterval {
  readonly repeatType: RepeatType;
  readonly repeatValue: number;
  readonly repeatTimeUnit: TimeUnit;
}

export interface DelayInterval {
  readonly delayType: DelayType;
  readonly delayValue: number;
  readonly delayTimeUnit: TimeUnit;
}

export interface OrgTime {
  readonly date: string;
  readonly time: string | null;
  readonly repeater: RepeatInterval | null;
  readonly delay: DelayInterval | null;
}

export type TextNode =
  | { readonly type: "plain"; readonly text: string }
  | {
      readonly type: "link";
      readonly url: UrlString;
      readonly title: string | null;
    };

export type RichText = ReadonlyArray<TextNode>;

export interface Task {
  readonly level: number;
  readonly todoKeyword: string;
  readonly priority: number | null;
  readonly title: RichText;
  readonly tags: ReadonlyArray<string>;
  readonly scheduled: OrgTime | null;
  readonly deadline: OrgTime | null;
  readonly createdProp: OrgTime | null;
  readonly closed: OrgTime | null;
  readonly properties: ReadonlyArray<readonly [string, string]>;
  readonly description: RichText;
}

/**
 * TaskPointer uniquely identifies a task in the system.
 * Uses branded types to prevent mixing file paths and indices with arbitrary strings/numbers.
 */
export interface TaskPointer {
  readonly file: FilePath;
  readonly taskIndex: TaskIndex;
}

export interface TaskWithPointer {
  readonly task: Task;
  readonly pointer: TaskPointer;
}

export interface TaskNode {
  readonly task: Task;
  readonly pointer: TaskPointer;
  readonly children: ReadonlyArray<TaskNode>;
}

export type ViewName =
  | "all"
  | "inbox"
  | "relevant"
  | "someday"
  | "notes"
  | "list"
  | "waiting"
  | "project"
  | "todo"
  | "done"
  | "trash";
