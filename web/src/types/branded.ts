/**
 * Branded Types
 * 
 * Phantom types that prevent mixing semantically different values.
 * Uses TypeScript's structural typing to create nominal-like types.
 * 
 * Example:
 *   const idx: TaskIndex = taskIndex(5);
 *   const path: FilePath = filePath("foo.org");
 *   // TypeScript prevents: const bad: TaskIndex = 5;  // Type error!
 */

declare const brand: unique symbol;

/**
 * Brand a base type with a unique label.
 * The brand symbol is compile-time only - zero runtime cost.
 */
type Brand<T, TBrand extends string> = T & { readonly [brand]: TBrand };

/**
 * Non-negative integer representing task position in a file.
 * Prevents accidentally using arbitrary numbers as indices.
 */
export type TaskIndex = Brand<number, 'TaskIndex'>;

/**
 * File path string that must be non-empty.
 * Ensures file paths are validated before use.
 */
export type FilePath = Brand<string, 'FilePath'>;

/**
 * Smart constructor for TaskIndex.
 * Validates the invariant: taskIndex >= 0
 */
export function taskIndex(n: number): TaskIndex {
  if (!Number.isInteger(n) || n < 0) {
    throw new Error(`TaskIndex must be a non-negative integer, got: ${n}`);
  }
  return n as TaskIndex;
}

/**
 * Unsafe constructor for TaskIndex when you've already validated.
 * Use sparingly - prefer `taskIndex()` which validates.
 */
export function unsafeTaskIndex(n: number): TaskIndex {
  return n as TaskIndex;
}

/**
 * Smart constructor for FilePath.
 * Validates the invariant: path is non-empty after trimming.
 */
export function filePath(s: string): FilePath {
  const trimmed = s.trim();
  if (trimmed.length === 0) {
    throw new Error('FilePath cannot be empty or whitespace-only');
  }
  return trimmed as FilePath;
}

/**
 * Unsafe constructor for FilePath when you've already validated.
 * Use sparingly - prefer `filePath()` which validates.
 */
export function unsafeFilePath(s: string): FilePath {
  return s as FilePath;
}

/**
 * Extract the underlying value from a branded type.
 * Useful when interfacing with external APIs that expect primitive types.
 */
export function unwrapTaskIndex(idx: TaskIndex): number {
  return idx as number;
}

export function unwrapFilePath(path: FilePath): string {
  return path as string;
}

/**
 * URL string that must be a valid URL format.
 */
export type UrlString = Brand<string, 'UrlString'>;

/**
 * Smart constructor for UrlString.
 * Tries to parse the URL to validate it.
 */
export function urlString(s: string): UrlString {
  try {
    // Use the URL constructor to validate the URL format.
    // A relative URL will throw an error if no base is provided, which is fine for our use case.
    new URL(s);
    return s as UrlString;
  } catch (_) {
    throw new Error(`Invalid URL string: ${s}`);
  }
}

/**
 * Unsafe constructor for UrlString when you've already validated.
 */
export function unsafeUrlString(s: string): UrlString {
  return s as UrlString;
}
