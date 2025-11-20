/**
 * Result Type
 * 
 * Functional error handling without exceptions.
 * Inspired by Rust's Result<T, E> and Haskell's Either.
 * 
 * Benefits:
 * - Forces explicit error handling at compile time
 * - Type-safe errors (no "unknown error" or implicit any)
 * - Composable with map/flatMap for functional pipelines
 * - No try/catch blocks needed for domain logic
 */

/**
 * Result type: either success (Ok) or failure (Err).
 * 
 * Generic over value type T and error type E (defaults to Error).
 */
export type Result<T, E = Error> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: E };

/**
 * Construct a successful Result.
 */
export function ok<T>(value: T): Result<T, never> {
  return { ok: true, value };
}

/**
 * Construct a failed Result.
 */
export function err<E>(error: E): Result<never, E> {
  return { ok: false, error };
}

/**
 * Check if Result is successful.
 * Type guard for narrowing.
 */
export function isOk<T, E>(result: Result<T, E>): result is { readonly ok: true; readonly value: T } {
  return result.ok === true;
}

/**
 * Check if Result is failure.
 * Type guard for narrowing.
 */
export function isErr<T, E>(result: Result<T, E>): result is { readonly ok: false; readonly error: E } {
  return result.ok === false;
}

/**
 * Transform the success value if Result is Ok.
 * If Result is Err, propagate the error unchanged.
 * 
 * Functor map operation.
 * 
 * Example:
 *   const r: Result<number, string> = ok(5);
 *   const r2 = map(r, x => x * 2);  // ok(10)
 */
export function map<T, U, E>(
  result: Result<T, E>,
  fn: (value: T) => U
): Result<U, E> {
  return result.ok ? ok(fn(result.value)) : result;
}

/**
 * Transform the success value with a function that returns a Result.
 * If Result is Err, propagate the error unchanged.
 * 
 * Monad flatMap/bind operation. Allows chaining fallible operations.
 * 
 * Example:
 *   const parseNum = (s: string): Result<number, string> =>
 *     isNaN(Number(s)) ? err("not a number") : ok(Number(s));
 * 
 *   const r: Result<string, string> = ok("42");
 *   const r2 = flatMap(r, parseNum);  // ok(42)
 */
export function flatMap<T, U, E>(
  result: Result<T, E>,
  fn: (value: T) => Result<U, E>
): Result<U, E> {
  return result.ok ? fn(result.value) : result;
}

/**
 * Transform the error value if Result is Err.
 * If Result is Ok, propagate the value unchanged.
 * 
 * Example:
 *   const r: Result<number, Error> = err(new Error("oops"));
 *   const r2 = mapErr(r, e => e.message);  // err("oops")
 */
export function mapErr<T, E, F>(
  result: Result<T, E>,
  fn: (error: E) => F
): Result<T, F> {
  return result.ok ? result : err(fn(result.error));
}

/**
 * Unwrap a Result, returning the value or a default.
 * 
 * Example:
 *   const r: Result<number, string> = err("failed");
 *   const value = unwrapOr(r, 0);  // 0
 */
export function unwrapOr<T, E>(result: Result<T, E>, defaultValue: T): T {
  return result.ok ? result.value : defaultValue;
}

/**
 * Unwrap a Result, returning the value or computing a default.
 * 
 * Example:
 *   const r: Result<number, string> = err("failed");
 *   const value = unwrapOrElse(r, e => e.length);  // 6
 */
export function unwrapOrElse<T, E>(
  result: Result<T, E>,
  fn: (error: E) => T
): T {
  return result.ok ? result.value : fn(result.error);
}

/**
 * Match on a Result with two handler functions.
 * 
 * Example:
 *   const r: Result<number, string> = ok(42);
 *   const msg = matchResult(r, {
 *     ok: n => `Success: ${n}`,
 *     err: e => `Error: ${e}`
 *   });  // "Success: 42"
 */
export function matchResult<T, E, R>(
  result: Result<T, E>,
  handlers: {
    readonly ok: (value: T) => R;
    readonly err: (error: E) => R;
  }
): R {
  return result.ok ? handlers.ok(result.value) : handlers.err(result.error);
}

/**
 * Combine multiple Results into a single Result of an array.
 * If any Result is Err, return the first error.
 * If all are Ok, return Ok with array of all values.
 * 
 * Example:
 *   const results = [ok(1), ok(2), ok(3)];
 *   const combined = all(results);  // ok([1, 2, 3])
 */
export function all<T, E>(
  results: ReadonlyArray<Result<T, E>>
): Result<ReadonlyArray<T>, E> {
  const values: T[] = [];
  
  for (const result of results) {
    if (!result.ok) {
      return result;
    }
    values.push(result.value);
  }
  
  return ok(values);
}

/**
 * Convert a Promise to a Result.
 * Catches any thrown errors and wraps them as Err.
 * 
 * Example:
 *   const r = await fromPromise(
 *     fetch('/api/data'),
 *     e => `Network error: ${e}`
 *   );
 */
export async function fromPromise<T, E>(
  promise: Promise<T>,
  mapError: (error: unknown) => E
): Promise<Result<T, E>> {
  try {
    const value = await promise;
    return ok(value);
  } catch (error) {
    return err(mapError(error));
  }
}
