/**
 * Type Utilities
 * 
 * Generic type-level utilities for exhaustiveness checking and pattern matching.
 * Inspired by functional languages (Haskell, Scala) for compile-time safety.
 */

/**
 * Ensures exhaustive checking of discriminated unions.
 * 
 * When all cases of a union are handled, `value` has type `never`.
 * If a case is missed, TypeScript will error at compile time.
 * 
 * Usage:
 *   switch (action.type) {
 *     case 'Foo': return handleFoo(action);
 *     case 'Bar': return handleBar(action);
 *     default: return assertNever(action);
 *   }
 * 
 * If you add a new action type but forget to handle it, TypeScript errors.
 */
export function assertNever(value: never): never {
  throw new Error(`Unexpected value: ${JSON.stringify(value)}`);
}

/**
 * Type-safe pattern matching for discriminated unions.
 * 
 * An alternative to switch statements that forces you to handle all cases.
 * Returns the result of calling the appropriate handler.
 * 
 * Usage:
 *   const result = match(action, {
 *     ViewChanged: (a) => handleViewChange(a.view),
 *     TasksLoaded: (a) => handleTasksLoaded(a.tasks),
 *     // ... all other cases
 *   });
 * 
 * TypeScript will error if:
 * - You're missing a handler for a union member
 * - You have a handler for a non-existent type
 * - The handler parameter types don't match
 * 
 * @param value The discriminated union value
 * @param handlers Object mapping each type to its handler function
 * @returns The result of calling the matched handler
 */
export function match<T extends { readonly type: string }, R>(
  value: T,
  handlers: { [K in T['type']]: (val: Extract<T, { readonly type: K }>) => R }
): R {
  type TypeKey = T['type'];
  const handler = handlers[value.type as TypeKey];
  
  if (!handler) {
    throw new Error(`No handler for type: ${value.type}`);
  }
  
  // Safe cast: we know handler exists and matches the discriminated type
  return handler(value as Extract<T, { readonly type: TypeKey }>);
}

/**
 * Extract the variant type from a discriminated union.
 * 
 * Example:
 *   type Action = { type: 'A'; x: number } | { type: 'B'; y: string };
 *   type ActionA = ExtractVariant<Action, 'A'>;  // { type: 'A'; x: number }
 */
export type ExtractVariant<T extends { readonly type: string }, K extends T['type']> = 
  Extract<T, { readonly type: K }>;

/**
 * NonEmptyArray - an array guaranteed to have at least one element.
 * 
 * Use this when your domain logic requires non-empty collections.
 * TypeScript will prevent you from creating empty instances.
 */
export type NonEmptyArray<T> = readonly [T, ...ReadonlyArray<T>];

/**
 * Smart constructor for NonEmptyArray.
 * Returns null if the array is empty, enforcing the non-empty invariant.
 */
export function nonEmptyArray<T>(arr: ReadonlyArray<T>): NonEmptyArray<T> | null {
  if (arr.length === 0) return null;
  return arr as NonEmptyArray<T>;
}

/**
 * Unsafe constructor for NonEmptyArray.
 * Throws if the array is empty. Use when you're certain the array has elements.
 */
export function unsafeNonEmptyArray<T>(arr: ReadonlyArray<T>): NonEmptyArray<T> {
  if (arr.length === 0) {
    throw new Error('Expected non-empty array but got empty array');
  }
  return arr as NonEmptyArray<T>;
}
