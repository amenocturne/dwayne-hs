# Dwayne Task Manager - Web Frontend

A pure functional frontend built with **Snabbdom** + **TypeScript** using the **Elm Architecture**.

## Architecture

This project demonstrates a "vanilla-flavored" approach to frontend development:
- **Zero framework dependencies** (just Snabbdom for Virtual DOM)
- **Pure functional state management** (Elm architecture)
- **Complete type safety** (TypeScript strict mode)
- **No build complexity** (single esbuild command)

## Tech Stack

- **TypeScript** - Type-safe JavaScript with strict mode enabled
- **Snabbdom** - Lightweight virtual DOM library (14kb, no framework dependencies)
- **esbuild** - Fast bundler (builds in ~3ms)

## Project Structure

```
web/
├── src/
│   ├── state/              # Elm architecture state management
│   │   ├── actions.ts      # 37 lines  - Action types (28 variants)
│   │   ├── effects.ts      # 256 lines - Effect types + runner (10 variants)
│   │   ├── reducer.ts      # 368 lines - Pure state transformer
│   │   └── store.ts        # 47 lines  - Store creation & dispatch loop
│   ├── view/               # Pure view functions (Snabbdom VNodes)
│   │   ├── app.ts          # Root view composition
│   │   └── components/     # Reusable view components
│   ├── api/                # API client functions
│   │   └── client.ts       # HTTP requests to backend
│   ├── types/              # TypeScript type definitions
│   │   ├── state.ts        # AppState structure
│   │   ├── domain.ts       # Domain types (Task, etc.)
│   │   └── api.ts          # API request/response types
│   └── main.ts             # 263 lines - Bootstrap & initialization
├── dist/                   # Build output
│   └── bundle.js           # 85.7kb bundled JavaScript
├── index.html              # Single page app entry
├── style.css               # Global styles
├── package.json            # Dependencies (Snabbdom + TypeScript only)
└── tsconfig.json           # TypeScript strict configuration
```

## Elm Architecture Flow

```
User Event → dispatch(action) → Pure Reducer → [new state, effect]
                                     ↓              ↓
                                  render()    runEffect() → dispatch(result)
```

**Key Principles:**
1. **Single source of truth**: All state in one `AppState` structure
2. **State is read-only**: Updates return new state objects (immutable)
3. **Actions describe facts**: `{ type: 'TasksLoaded', tasks: [...] }`
4. **Reducer is pure**: No side effects, no I/O, no mutations
5. **Effects are data**: Side effects described as values, executed separately

## Features

- ✅ View switching (all, inbox, relevant, project, etc.)
- ✅ Global search with debouncing
- ✅ Project-scoped search
- ✅ Infinite scroll (load more)
- ✅ Task selection sidebar with details
- ✅ Project tree loading
- ✅ Parent project detection
- ✅ WebSocket live updates
- ✅ Error handling with toast notifications
- ✅ Project view mode
- ✅ Dark/light theme support

## Getting Started

### Install Dependencies
```bash
npm install
```

### Build for Production
```bash
npm run build
# Output: dist/bundle.js (85.7kb)
```

### Type Check
```bash
npx tsc --noEmit
```

### Run Development Server
```bash
# Serve with any static server
python3 -m http.server 3000
# Or use browser-sync for live reload
npm run dev
```

Open http://localhost:3000

## Development Guide

### Adding a New Feature

#### 1. Define Action
```typescript
// src/state/actions.ts
export type Action =
  | ... existing actions ...
  | { type: 'FeatureRequested'; data: string }
  | { type: 'FeatureCompleted'; result: number };
```

#### 2. Define Effect
```typescript
// src/state/effects.ts
export type Effect =
  | ... existing effects ...
  | { type: 'ExecuteFeature'; data: string };
```

#### 3. Handle in Reducer
```typescript
// src/state/reducer.ts
case 'FeatureRequested':
  return [
    { ...state, loading: true },
    { type: 'ExecuteFeature', data: action.data }
  ];

case 'FeatureCompleted':
  return [
    { ...state, result: action.result, loading: false },
    { type: 'None' }
  ];
```

#### 4. Implement Effect
```typescript
// src/state/effects.ts - runEffect()
case 'ExecuteFeature':
  const result = await doSomething(effect.data);
  dispatch({ type: 'FeatureCompleted', result });
  break;
```

#### 5. Wire UI
```typescript
// src/main.ts or view callback
onFeatureClick: () => {
  dispatch({ type: 'FeatureRequested', data: 'example' });
}
```

TypeScript ensures exhaustiveness - if you miss a case, it won't compile!

## Architecture Benefits

### Type Safety
- **Exhaustiveness checking**: Compiler ensures all action cases handled
- **Discriminated unions**: Type narrows automatically in switch statements
- **Strict mode**: `noImplicitAny`, `strictNullChecks`, etc.

### Testability
```typescript
// Test reducer without any mocks or setup
const [newState, effect] = update(initialState, action);
expect(newState.currentView).toBe('inbox');
expect(effect.type).toBe('FetchTasks');
```

### Predictability
- State only changes in one place (reducer)
- All state transitions visible via actions
- No hidden mutations or side effects

### Performance
- **Zero runtime overhead**: Types erase at compile time
- **Immutable updates**: Structural sharing via spread operators
- **Virtual DOM**: Snabbdom's efficient diffing algorithm
- **Small bundle**: 85.7kb for complete application
- **Fast builds**: 3ms with esbuild

## Data Model

TypeScript types mirror the Haskell `Task` type from `Model.OrgMode`:

```typescript
interface Task {
  readonly level: number;
  readonly todoKeyword: string;        // TODO, INBOX, WAITING, DONE, PROJECT, etc.
  readonly priority: number | null;     // 1 (high), 2 (medium), 3 (low), or null
  readonly title: RichText;             // Array of plain text or link nodes
  readonly tags: ReadonlyArray<string>;
  readonly scheduled: OrgTime | null;
  readonly deadline: OrgTime | null;
  readonly createdProp: OrgTime | null;
  readonly closed: OrgTime | null;
  readonly properties: ReadonlyArray<readonly [string, string]>;
  readonly description: RichText;
}
```

## Documentation

- **[ELM_ARCHITECTURE.md](./ELM_ARCHITECTURE.md)** - Comprehensive architecture guide with examples
- **[ARCHITECTURE_FLOW.md](./ARCHITECTURE_FLOW.md)** - Visual flow diagrams and patterns
- **[PHASE2_SUMMARY.md](./PHASE2_SUMMARY.md)** - Refactoring summary and comparison

## Dependencies

```json
{
  "dependencies": {
    "snabbdom": "^3.6.4"  // Virtual DOM library (14kb)
  },
  "devDependencies": {
    "esbuild": "^0.24.2",     // Bundler
    "typescript": "^5.7.2"    // Type checker
  }
}
```

**Total production dependencies**: 1 (Snabbdom)

## Philosophy

This project demonstrates that you don't need React, Vue, or Angular to build a modern, type-safe, maintainable web application. With:

- **Snabbdom** for virtual DOM (14kb)
- **TypeScript** for type safety (zero runtime cost)
- **Pure functions** for state management (Elm architecture)
- **Web APIs** for everything else (fetch, WebSocket, etc.)

You get:
- ✅ Complete type safety
- ✅ Predictable state management
- ✅ Easy testing (pure functions)
- ✅ Small bundle size
- ✅ Fast build times
- ✅ Zero framework lock-in

**"Vanilla-flavored" code beats framework complexity.**

## Code Statistics

| Module | Lines | Purpose |
|--------|-------|---------|
| `actions.ts` | 37 | Action type definitions |
| `effects.ts` | 256 | Effect types + runner |
| `reducer.ts` | 368 | Pure state transformations |
| `store.ts` | 47 | Store creation & dispatch |
| `main.ts` | 263 | Bootstrap & initialization |
| **Total State** | **708** | **Complete state management** |

Compare to Redux + Redux-Saga + Redux-Thunk: Often 1000+ lines of boilerplate!

## Build Results

```bash
$ npm run build
  dist/bundle.js  85.7kb
⚡ Done in 3ms

$ npx tsc --noEmit
✓ Type check passed!
```

## License

MIT

## Credits

- **Architecture**: Inspired by Elm Architecture (TEA)
- **Virtual DOM**: Snabbdom by Simon Friis Vindum
- **Type System**: TypeScript by Microsoft
- **Philosophy**: Functional programming principles from Haskell/Scala
