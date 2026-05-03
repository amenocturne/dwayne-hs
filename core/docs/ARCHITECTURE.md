# Dwayne Architecture

This document describes the runtime architecture after the CQRS read-model
refactor (Phase 1–3, completed 2026-05). For the design rationale and the
step-by-step plan that produced this shape, see the vault-side spec at
`~/Vault/Obsidian/Mirror/projects/software/active/dwayne/dwayne-cqrs-read-model-spec.md`.

## TL;DR

- **Events are the canonical write log.** Every mutation appends one row to
  the `events` table; nothing else writes task state.
- **`task_current_state` is the canonical read model.** A SQL trigger
  projects each new event into a one-row-per-task summary. Views, search,
  and project queries read here; nobody folds events at request time.
- **`TaskRepo` is the access seam.** API handlers, the TUI's reload, and CLI
  views all go through it. Behind it lives `EventStoreRepo`, which talks to
  both tables.
- **Two server modes.** `dwayne --web` mounts the UI surface; `dwayne
  --sync-server` mounts the events transport; `dwayne --serve` mounts both
  for local dev convenience.
- **Android is local-first.** Same architecture: events log + projection,
  WorkManager-driven sync against `/api/events`.

## Tables

| Table                | Role                  | Writers                        | Readers                            |
| -------------------- | --------------------- | ------------------------------ | ---------------------------------- |
| `events`             | append-only write log | mutation handlers, sync push   | trigger; sync pull                 |
| `task_current_state` | derived read model    | trigger `events_to_state` only | `TaskRepo.queryTasks` / `getTask`  |
| `sync_state`         | sync bookkeeping (kv) | sync daemon                    | sync daemon                        |
| `migrations`         | applied-migration log | migration runner               | migration runner                   |

The legacy `tasks` and `task_history` tables were dropped in migration 005.
They had no live readers after Phase 2 and existed only as a fallback
during the events-as-source-of-truth transition.

## Data Flow

### Write path

```
Mutation request
   |
   v
TaskRepo.getTask(ptr)        -- single row from task_current_state
   |
   v
op :: Task -> Task           -- pure transformation
   |
   v
diffTaskAsEvent              -- only the fields that actually changed
   |
   v
TaskRepo.appendEvent         -- INSERT OR IGNORE INTO events
   |
   v
trigger events_to_state      -- upsert task_current_state for that task
```

The trigger recomputes the affected `(file_path, task_index)` row by
selecting the latest non-NULL value per field across that task's event
history. This is correct even if events arrive out of order (sync push
delivering older events after newer ones); see
`Events.Projection.foldTaskEvents` for the matching pure-projection
semantics.

### Read path

```
Query (view + filter + paginate)
   |
   v
TaskRepo.queryTasks            -- SQL on task_current_state with indexed predicates
   |
   v
[Task]                         -- already filtered/sorted/limited
```

`task_current_state` is indexed on `todo_keyword`, `(todo_keyword, priority)`,
`deadline`, `scheduled`, and `file_path`. Each view's `WHERE` clause hits
one of those indexes. Substring search on `title`/`description` uses SQL
`LIKE` for now; FTS5 is a future option if needed.

## Module Layout

| Module                   | Role                                                     |
| ------------------------ | -------------------------------------------------------- |
| `Events.Types`           | `Event` data type + JSON wire format                     |
| `Events.Diff`            | `diffTaskAsEvent` / `fullEvent` / `genesisEvent`         |
| `Events.Store`           | Insert/select on the `events` table                      |
| `Events.Projection`      | Pure event-fold helpers (used by tests + `dbExport`)     |
| `Repo.TaskRepo`          | Typeclass + query types                                  |
| `Repo.EventStoreRepo`    | `TaskRepo` instance over `events` + `task_current_state` |
| `Api.Handlers`           | Read + mutation handlers; sit on top of `TaskRepo`       |
| `Api.EventHandlers`      | Sync transport: `GET /api/events`, `POST /api/events`    |
| `Api.Server`             | Servant API trees + WAI Application wiring               |
| `Tui.RepoView`           | TUI's "render window" loader on top of `TaskRepo`        |
| `Tui.MutationEvents`     | TUI mutation hook: emits events on every state change    |
| `DB.Schema` / `DB.Migration` | Schema + migration registry                          |
| `DB.Export`              | Project events into `FileState` for org-file export      |
| `DB.TaskStore`           | Legacy `TaskStore` interface — TUI's reload/save seam    |

The TUI still keeps a `LinearHistory (FileState Task)` in `AppContext` for
client-side undo/redo and Brick rendering. That cache is hydrated from
`TaskRepo.queryTasks` at load time and after every mutation; it is not
the source of truth.

## Server Modes

`Api.Server` exposes three Servant API trees and three WAI applications:

| Mode               | CLI flag           | API tree       | Routes                                                                |
| ------------------ | ------------------ | -------------- | --------------------------------------------------------------------- |
| Web (UI only)      | `dwayne --web`     | `WebAPI`       | `/api/{views,search,projects,tasks/...}` + static assets at `/`       |
| Sync (events only) | `dwayne --sync-server` | `SyncAPI`  | `/api/events` (push + pull)                                           |
| Combined           | `dwayne --serve`   | `CombinedAPI`  | Everything above on a single port (default for local dev)             |

Routes that aren't mounted return HTTP 404. This lets a deployment expose
only what the host actually serves — e.g. a sync hub doesn't need to ship
the web bundle, and the local UI process doesn't need to accept push
events from other devices.

`runServer` (combined) and `runWebServer` both attach the WebSocket layer
for live UI updates; `runSyncServer` does not.

## Sync

Single-direction model:

- **Pull** — `GET /api/events?since=<iso8601>`. Server returns its events
  log filtered by `occurred_at`. Client `INSERT OR IGNORE`s into its own
  `events` table; the trigger projects each new row.
- **Push** — `POST /api/events { events: [...] }`. Same `INSERT OR IGNORE`
  semantics on the server side.
- **Conflict resolution** — none needed at row level. Two clients that
  emit different deltas at the same moment both make it into the log; the
  trigger's "latest non-NULL value per field" semantics decide which one
  shows up in `task_current_state`.

The sync daemon lives in `Sync.Client`. It runs as a separate process
against a `--sync-server` (or combined-mode) host. There is no
real-time push from the server back to clients — clients poll on an
interval.

## Android

The mobile app (under `mobile/`) mirrors the same architecture:

- Local SQLite with the same `events` + `task_current_state` schema (no
  code-shared schema yet, but the DDL is kept in sync by hand).
- `LocalTaskRepository` is the Kotlin analogue of `EventStoreRepo`.
- `EventProjection` recomputes a task from its events list (mirror of
  `Events.Projection.foldTaskEvents`).
- `SyncEngine` runs under WorkManager, hits `/api/events` push/pull on a
  schedule, and lets the local trigger handle the projection.

The transport interface (`SyncTransport`) is small enough that the same
events round-trip cleanly between clients regardless of language.

## Invariants

These hold after every mutation, sync cycle, and DB migration:

1. **Events are append-only.** No `UPDATE events`, no `DELETE FROM events`
   outside one-shot migrations. Sync push is `INSERT OR IGNORE`.
2. **`task_current_state` is fully derivable.** Drop it, replay every
   event in any order, get the same state. `dwayne dbRebuildState`
   exercises this.
3. **The trigger is the only writer to `task_current_state`.** No
   application code writes to it directly.
4. **Soft delete only.** Deletion is `todo_keyword = TRASH`, never row
   removal.

If any of these break, the sync layer's correctness arguments break with
them.
