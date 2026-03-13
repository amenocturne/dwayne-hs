# Dwayne

GTD-style task manager: Brick TUI + Servant web API, org-mode files as storage.

## Structure

- `core/` — Haskell codebase (cabal project)
- `web/` — Frontend (TypeScript, npm)
- `raycast/` — Raycast extension (TypeScript, bun)

## Commands

```
just haskell-build          # build haskell
just haskell-test           # run tests
just haskell-format         # ormolu formatting
just haskell-run            # run TUI (needs DWAYNE_CONFIG)
just haskell-serve          # run API server
just web-dev / web-build    # frontend dev/build
just raycast-install        # install + register raycast extension
just raycast-dev            # raycast hot-reload dev
```

## Architecture

Commands are the central abstraction — each command can have TUI, CLI, and API bindings.
- `Commands/Registry.hs` — all commands registered here
- `Commands/Command.hs` — Command type, shared filtering logic
- View API routes dispatch dynamically from command registry (catch-all `views/:name`)
- Search and Projects endpoints are standalone in `Api/Server.hs`

Adding a new view: create command in `Commands/Views.hs` with `cmdApi` binding, register in `Registry.hs`. No Server.hs changes needed.

## Key types

- `Task` — defined in `Model/OrgMode.hs`
- `AppContext` — TUI state container in `Tui/Types.hs`
- `FileState` — `Map FilePath (ParserResult (TaskFile Task))` in `Core/Types.hs`
