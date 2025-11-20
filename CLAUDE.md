# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

**Build and Run:**
- `cabal build` - Build the project
- `cabal run` - Run with default config
- `DWAYNE_CONFIG=./resources/config.yml cabal run` - Run with custom config
- `just run` - Run with resources/config.yml (preferred)
- `just build` - Build the project

**Testing:**
- `cabal test` - Run all tests
- `just test` - Run all tests (preferred)

**Installation:**
- `cabal install --overwrite-policy=always` - Install executable
- `just install` - Install executable (preferred)

**Profiling:**
- `just profile` - Enable profiling and run with profiling output

## Architecture Overview

**Core Philosophy:** Maximum modularity through type classes - any component can be swapped without changing the rest of the system.

**Key Type Classes:**
- `Render` - Polymorphic UI rendering
- `Writer` - Serialization back to text format
- `Searcher` - Search functionality with smart case handling
- `Refileable` - Task movement and organization operations
- `Injection` - Bidirectional type conversions

**Module Organization:**
- `Model/` - Core data structures (OrgMode task model, LinearHistory for undo/redo)
- `Parser/` - Combinator-based parsers with location tracking
- `Writer/` - Serialization back to file formats
- `Tui/` - Brick-based terminal user interface
- `Render/` - Display formatting and rendering logic
- `Searcher/` - Search implementations
- `Refile/` - Task organization and movement
- `Validation/` - System and project validation

**State Management:**
- Uses `LinearHistory` for undo/redo functionality
- Lens-based state updates: `ctx & field1 . field2 .~ newValue`
- Event-driven pure state transitions
- Cached views that recompute only when filters change

**Task Model:**
The entire application is polymorphic over task types. The main implementation uses Org-mode format with support for TODO states, tags, scheduling, and rich metadata.

**Configuration:**
- Default config location: `~/.config/dwayne/config.yml`
- Override with `DWAYNE_CONFIG` environment variable
- Sample config in `resources/config.yml`

**Dependencies:**
- Built with Haskell using Cabal
- Major deps: brick (TUI), vty (terminal), lens (optics), text, vector, yaml
- Test framework: hspec with QuickCheck
