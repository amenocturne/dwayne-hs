# Agent Guide for Dwayne-hs

## Project Context

This is a personal project built for the author's own use. **Do not prioritize backward compatibility** when refactoring or adding features. Breaking changes are acceptable if they improve code quality or architecture.

## Build/Test Commands
- `just build` or `cabal build` - Build project
- `just test` or `cabal test` - Run all tests
- `cabal test --test-show-details=direct --test-options="-m <pattern>"` - Run specific test (match by name)
- `just run` - Run with resources/config.yml
- `just format` - Format code with ormolu

## Code Style

**Language Extensions:** Use pragmas at top of file: `{-# LANGUAGE OverloadedStrings #-}`, `FlexibleContexts`, `TemplateHaskell`, `MultiParamTypeClasses`

**Imports:** Qualified imports for containers (`Data.Map.Strict as M`, `Data.Text as T`, `Data.Set as S`, `Data.Vector as V`). Lens imports: `Control.Lens` with explicit operators or `makeLenses`.

**Types:** Use lens-generated accessors (e.g., `_field` with `makeLenses ''TypeName`). Prefer type classes for polymorphism (`Render`, `Writer`, `Searcher`, `Refileable`, `Injection`).

**Naming:** Lowercase with underscores for record fields before `makeLenses`, camelCase for functions. Use descriptive names (e.g., `taskLevelParser`, `repeatInterval`).

**State Updates:** Use lens operators: `ctx & field1 . field2 .~ newValue` or `ctx ^. field` for reads.

**Error Handling:** Return `ParserResult` (ParserSuccess/ParserError) for parsers. Use `Maybe` and `Either` for operations. IO errors caught with `Control.Exception.catch`.

**Testing:** Use hspec with `describe`/`it`. Test specs in `test/` mirror `src/` structure. Use `shouldBe` assertions and check parser results with `runParser`.

**Comments:** Only add comments that provide non-obvious context or explain "why" rather than "what". Remove comments that merely restate what the code already clearly expresses. Top-level Haddock comments with examples are valuable. Inline comments should be rare and meaningful.
