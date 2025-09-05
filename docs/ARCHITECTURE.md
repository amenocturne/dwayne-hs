# Dwayne-hs Architecture Guide

## Design Philosophy

### The Core Vision: Maximum Modularity
Dwayne is designed so that **any component can be swapped without changing the rest of the system**. Whether you use Org-mode or todo.txt format, fuzzy or exact search, simple or complex task models - the core engine remains unchanged.

### Key Design Mindset

**1. Swappable Modules**
Every major behavior is defined by a type class interface:
- `matches query task` works identically whether using fuzzy search, exact matching, or field-priority search
- Change behavior by importing a different instance, not by modifying call sites
- Think "dependency injection" but enforced by the type system

**2. Task Type Polymorphism** 
The entire application is polymorphic over the task type:
- Someone with `SimpleTask { title, status }` gets the full TUI experience
- Someone with `ComplexTask { dependencies, estimates, projects }` gets the same interface
- Type classes ensure only sensible operations are available for each type

**3. Vim-Inspired Efficiency**
- **Composable Commands**: Like vim's "d5w" (delete 5 words), operations should compose naturally
- **Modal Interface**: Different modes for different mental models of interaction
- **Undo/Redo Everything**: If it changes user data, it should be undoable (LinearHistory)
- **Efficient Navigation**: Minimal keystrokes for common operations

**4. Type Safety as Design Guide**
Function signatures should tell you exactly what happens:
- Pure functions for transformations and business logic
- Controlled side effects only where necessary (IO, file operations)
- Type classes prevent nonsensical implementations at compile time

## Key Type Classes

### Core Abstractions

- **`RenderTask`**: Polymorphic UI rendering (compact/full views, with/without colors)
- **`Writer`**: Serialization back to text format
- **`Searcher`**: Search with smart case handling
- **`Refileable`**: Task movement and organization operations
- **`Injection`**: Bidirectional type conversions

### The Pattern

Each domain concept follows: `Abstract Type Class → Generic Implementation → Specific Instance`

Example: `Searcher` (abstract) → search algorithms → `OrgSearcher` (Org-mode specific)

## Parser System

- **Combinator-Based**: Composable, small parsers build complex ones
- **Location Tracking**: For meaningful error messages
- **Backtracking**: `tryParser` enables rollback on failure
- **Domain-Specific**: Org-mode parsers built on generic foundation

## State Management Patterns

- **LinearHistory**: Undo/redo for complex user actions
- **Lens Composition**: `ctx & field1 . field2 . field3 .~ newValue`
- **Cached Views**: Recompute only when filters/sorting change
- **Event-Driven**: Pure state transitions triggered by user events

## Developer Guide: The Architectural Mindset

### When Implementing Feature X

**Step 1: Understand the Abstraction**
- How does this fit into existing type classes?
- What would a "most general interface" look like?
- What are 2-3 concrete specializations I can imagine?

**Step 2: Design for Swappability**
- Could someone want different behavior here?
- What would they need to implement to swap this module?
- Are the dependencies minimal and well-defined?

**Step 3: Maintain Type Safety**
- Does the function signature clearly express what happens?
- Are side effects controlled and obvious?
- Do type class constraints prevent nonsensical implementations?

### Interface Design Principles

**Minimal but Complete**: Include only what's necessary, but ensure the interface can express all reasonable use cases.

**Function Signature Hierarchy**: 
- Specific contexts (just `FileState`) when possible
- Broader contexts (`AppContext`) when needed  
- Never overreach (avoid `GlobalAppStateF` unless truly necessary)

**Pure by Default**: Side effects should be explicit and justified, not scattered throughout the codebase.
