# Features Overview

Comprehensive list of all Dwayne features and capabilities.

## Core Task Management

### CRUD Operations
- **Create**: Add tasks via external editor (`at`) or inline
- **Read**: View tasks in compact list and detailed pane
- **Update**: Edit tasks in external editor (`Enter`)
- **Delete**: Mark as TRASH (`tx`)

### Task Metadata
- **TODO Keywords**: INBOX, RELEVANT, SOMEDAY, NOTES, LIST, WAITING, PROJECT, TODO, DONE, TRASH
- **Priorities**: A (highest), B, C (lowest), or none
- **Tags**: Unlimited custom tags (`:tag1:tag2:`)
- **Dates**:
  - Created (automatic)
  - Scheduled (when to start)
  - Deadline (when to finish)
  - Closed (when completed)
- **Properties**: Custom key-value pairs
- **Description**: Multi-line text content

### Hierarchical Tasks
- Unlimited nesting depth (*, **, ***, etc.)
- Parent-child relationships
- Level-aware operations (preserves hierarchy)
- Automatic subtask detection

## GTD Workflow

### Predefined States
- **INBOX**: Capture all tasks here first
- **RELEVANT**: High-priority, needs attention
- **SOMEDAY**: Deferred to future
- **NOTES**: Reference material
- **LIST**: Checklists and collections
- **WAITING**: Blocked or waiting for others
- **PROJECT**: Tasks with subtasks
- **TODO**: Ready to work on
- **DONE**: Completed
- **TRASH**: Archived/deleted

### Quick State Transitions
Single-key commands to move tasks through workflow:
- `ti` → INBOX (capture)
- `tr` → RELEVANT (identify)
- `tt` → TODO (plan)
- `td` → DONE (complete)
- `tx` → TRASH (archive)

See [GTD Workflow Guide](./gtd-workflow.md) for methodology details.

## File Management

### Multiple Files
- Work with unlimited Org files simultaneously
- Each file maintains its own task hierarchy
- Quick file switching (implicit via view filters)

### Special Files
- **Inbox file**: Where new tasks are created (`at`)
- **Projects file**: Where PROJECT tasks should live
- **Automatic inclusion**: Inbox and projects files always included

### File Operations
- **Save**: Write all changes (`:w`)
- **Force save**: Override external modification checks (`:w!`)
- **Auto-detect changes**: Warns if files modified externally
- **Format preservation**: Maintains Org Mode syntax

## Search & Filtering

### Smart Search
- **Smartcase**: Case-insensitive unless query has uppercase
- **Real-time results**: Updates as you type
- **Scope**: Searches title and description
- **Filter within views**: Search works on current filtered view

Example:
- `/todo` → finds "todo", "TODO", "ToDo" (case-insensitive)
- `/TODO` → finds only "TODO" (case-sensitive)

### View Filters
Quick filters by TODO state:
- All tasks
- By specific state (INBOX, RELEVANT, TODO, etc.)
- Combines with search for powerful queries

## Sorting

### Available Sort Methods
- **By creation date**: Ascending or descending
- **By priority**: Ascending (A→B→C→none) or descending

### Sort Persistence
- Sorts persist across navigation
- Combine with filters for custom views
- Part of view specification (cached)

### Default Sorts
Some views have default sorts:
- INBOX view: Creation date descending (newest first)
- RELEVANT view: Priority ascending (A tasks first)
- TODO view: Priority ascending

## Undo/Redo

### Dual History Tracks
1. **File state history**: All data changes
2. **Navigation history**: View jumps and cursor position

### Full Undoability
Every operation that modifies data is undoable:
- Task creation/editing
- State changes
- Tag additions/removals
- Priority changes
- Refiling operations
- Bulk operations

### Commands
- `u`: Undo last change
- `Ctrl-r`: Redo last undone change
- `Ctrl-o`: Jump back in navigation history
- `Tab`: Jump forward in navigation history

## Project Management

### PROJECT Tasks
- Special task type for grouping subtasks
- Must live in projects file (validated)
- Higher hierarchy level than subtasks

### Project Operations
- **Create project**: Change task to PROJECT (`tp`)
- **View project**: Show project + all subtasks (`gp`)
- **Refile to project**: Move tasks into projects (`gr`)

### Project View (`gp`)
- Shows project task itself
- Shows all direct and nested subtasks
- Project always listed first
- Custom sorting (project first, then normal sort)

### Validation
- Checks PROJECT tasks are in projects file
- Offers to move misplaced PROJECT tasks
- Preserves subtask hierarchy when moving

## Selection & Bulk Operations

### Selection Mode
- Enter with `V`
- Toggle individual tasks with `v`
- Select ranges with `Shift-v`
- Visual indicator shows selected tasks

### Bulk Operations
When selection mode is active, these commands apply to ALL selected tasks:
- TODO state changes (`ti`, `tr`, `tt`, `td`, etc.)
- Tag operations (`a,m`, `d,s`, etc.)

### Use Cases
- Mark 10 tasks as DONE at once
- Add "urgent" tag to multiple tasks
- Move many tasks to INBOX for re-processing

## External Editor Integration

### Full Editing
- Opens task in `$EDITOR` environment variable
- Shows complete Org syntax
- Edit any aspect of task
- Validates syntax on save

### New Task Creation
- `at` creates task in editor
- Start with template
- Full Org format supported

### Supported Editors
Any terminal or GUI editor:
- vim, nvim, emacs
- nano, micro
- VS Code (`code --wait`)
- Sublime Text, etc.

## URL Handling

### URL Detection
Finds URLs in:
- Task title
- Task description (body)

Supports:
- Org-style links: `[[https://example.com]]`
- Plain URLs: `https://example.com`

### Open in Browser
- Press `gx` on task with URL
- Extracts first URL found
- Opens in default browser (macOS: `open` command)

## Color Scheme & Display

### Catppuccin Mocha Theme
Default color scheme with:
- **States**: Different colors per TODO keyword
  - INBOX: Lavender
  - RELEVANT/WAITING: Mauve
  - NOTES/PROJECT: Green
  - TODO: Blue
  - DONE: Teal
  - TRASH: Overlay2 (gray)
- **Priorities**: Red (A), Yellow (B), Blue (C)
- **Hierarchy levels**: Color-coded depth (5 levels)
- **Tags**: Sky blue

### Syntax Highlighting
- Color-coded task components
- Visual hierarchy via indentation and color
- Clear state indication

## Validation System

### Automatic Checks
Runs on startup and after certain operations:
- PROJECT tasks in correct file
- Task hierarchy consistency
- File format validity

### Interactive Fixes
- Displays validation dialog
- Explains issue
- Offers automatic fix
- User can accept or reject
- All fixes are undoable

## Macros

### Pre-defined Macros
- **Music macro** (`mm`): Add "music" and "download" tags, set to LIST

### Macro System
Macros are sequences of operations executed in order. Extensible via code modification.

## Caching & Performance

### View Caching
- Filtered views cached
- Only recompute when filter/sort changes
- Version tracking prevents stale cache

### Efficient Data Structures
- Vector-based storage for O(1) indexing
- Lazy computation where possible
- Minimal re-rendering

## Viewport Management

### Smart Scrolling
- Configurable margin (default 6 lines from edge)
- Auto-scroll when cursor approaches edge
- Cursor stays visible at all times

### Navigation
- Page up/down via cursor movement
- Jump to top/bottom (gg/G)
- Auto-center on jump operations

## Configuration

### Config File
Location: `~/.config/dwayne/config.yml` (override with `DWAYNE_CONFIG`)

### Options
- File list
- Inbox file path
- Projects file path
- Scrolling margin
- Key timeout
- Color scheme (extensible)

See [Configuration Guide](./configuration.md) for details.

## Data Format

### Org Mode
- Plain text format
- Human-readable
- Git-friendly
- Editor-agnostic
- Cross-platform

### Format Features
- Task headlines with metadata
- Properties blocks
- Timestamps with repeat/delay intervals
- Tags and priorities
- File-level configuration

See [Org Mode Format](./org-mode-format.md) for syntax details.

## Extensibility

### Type Class Architecture
Swap any component by implementing type classes:
- `Render`: Custom rendering
- `Writer`: Custom serialization
- `Searcher`: Custom search logic
- `Refileable`: Custom organization

### Customization Points
- Keybindings (code modification)
- TODO keywords (code modification)
- Tag shortcuts (code modification)
- Macros (code modification)
- Color schemes (code modification)
- Sort methods (code modification)

## Advanced Features

### Time Support
- **Dates**: `YYYY-MM-DD DayOfWeek`
- **DateTimes**: `YYYY-MM-DD DayOfWeek HH:MM`
- **Repeat intervals**: `+Xd`, `++Xd`, `.+Xd`
- **Delay intervals**: `-Xd`, `--Xd`
- **Time units**: h (hour), d (day), w (week), m (month), y (year)

### Properties System
Custom key-value pairs on tasks:
```org
:PROPERTIES:
:CREATED: [2025-11-14 Thu 10:30]
:CUSTOM_FIELD: value
:END:
```

### Level Adjustment
When moving tasks, hierarchy preserved:
- Project at level 1 with subtasks at 2-3
- After move: relative levels maintained
- No flattening or breaking of structure

## Limitations

### Current Constraints
- Single color scheme (though framework supports more)
- Hardcoded keybindings (require code changes)
- No fuzzy search (substring only)
- Terminal-bound (no GUI)
- No cloud sync or collaboration

### Deliberate Choices
- Plain text only (no database)
- Vim-style interface (steep learning curve)
- Modal design (different modes for different operations)
- External editor for complex editing (leverage existing tools)
