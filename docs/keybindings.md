# Keybindings Reference

Complete reference for all keyboard shortcuts in Dwayne.

## Legend

- `<space>` - Space bar
- `Ctrl-x` - Hold Control and press x
- `Shift-x` - Hold Shift and press x
- `xy` - Press x, then y in sequence

## Navigation

| Key | Action |
|-----|--------|
| `j` / `Down` | Move cursor down one task |
| `k` / `Up` | Move cursor up one task |
| `G` | Jump to last task |
| `gg` | Jump to first task |
| `Ctrl-o` | Jump back in navigation history |
| `Tab` | Jump forward in navigation history |

## Task Operations

| Key | Action |
|-----|--------|
| `Enter` | Edit selected task in external editor |
| `at` | Add new task to inbox |
| `u` | Undo last change |
| `Ctrl-r` | Redo last undone change |
| `r` | Refresh current view |
| `gx` | Open URL in task (browser) |

## Changing TODO State

All commands work on current task, or all selected tasks if in selection mode.

| Key | State | Description |
|-----|-------|-------------|
| `ti` | INBOX | Unprocessed task |
| `tr` | RELEVANT | Needs attention |
| `ts` | SOMEDAY | Maybe later |
| `tn` | NOTES | Reference/information |
| `tl` | LIST | Checklist/collection |
| `tw` | WAITING | Blocked/waiting |
| `tp` | PROJECT | Task with subtasks |
| `tt` | TODO | Ready to work on |
| `td` | DONE | Completed |
| `tx` | TRASH | Deleted/discarded |

## Tag Operations

All commands work on current task, or all selected tasks if in selection mode.

### Add Tags
| Key | Tag |
|-----|-----|
| `a,m` | music |
| `a,c` | cool |
| `a,s` | software |
| `a,b` | book |

### Delete Tags
| Key | Tag |
|-----|-----|
| `d,m` | music |
| `d,c` | cool |
| `d,s` | software |
| `d,b` | book |

## Priority

| Key | Action |
|-----|--------|
| `Shift-Up` | Increase priority (A → B → C → None) |
| `Shift-Down` | Decrease priority (None → C → B → A) |

## Views & Filters

All view shortcuts use `<space>a` prefix:

| Key | View | Default Sort |
|-----|------|-------------|
| `<space>aa` | All tasks | None |
| `<space>ai` | INBOX tasks | Creation date DESC |
| `<space>ar` | RELEVANT tasks | Priority ASC |
| `<space>as` | SOMEDAY tasks | None |
| `<space>an` | NOTES tasks | None |
| `<space>al` | LIST tasks | None |
| `<space>aw` | WAITING tasks | Priority ASC |
| `<space>ap` | PROJECT tasks | Priority ASC |
| `<space>at` | TODO tasks | Priority ASC |
| `<space>ad` | DONE tasks | None |
| `<space>ax` | TRASH tasks | None |

## Sorting

| Key | Sort Method |
|-----|------------|
| `sca` | By creation date (ascending) |
| `scd` | By creation date (descending) |
| `spa` | By priority (ascending - A first) |
| `spd` | By priority (descending - C first) |

## Project Operations

| Key | Action |
|-----|--------|
| `gp` | Go to project view (show project + all subtasks) |
| `gr` | Refile task to project (opens project selection dialog) |

## Selection Mode

| Key | Action |
|-----|--------|
| `V` (Shift-v) | Enter selection mode |
| `v` | Toggle selection on current task |
| `Shift-v` (in selection) | Select range from last selected to cursor |
| `Esc` | Exit selection mode |

### How Selection Works

1. Press `V` to enter selection mode
2. Navigate with `j`/`k`
3. Press `v` to toggle selection on current task
4. OR press `Shift-v` to select everything between last selection and cursor
5. Apply operations (like `tt`, `a,m`, etc.) - they affect ALL selected tasks
6. Press `Esc` to exit selection mode

## Search & Command Mode

| Key | Mode | Description |
|-----|------|-------------|
| `/` | Search | Filter tasks in real-time as you type |
| `:` | Command | Execute vim-style commands |
| `Esc` | - | Exit search/command mode |

### Search Behavior
- Type to filter tasks
- **Smartcase**: lowercase = case-insensitive, uppercase = case-sensitive
- Searches task title and description
- Results update in real-time

### Commands

| Command | Action |
|---------|--------|
| `:w` | Save all files (checks for external modifications) |
| `:w!` | Force save all files (ignore external modifications) |
| `:q` | Quit (only if no unsaved changes) |
| `:q!` | Force quit (discard unsaved changes) |
| `:wq` | Save and quit |

## Macros

| Key | Macro | Actions |
|-----|-------|---------|
| `mm` | Music macro | Add "music" and "download" tags, change to LIST |

## General

| Key | Action |
|-----|--------|
| `Esc` | Clear key buffer / Exit modes / Close dialogs |
| `Ctrl-c` | Quit application immediately |

## Multi-Key Sequences

Some commands require multiple keys in sequence. You have 1 second (default) to complete the sequence.

Examples:
- `<space>ai` - Press space, release, press a, press i (within 1 second)
- `sca` - Press s, press c, press a (within 1 second)
- `a,m` - Press a, press comma, press m (within 1 second)

If you wait too long, the sequence resets and you'll need to start over.

## Context-Aware Behavior

Many commands behave differently based on context:

### Selection Mode Active
- `ti`, `tr`, `tt`, `td`, etc. → Apply to ALL selected tasks
- `a,m`, `d,s`, etc. → Apply to ALL selected tasks

### Normal Mode
- Same commands → Apply only to current task

### In Dialog
- `j`/`k` or arrows → Navigate dialog options
- `Enter` → Confirm selection
- `Esc` → Cancel dialog

## Tips for Efficiency

1. **Learn the space-a prefix**: `<space>a` + letter gives you instant view switching
2. **Use selection mode for bulk changes**: Select many tasks, change state once
3. **Master gg and G**: Jump to start/end instantly
4. **Combine filters and sorts**: Use `<space>ar` then `spa` for relevant tasks by priority
5. **Use search within views**: Filter to INBOX (`<space>ai`), then search (`/query`)
6. **Undo is your friend**: Made a mistake? Just press `u`
7. **Learn the macros**: `mm` for music tasks saves time

## Keybinding Customization

Currently, keybindings are hardcoded in the source code. To customize:

1. Edit `src/Tui/Keybindings.hs`
2. Modify the `handleKeyPress` function
3. Rebuild: `cabal build`
4. Reinstall: `cabal install`

See [Development Guide](./development.md) for details on extending keybindings.
