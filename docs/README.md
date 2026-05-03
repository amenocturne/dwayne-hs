# Dwayne Documentation

Welcome to the Dwayne documentation! Dwayne is a powerful TUI (Terminal User Interface) application for GTD-style task management using Org Mode plain-text files.

## Documentation Index

### Getting Started
- [Installation & Quick Start](./getting-started.md) - Install, configure, and start using Dwayne
- [Configuration Guide](./configuration.md) - Configure Dwayne for your workflow

### User Guide
- [Features Overview](./features.md) - Complete feature list and capabilities
- [Keybindings Reference](./keybindings.md) - All keyboard shortcuts and commands
- [GTD Workflow](./gtd-workflow.md) - Using Dwayne for Getting Things Done methodology
- [Task Management](./task-management.md) - Working with tasks: creating, editing, organizing
- [Projects & Organization](./projects.md) - Managing projects and hierarchical tasks
- [Search & Filtering](./search-filtering.md) - Finding and filtering tasks

### Advanced Topics
- [Org Mode Format](./org-mode-format.md) - Understanding the underlying data format
- [Selection & Bulk Operations](./bulk-operations.md) - Working with multiple tasks at once
- [External Editor Integration](./external-editor.md) - Using your favorite editor with Dwayne

### Reference
- [Architecture Overview](./architecture.md) - Technical architecture and design philosophy
- [Development Guide](./development.md) - Building, testing, and extending Dwayne

## What is Dwayne?

Dwayne combines:
- **Vim-like keybindings** for efficient navigation and editing
- **GTD methodology** with predefined workflow states
- **Plain-text Org Mode format** for flexibility and version control
- **Terminal interface** for speed and accessibility

## Key Features

- Full task lifecycle management (create, edit, complete, archive)
- Hierarchical task organization with unlimited nesting
- Rich metadata: priorities, tags, dates, properties
- Multiple file support with automatic inbox and projects files
- Smart search with case-sensitive/insensitive modes
- Flexible filtering and sorting
- Complete undo/redo system
- Bulk operations via selection mode
- Project management with validation
- External editor integration

## Quick Reference

### Essential Commands
- `j`/`k` - Navigate tasks
- `Enter` - Edit task in external editor
- `at` - Add new task
- `/` - Search tasks
- `:w` - Save changes
- `:q` - Quit
- `u` / `Ctrl-r` - Undo / Redo

### Quick Views
- `<space>ai` - Show INBOX
- `<space>ar` - Show RELEVANT tasks
- `<space>at` - Show TODO tasks
- `<space>aa` - Show all tasks

### Change Task State
- `ti` - Mark as INBOX
- `tr` - Mark as RELEVANT
- `tt` - Mark as TODO
- `td` - Mark as DONE

See [Keybindings Reference](./keybindings.md) for the complete list.

## Getting Help

- Check the documentation files in this directory
- Review the [CLAUDE.md](../CLAUDE.md) file for development commands
- Examine the sample configuration in [resources/config.yml](../resources/config.yml)
