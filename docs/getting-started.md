# Getting Started with Dwayne

## Installation

### Prerequisites
- Haskell toolchain (GHC and Cabal)
- A terminal emulator with true color support
- Git (for version control of your task files)

### Build from Source

1. Clone the repository (if not already done)
2. Build and install:
   ```bash
   just install
   # or
   cabal install --overwrite-policy=always
   ```

This installs the `dwayne` executable to your Cabal bin directory (usually `~/.cabal/bin`).

### Verify Installation

```bash
dwayne --version  # (if implemented)
# or just run:
dwayne
```

## Initial Configuration

### Create Configuration Directory

```bash
mkdir -p ~/.config/dwayne
```

### Create Configuration File

Create `~/.config/dwayne/config.yml`:

```yaml
files:
  - ~/tasks/inbox.org
  - ~/tasks/projects.org
  - ~/tasks/personal.org
inboxFile: ~/tasks/inbox.org
projectsFile: ~/tasks/projects.org
scrollingMargin: 6
keyTimeoutMs: 1000
colorScheme: "default"
```

### Configuration Options

- **files**: List of Org files to manage (will be created if they don't exist)
- **inboxFile**: Where new tasks are added (should be in the files list)
- **projectsFile**: Where PROJECT tasks should live
- **scrollingMargin**: Number of lines from edge before scrolling (default: 6)
- **keyTimeoutMs**: Timeout for multi-key sequences in milliseconds (default: 1000)
- **colorScheme**: Color theme (currently only "default" is available)

### Create Initial Task Files

Create your task files with basic structure:

**~/tasks/inbox.org**:
```org
#+TITLE: Inbox

* INBOX First task
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu 10:30]
  :END:
```

**~/tasks/projects.org**:
```org
#+TITLE: Projects

* PROJECT Example Project :work:
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu 10:30]
  :END:
** TODO First project task
   :PROPERTIES:
   :CREATED: [2025-11-14 Thu 10:31]
   :END:
```

## First Run

### Launch Dwayne

```bash
dwayne
```

Or with custom config:
```bash
DWAYNE_CONFIG=/path/to/config.yml dwayne
```

### Quick Orientation

When Dwayne starts:
- **Left pane**: Shows list of tasks (compact view)
- **Right pane**: Shows detailed view of selected task
- **Status line**: Shows task count and current file

### Basic Navigation

Try these commands to get familiar:

1. **Move between tasks**: Press `j` (down) and `k` (up)
2. **View inbox**: Press `<space>ai` (space, then a, then i)
3. **Add a task**: Press `at`, type your task in your editor, save and close
4. **Mark task as TODO**: Navigate to a task, press `tt`
5. **Search tasks**: Press `/`, type a search query, press Enter
6. **Save your work**: Press `:`, type `w`, press Enter
7. **Quit**: Press `:`, type `q`, press Enter

## Understanding the Interface

### Left Pane (Task List)

Shows tasks in compact format:
```
* INBOX First task :tag1:tag2:
** TODO Subtask [#A]
```

Elements:
- `*` / `**` - Task level (hierarchy depth)
- `INBOX`, `TODO` - Task state
- `[#A]` - Priority (A is highest)
- `:tag1:tag2:` - Tags

### Right Pane (Details)

Shows complete task information:
- Full title with metadata
- Properties (CREATED, custom properties)
- Scheduled/Deadline dates
- Full description text
- File location and task count

## Your First Workflow

### 1. Capture Tasks
```
at → type task in editor → save and quit
```
New tasks go to your inbox file with INBOX state.

### 2. Review Inbox
```
<space>ai → view all INBOX tasks
```

### 3. Organize Tasks
For each task, decide:
- Change to RELEVANT: `tr` (important, needs attention)
- Change to TODO: `tt` (ready to work on)
- Change to SOMEDAY: `ts` (maybe later)
- Add priority: `Shift-Up` (cycle through A, B, C)
- Add tags: `a,m` (music), `a,s` (software), etc.

### 4. Work on Tasks
```
<space>at → view TODO tasks
Navigate to task → Enter → edit in full editor
Mark complete → td
```

### 5. Save Your Work
```
:w → saves all changes to files
```

## Next Steps

- Read [GTD Workflow](./gtd-workflow.md) to understand the methodology
- Learn all [Keybindings](./keybindings.md) for maximum efficiency
- Explore [Features](./features.md) for advanced capabilities
- Set up [Projects](./projects.md) for complex task hierarchies

## Common Issues

### Editor Not Found
Set your `EDITOR` environment variable:
```bash
export EDITOR=vim  # or nano, emacs, code, etc.
```

### Configuration Not Loaded
Dwayne looks for config at `~/.config/dwayne/config.yml` by default.
Override with:
```bash
DWAYNE_CONFIG=/path/to/config.yml dwayne
```

### Files Not Found
Make sure all files in your config exist, or Dwayne will fail to start.
Create them first:
```bash
touch ~/tasks/inbox.org ~/tasks/projects.org
```
