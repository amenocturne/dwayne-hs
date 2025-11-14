# Task Management

Complete guide to creating, editing, and managing tasks in Dwayne.

## Creating Tasks

### Quick Capture (Inbox)

**Command**: `at`

**Workflow**:
1. Press `at` from any view
2. Your `$EDITOR` opens with a template
3. Write your task in Org format
4. Save and close editor
5. Task appears in inbox

**Example**:
```org
* INBOX Call dentist for appointment :personal:health:
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu 10:30]
  :END:
  Need to schedule teeth cleaning. Office hours 9-5.
```

**Tips**:
- Don't overthink, just capture the idea
- Add details in description if needed
- Processing happens later during review

### Creating in External Editor

**Command**: `Enter` on any task, then create new sections

**Example**:
```org
* TODO Main task
** TODO Subtask 1
** TODO Subtask 2
*** TODO Sub-subtask
```

## Editing Tasks

### Full Edit

**Command**: `Enter`

**What happens**:
1. Current task opens in `$EDITOR`
2. See complete Org format
3. Edit any field
4. Save and close
5. Changes validated and applied

**What you can edit**:
- TODO state
- Priority
- Title
- Tags
- Scheduled/deadline dates
- Properties
- Description

**Example editing**:
```org
* TODO [#A] Finish report :work:urgent:
SCHEDULED: <2025-11-15 Fri>
DEADLINE: <2025-11-17 Sun>
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu 10:30]
  :PROJECT: Q4 Review
  :END:
  Complete quarterly review report.
  Include sales figures and projections.
```

### Quick State Changes

**Commands**: `ti`, `tr`, `ts`, `tn`, `tl`, `tw`, `tp`, `tt`, `td`, `tx`

**Workflow**:
1. Navigate to task
2. Press command
3. State changes immediately

**No editor required** - fastest way to process tasks.

### Priority Changes

**Commands**:
- `Shift-Up`: Increase priority (A → B → C → None)
- `Shift-Down`: Decrease priority (None → C → B → A)

**Example workflow**:
```
Task starts with no priority
Shift-Up → [#C]
Shift-Up → [#B]
Shift-Up → [#A]
Shift-Up → no priority (cycles)
```

### Tag Management

#### Add Tags
- `a,m` - Add "music" tag
- `a,c` - Add "cool" tag
- `a,s` - Add "software" tag
- `a,b` - Add "book" tag

#### Remove Tags
- `d,m` - Delete "music" tag
- `d,c` - Delete "cool" tag
- `d,s` - Delete "software" tag
- `d,b` - Delete "book" tag

#### Custom Tags
Edit task (`Enter`) and add tags manually:
```org
* TODO Task title :custom:tag:another:
```

**Tag syntax**: `:tag1:tag2:tag3:`

## Task Structure

### Basic Task

Minimal task:
```org
* TODO Task title
```

### Complete Task

Task with all features:
```org
* TODO [#A] Task title :tag1:tag2:
SCHEDULED: <2025-11-15 Fri 09:00>
DEADLINE: <2025-11-17 Sun>
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu 10:30]
  :CUSTOM_PROPERTY: value
  :END:
  Task description can span multiple lines.

  Include as much detail as needed.

  Links: [[https://example.com]]
```

**Elements**:
1. **Level** (`*`, `**`, etc.): Hierarchy depth
2. **TODO state** (`TODO`, `INBOX`, etc.): Current status
3. **Priority** (`[#A]`, `[#B]`, `[#C]`): Importance
4. **Title**: Task summary
5. **Tags** (`:tag:`): Categories
6. **Timestamps**: Scheduled/deadline
7. **Properties**: Custom metadata
8. **Description**: Full details

### Hierarchical Tasks

**Parent task**:
```org
* PROJECT Write book :writing:
```

**Child tasks**:
```org
* PROJECT Write book :writing:
** TODO Outline chapters
** TODO Write chapter 1
*** TODO Research topic
*** TODO Create draft
*** TODO Edit and revise
** TODO Write chapter 2
** WAITING Editor feedback
```

**Levels**:
- `*` - Level 1 (top-level)
- `**` - Level 2 (child)
- `***` - Level 3 (grandchild)
- etc.

## Task Metadata

### TODO States

**Available states**:
- INBOX - Unprocessed
- RELEVANT - Needs attention
- SOMEDAY - Maybe later
- NOTES - Reference
- LIST - Checklist
- WAITING - Blocked
- PROJECT - Multi-step
- TODO - Ready to work
- DONE - Completed
- TRASH - Deleted

**Change with**: `ti`, `tr`, `ts`, `tn`, `tl`, `tw`, `tp`, `tt`, `td`, `tx`

### Priorities

**Levels**:
- `[#A]` - Highest priority (urgent/important)
- `[#B]` - Medium priority (important, not urgent)
- `[#C]` - Low priority (nice to have)
- No priority - Backlog

**Change with**: `Shift-Up`, `Shift-Down`

**Display**: Red (A), Yellow (B), Blue (C)

### Tags

**Purpose**: Categorize and filter tasks

**Common tags**:
- `:work:` - Work-related
- `:personal:` - Personal life
- `:urgent:` - Needs immediate attention
- `:phone:` - Phone call required
- `:computer:` - Needs computer
- `:errands:` - Outside the house
- `:waiting:` - Waiting for something
- `:someday:` - Not now

**Add in editor**:
```org
* TODO Task :tag1:tag2:tag3:
```

**Quick add**: `a,m`, `a,s`, etc.

### Dates and Times

#### Scheduled Date
When you plan to start:
```org
SCHEDULED: <2025-11-15 Fri>
```

With time:
```org
SCHEDULED: <2025-11-15 Fri 09:00>
```

#### Deadline
When task must be complete:
```org
DEADLINE: <2025-11-17 Sun>
```

#### Closed Date
Auto-set when marked DONE:
```org
CLOSED: [2025-11-14 Thu 15:30]
```

#### Created Date
Auto-set on task creation (in properties):
```org
:PROPERTIES:
:CREATED: [2025-11-14 Thu 10:30]
:END:
```

### Properties

**Custom key-value metadata**:
```org
:PROPERTIES:
:CREATED: [2025-11-14 Thu 10:30]
:PROJECT: Website Redesign
:EFFORT: 4h
:COST: $500
:CUSTOM_FIELD: any value
:END:
```

**Add in editor**: Edit task, add properties block

### Description

**Multi-line content**:
```org
* TODO Task title
  Description starts here.

  Can have multiple paragraphs.

  - Bullet points
  - More details

  Links: [[https://example.com]]
```

**Usage**:
- Context and background
- Step-by-step instructions
- Links and references
- Notes and thoughts

## Task Operations

### View Task Details

**Method**: Navigate to task with `j`/`k`

**Display**: Right pane shows full details

### Delete Task

**Soft delete**: `tx` (mark as TRASH)

**Hard delete**: Edit file (`Enter`), remove task lines, save

**Recommendation**: Use TRASH state, periodically clean up

### Duplicate Task

**Method**: Edit task (`Enter`), copy lines, paste, save

**Example**:
```org
* TODO Original task
* TODO Duplicate task (copy of above)
```

### Move Task

See [Refiling](#refiling-tasks) section.

### Complete Task

**Command**: `td`

**What happens**:
1. State changes to DONE
2. CLOSED timestamp added
3. Task moves to done view

**View completed**: `<space>ad`

### Reopen Task

**Method**: Navigate to DONE task, press `tt` (or any other state)

**What happens**:
- State changes from DONE
- CLOSED timestamp remains (for record)

## Refiling Tasks

### Move to Project

**Command**: `gr`

**Workflow**:
1. Navigate to task to move
2. Press `gr`
3. Refile dialog appears with all projects
4. Navigate with `j`/`k`
5. Press `Enter` to select project
6. Task moves under project as subtask

**Example**:
Before:
```org
File: inbox.org
* TODO Research competitors

File: projects.org
* PROJECT Market analysis
```

After `gr` and selecting "Market analysis":
```org
File: inbox.org
(task removed)

File: projects.org
* PROJECT Market analysis
** TODO Research competitors (moved here)
```

### Level Adjustment

Tasks preserve relative hierarchy:
```org
Before:
* TODO Parent task
** TODO Child task
*** TODO Grandchild task

After moving under level-2 project:
* PROJECT Some project
** TODO Parent task (now level 2)
*** TODO Child task (now level 3)
**** TODO Grandchild task (now level 4)
```

### Manual Refiling

**Method**: Edit files directly

**Workflow**:
1. Open source file in editor
2. Cut task (and subtasks)
3. Open destination file
4. Paste task
5. Adjust level (`*`, `**`, etc.)
6. Save both files
7. Restart Dwayne or refresh (`:r`)

## Batch Operations

### Selection Mode

**Enter**: `V`
**Exit**: `Esc`

**Select tasks**:
- `v` - Toggle current task
- `Shift-v` - Select range

**Apply operations**:
All state and tag commands work on selection:
- `tt` - Mark all selected as TODO
- `a,m` - Add "music" tag to all selected
- `Shift-Up` - Increase priority on all selected

**Example workflow**:
```
1. <space>ai (view inbox)
2. V (enter selection)
3. Navigate with j/k, press v on each task to select
4. tt (mark all as TODO)
5. a,w (add :work: tag to all) (if you had this keybinding)
6. Shift-Up (set all to priority C)
7. Esc (exit selection)
```

### Process Inbox in Batch

```
1. <space>ai (view inbox)
2. V (selection mode)
3. Select all similar tasks (e.g., all music-related)
4. a,m (add music tag)
5. tl (change to LIST)
6. Esc (exit)
7. Repeat for other categories
```

## Task Search

### Search Mode

**Enter**: `/`

**Type query**: Results update in real-time

**Exit**: `Esc`

**Smart case**:
- `/todo` - finds "todo", "TODO", "Todo" (case-insensitive)
- `/TODO` - finds only "TODO" (case-sensitive)

**Scope**: Searches title and description

**Combine with views**:
```
<space>at (show TODO tasks)
/ (search within TODO)
urgent (find urgent TODOs)
```

## Task URLs

### Add URL to Task

**In editor**:
```org
* TODO Check documentation
  See: [[https://docs.example.com]]
```

Or in title:
```org
* TODO Read article [[https://blog.example.com/post]]
```

### Open URL

**Command**: `gx`

**What happens**:
1. Finds first URL in task
2. Opens in default browser

**Supported formats**:
- `[[https://example.com]]` (Org-style)
- `https://example.com` (plain URL)

## Undo/Redo

### Undo Changes

**Command**: `u`

**What it undoes**:
- Task edits
- State changes
- Tag additions/removals
- Priority changes
- Task creation
- Refiling
- Bulk operations

**How far back**: Unlimited (limited by memory)

### Redo Changes

**Command**: `Ctrl-r`

**What it redoes**: Previously undone operations

### Example Workflow

```
1. tt (change to TODO) - oops, wrong task!
2. u (undo - back to previous state)
3. Navigate to correct task
4. tt (change correct task to TODO)
```

## Tips and Best Practices

### 1. Capture Quickly
Use `at` to quickly capture ideas without overthinking. Process later.

### 2. Process Regularly
Review inbox daily. Don't let it pile up.

### 3. Use Priorities Sparingly
Too many A priorities = no priorities. Be selective.

### 4. Tag for Context
Use tags to filter by where/when you can do tasks:
- `:online:` - Need internet
- `:offline:` - Can do without internet
- `:home:` - At home
- `:office:` - At office

### 5. Write Good Descriptions
Future you will thank you for context and details.

### 6. One Action per Task
If task has multiple steps, make it a PROJECT with subtasks.

### 7. Use Search
Faster than scrolling. `/keyword` to find specific tasks.

### 8. Leverage Bulk Operations
Process similar tasks together in selection mode.

### 9. Regular Reviews
Weekly review to keep system current and trusted.

### 10. Undo Freely
Experiment! You can always undo.
