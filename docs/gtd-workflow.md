# GTD Workflow with Dwayne

How to use Dwayne to implement the Getting Things Done methodology.

## GTD Principles

Getting Things Done (GTD) is a productivity methodology that focuses on:
1. **Capture**: Collect everything that has your attention
2. **Clarify**: Process what each item means and what to do about it
3. **Organize**: Put items where they belong
4. **Reflect**: Review your system regularly
5. **Engage**: Do the work with confidence

Dwayne implements this workflow through its TODO states and views.

## Task States Mapped to GTD

### INBOX - Capture
**Purpose**: Collect everything that comes to mind without processing.

**When to use**:
- Quick captures throughout the day
- Brain dumps during planning
- Any new task before you've thought about it

**Commands**:
- `at` - Add new task (goes to inbox)
- `ti` - Move task to inbox

**View**: `<space>ai` - Show all inbox tasks

### RELEVANT - Clarify & Organize
**Purpose**: Tasks that need your attention and are ready to plan.

**When to use**:
- After reviewing inbox items
- Tasks that are important and actionable soon
- Items you need to think about this week/month

**Commands**:
- `tr` - Mark as RELEVANT

**View**: `<space>ar` - Show relevant tasks (sorted by priority)

**Typical flow**:
```
INBOX → (review) → RELEVANT (with priority and tags)
```

### TODO - Organize (Next Actions)
**Purpose**: Concrete, actionable tasks you're committed to doing.

**When to use**:
- Tasks with clear next actions
- Items you plan to work on this week
- Tasks ready to start immediately

**Commands**:
- `tt` - Mark as TODO

**View**: `<space>at` - Show all TODO tasks (sorted by priority)

**Typical flow**:
```
RELEVANT → (plan) → TODO (with scheduled dates, clear next action)
```

### WAITING - Organize (Waiting For)
**Purpose**: Tasks blocked on someone else or external events.

**When to use**:
- Delegated tasks
- Waiting for information
- Dependent on external events

**Commands**:
- `tw` - Mark as WAITING

**View**: `<space>aw` - Show waiting tasks

**Best practice**: Add note about what you're waiting for and who/what is responsible.

### PROJECT - Organize (Projects)
**Purpose**: Multi-step outcomes requiring multiple tasks.

**When to use**:
- Anything requiring more than one action
- Complex goals with subtasks
- Coordinated efforts

**Commands**:
- `tp` - Mark as PROJECT
- `gp` - View project + all subtasks
- `gr` - Refile task into project

**View**: `<space>ap` - Show all projects

**Structure**:
```org
* PROJECT Write thesis :work:
** TODO Research existing literature
** TODO Create outline
** WAITING Advisor feedback on proposal
** TODO Write chapter 1
```

### SOMEDAY - Organize (Someday/Maybe)
**Purpose**: Things you might want to do but not now.

**When to use**:
- Ideas for the future
- Tasks you're not ready to commit to
- Dreams and aspirations

**Commands**:
- `ts` - Mark as SOMEDAY

**View**: `<space>as` - Show someday tasks

**Review frequency**: Monthly or quarterly

### NOTES - Reference
**Purpose**: Reference material, not actionable tasks.

**When to use**:
- Information to remember
- Reference documents
- Context for other tasks

**Commands**:
- `tn` - Mark as NOTES

**View**: `<space>an` - Show notes

### LIST - Organize (Checklists)
**Purpose**: Simple lists and checklists.

**When to use**:
- Shopping lists
- Books to read
- Movies to watch
- Music to download

**Commands**:
- `tl` - Mark as LIST
- `mm` - Music macro (sets to LIST + tags)

**View**: `<space>al` - Show lists

### DONE - Complete
**Purpose**: Completed tasks.

**When to use**:
- Task finished
- Ready to archive

**Commands**:
- `td` - Mark as DONE

**View**: `<space>ad` - Show completed tasks

**Auto-dates**: Sets CLOSED timestamp automatically.

### TRASH - Archive
**Purpose**: Deleted, cancelled, or irrelevant tasks.

**When to use**:
- Tasks no longer relevant
- Cancelled projects
- Mistakes or duplicates

**Commands**:
- `tx` - Mark as TRASH

**View**: `<space>ax` - Show trashed tasks

**Cleanup**: Periodically delete from files or keep for record.

## Daily GTD Workflow

### Morning Routine (10-15 minutes)

1. **Review INBOX** (`<space>ai`)
   ```
   - Process each item
   - Ask: What is it? Is it actionable?
   - Clarify next actions
   - Move to appropriate state (RELEVANT, TODO, SOMEDAY, etc.)
   - Add priorities and tags
   ```

2. **Review RELEVANT** (`<space>ar`)
   ```
   - Check what needs attention today
   - Move ready items to TODO
   - Update priorities
   ```

3. **Review TODO** (`<space>at`)
   ```
   - Pick 3-5 tasks for today
   - Add scheduled dates
   - Verify priorities (A for today, B for this week, C for later)
   ```

### During the Day

1. **Capture immediately** (`at`)
   ```
   - Any new task goes straight to inbox
   - Don't process, just capture
   - Continue with current work
   ```

2. **Work from TODO** (`<space>at`)
   ```
   - Focus on priority A tasks
   - Mark DONE when complete (td)
   - Update progress
   ```

3. **Check WAITING** (`<space>aw`)
   ```
   - Follow up on blocked items
   - Move to TODO if unblocked
   ```

### End of Day (5 minutes)

1. **Final inbox review** (`<space>ai`)
   ```
   - Process anything captured during the day
   - Get inbox to zero
   ```

2. **Save work** (`:w`)
   ```
   - Persist all changes
   ```

## Weekly Review (30-60 minutes)

### Friday afternoon or Sunday evening

1. **Review DONE** (`<space>ad`)
   ```
   - Celebrate wins
   - Extract lessons learned
   - Move to TRASH or keep for records
   ```

2. **Review PROJECTS** (`<space>ap`)
   ```
   - Check each project (gp on each)
   - Ensure each has a next action (TODO task)
   - Update project status
   - Add new subtasks as needed
   ```

3. **Review WAITING** (`<space>aw`)
   ```
   - Follow up on overdue items
   - Cancel if no longer relevant (tx)
   ```

4. **Review SOMEDAY** (`<space>as`)
   ```
   - Anything ready to activate?
   - Move to RELEVANT if priorities changed
   - Trim list of unrealistic items (tx)
   ```

5. **Review RELEVANT** (`<space>ar`)
   ```
   - Still relevant?
   - Move to TODO if ready
   - Move to SOMEDAY if not urgent
   - Update priorities
   ```

6. **Look ahead**
   ```
   - Check scheduled dates
   - Plan next week's priorities
   - Adjust priorities (Shift-Up/Down)
   ```

## Tips for GTD Success

### 1. Inbox Zero Daily
Process your inbox every day. Don't let it pile up.

### 2. Use Priorities Wisely
- **A**: Must do today/this week
- **B**: Important but not urgent
- **C**: Nice to have
- **None**: Backlog or reference

### 3. One Next Action per Project
Every PROJECT should have at least one TODO subtask.

### 4. Tags for Context
Use tags to filter by context:
```
:work: - Work-related
:home: - Personal
:computer: - Needs computer
:phone: - Can do on phone
:errands: - Outside tasks
```

Add tags with `a,` shortcuts or in editor.

### 5. Scheduled Dates for Deadlines
Use scheduled/deadline dates for:
- Hard deadlines
- Time-specific tasks
- Reminders

### 6. Search for Specific Tasks
Use `/` to find tasks quickly:
```
/report → find all tasks about reports
/urgent → find urgent tasks
```

### 7. Bulk Processing
Use selection mode (`V`) for:
- Marking multiple tasks DONE
- Adding tags to similar tasks
- Changing priorities in batch

### 8. Regular Backups
Your tasks are in plain text. Back them up:
```bash
cd ~/tasks
git add .
git commit -m "Daily backup"
git push
```

## Common GTD Scenarios

### Scenario 1: Email with Task
```
1. Read email with action item
2. at → capture task in inbox
3. Add context: "Reply to Bob about budget proposal :work:email:"
4. Continue with other emails
5. Later: Process inbox, mark as TODO with priority A
```

### Scenario 2: Big Project Arrives
```
1. Create project: at → "PROJECT Organize conference :work:"
2. tp → Mark as PROJECT
3. gp → View project
4. Add subtasks:
   - at → "TODO Find venue"
   - at → "TODO Book speakers"
   - at → "TODO Create schedule"
5. Prioritize first next action: navigate to "Find venue", Shift-Up → [#A]
```

### Scenario 3: Waiting for Someone
```
1. Delegate task to colleague
2. Create task: at → "WAITING Budget approval from manager"
3. tw → Mark as WAITING
4. Add scheduled date for follow-up: Enter → add SCHEDULED: <2025-11-20 Wed>
5. Weekly review: Check if unblocked, move to TODO if approved
```

### Scenario 4: Idea for Later
```
1. Random idea: "Learn Rust"
2. at → capture
3. During inbox processing: ts → SOMEDAY
4. Add tag: a,s → :software:
5. Monthly review: Move to RELEVANT when ready to commit
```

## GTD with Selection Mode

Process multiple tasks at once:

```
1. View inbox: <space>ai
2. Enter selection: V
3. Select similar tasks: v (on each)
4. Apply operation to all:
   - tt → mark all as TODO
   - a,w → add :work: tag to all
5. Exit selection: Esc
```

## Customizing Your Workflow

GTD is flexible. Adapt Dwayne to your style:

- **Add custom tags** for your contexts
- **Create macros** for repeated operations
- **Adjust view sorts** to match priorities
- **Use search** to create custom filters
- **Multiple files** for different areas (work.org, personal.org)

## Resources

- **GTD Book**: "Getting Things Done" by David Allen
- **Org Mode**: Dwayne uses Org Mode format, compatible with Emacs org-mode
- **Plain Text**: Your tasks are portable, future-proof text files
