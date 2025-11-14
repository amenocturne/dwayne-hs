# Search & Filtering

Complete guide to finding and filtering tasks in Dwayne.

## Search

### Basic Search

**Command**: `/`

**Workflow**:
1. Press `/` from any view
2. Type your query
3. Results update in real-time
4. Press `Esc` to clear search

**Example**:
```
/ → "report" → shows all tasks with "report" in title or description
```

### Smart Case Search

Dwayne uses **smartcase** matching (Vim-style):

**Case-insensitive** (default):
```
/todo → matches "todo", "TODO", "Todo", "tOdO"
/report → matches "report", "Report", "REPORT"
```

**Case-sensitive** (when uppercase in query):
```
/TODO → matches only "TODO"
/Report → matches only "Report"
/API → matches only "API" (not "api")
```

**Rule**: If your query has ANY uppercase letter, search becomes case-sensitive.

### Search Scope

Search looks in:
- Task title
- Task description (body text)

**Does NOT search**:
- Tags
- Properties
- TODO state
- Priority

**For those, use filters instead.**

### Search Within View

Search works on the **current filtered view**:

**Example 1**: Search within TODO tasks
```
<space>at → view TODO tasks (100 tasks)
/urgent → search within TODO (finds 5 tasks)
```

**Example 2**: Search within work tasks
```
<space>aa → view all tasks
# (manually filter to :work: tasks - not supported directly, but search helps)
/meeting :work: → finds work meetings
```

**Tip**: Combine view shortcuts with search for powerful filtering.

### Clear Search

**Command**: `Esc`

**Effect**: Returns to full view (still filtered by view shortcut if active)

### Search Tips

**1. Use partial matches**:
```
/conf → finds "conference", "confirm", "configuration"
```

**2. Search for phrases**:
```
/team meeting → finds tasks with both "team" and "meeting"
```

**3. Search for URLs**:
```
/github.com → finds tasks with GitHub links
```

**4. Search in descriptions**:
```
/instructions → finds tasks with "instructions" in description
```

**5. Case-sensitive when needed**:
```
/API → finds API-related tasks (not "api")
```

## Filtering by Views

### Quick View Filters

All view shortcuts use `<space>a` prefix:

**Filter by TODO state**:
- `<space>aa` - All tasks (no filter)
- `<space>ai` - INBOX tasks only
- `<space>ar` - RELEVANT tasks only
- `<space>as` - SOMEDAY tasks only
- `<space>an` - NOTES tasks only
- `<space>al` - LIST tasks only
- `<space>aw` - WAITING tasks only
- `<space>ap` - PROJECT tasks only
- `<space>at` - TODO tasks only
- `<space>ad` - DONE tasks only
- `<space>ax` - TRASH tasks only

**Example workflow**:
```
<space>ar → view only RELEVANT tasks
Navigate with j/k
Process tasks
<space>at → switch to TODO tasks
```

### View + Sort Combinations

Some views have default sorts:

| View | Sort |
|------|------|
| INBOX (`<space>ai`) | Creation date DESC (newest first) |
| RELEVANT (`<space>ar`) | Priority ASC (A tasks first) |
| TODO (`<space>at`) | Priority ASC (A tasks first) |
| WAITING (`<space>aw`) | Priority ASC (A tasks first) |
| PROJECT (`<space>ap`) | Priority ASC (A tasks first) |

**Override default sort**:
```
<space>ar → RELEVANT tasks (sorted by priority)
scd → change sort to creation date DESC
```

### View Persistence

Views persist until you change them:
```
<space>ar → RELEVANT view
Navigate, edit tasks, etc.
View stays on RELEVANT
<space>aa → switch to All tasks
```

### Return to Default

**Command**: `<space>aa`

**Effect**: Show all tasks, no TODO state filter

## Sorting

### Sort Commands

**By creation date**:
- `sca` - Sort by creation date ascending (oldest first)
- `scd` - Sort by creation date descending (newest first)

**By priority**:
- `spa` - Sort by priority ascending (A → B → C → none)
- `spd` - Sort by priority descending (none → C → B → A)

### Sort Behavior

**Persistence**: Sort stays active until changed

**Example**:
```
spa → priority ascending
All views now show A priority first
scd → creation date descending
All views now show newest first
```

**Combines with filters**:
```
<space>at → TODO tasks
spa → sorted by priority
Result: TODO tasks with A priority at top
```

### Default Sorts

Some views auto-apply sorts:

**INBOX** (`<space>ai`):
- Default: Creation date DESC
- Shows newest captures first
- Override: `spa` to sort by priority

**RELEVANT** (`<space>ar`):
- Default: Priority ASC
- Shows A priority first
- Override: `scd` to sort by date

### Clear Sort

No direct command to clear sort.

**Workaround**: Apply a different sort, then switch views.

## Advanced Filtering Techniques

### Technique 1: View + Search

**Use**: Find specific tasks within a state

**Example**: Find urgent TODO items
```
<space>at → TODO tasks
/urgent → search within TODO
Result: Only TODO tasks with "urgent" in title/description
```

### Technique 2: Sort + Navigate

**Use**: Focus on highest priority

**Example**: Work on most important tasks
```
<space>at → TODO tasks
spa → priority ascending
j/k → navigate top tasks (all priority A)
```

### Technique 3: Multiple File Organization

**Use**: Separate concerns by file

**Example**: Work vs personal
```
Configuration:
files:
  - ~/tasks/work.org
  - ~/tasks/personal.org

All work tasks in work.org with :work: tag
All personal tasks in personal.org with :personal: tag

Search for work: /work → finds all :work: tasks
Search for personal: /personal → finds all :personal: tasks
```

### Technique 4: Tag-Based Filtering

**Use**: Filter by context or category

**Example**: Find all music tasks
```
/ → music → shows all tasks with "music" in title, description, or :music: tag

(Note: Search doesn't directly search tags, but if you use tag names in title/description, it works)
```

**Better approach**: Use tag in title for searchability
```
* TODO Download album :music:download:
vs
* TODO Download music album - add tags
(Second version more searchable with '/music')
```

### Technique 5: Date-Based Review

**Use**: Find old unprocessed tasks

**Example**: Find stale inbox items
```
<space>ai → INBOX tasks
sca → oldest first
Result: Long-neglected inbox items at top
```

### Technique 6: Priority Filtering

**Use**: Focus on high-priority items across all states

**Example**: All priority A tasks
```
<space>aa → All tasks
spa → priority ascending
Result: All A priority tasks at top (across TODO, RELEVANT, etc.)
```

### Technique 7: Completion Review

**Use**: Review what you've accomplished

**Example**: Recently completed tasks
```
<space>ad → DONE tasks
scd → newest first
Result: Recently completed tasks at top
```

## Filtering Workflows

### Daily Review Workflow

**Goal**: Process inbox, focus on important work

```
1. <space>ai → View inbox
2. scd → Newest first
3. Process each task (tr, tt, ts, etc.)
4. <space>ar → View RELEVANT
5. spa → Priority A first
6. Pick 3-5 tasks for today
7. <space>at → View TODO
8. Work through tasks
```

### Weekly Review Workflow

**Goal**: Review all areas

```
1. <space>ad → Done tasks
   scd → newest first
   Review accomplishments

2. <space>ai → Inbox
   Process to zero

3. <space>ap → Projects
   For each: gp, verify next action

4. <space>aw → Waiting
   Follow up on blocked items

5. <space>as → Someday
   Promote ready items to RELEVANT

6. <space>ar → Relevant
   spa → priority ascending
   Update priorities for next week
```

### Focus Workflow

**Goal**: Work on one thing

```
1. <space>at → TODO tasks
2. spa → Priority A first
3. Pick ONE task
4. gp (if it's part of a project, see context)
5. Work on task
6. td when complete
```

### Search Workflow

**Goal**: Find specific task quickly

```
1. <space>aa → All tasks (or specific view)
2. /keyword → search
3. j/k → navigate results
4. Esc → clear search
5. Work with found task
```

## Filtering Limitations

### No Tag Filtering

**Current**: No direct filter by tag

**Workaround**:
- Use search: `/music` (finds tasks with "music" in title/description)
- Organize by file: Keep tagged tasks in separate files
- Use TODO states: Assign tags to specific states (e.g., all :music: tasks are LIST)

### No Date Filtering

**Current**: No filter by scheduled/deadline dates

**Workaround**:
- Sort by creation date as proxy
- Use search: `/2025-11` (finds tasks with November 2025 in content)
- Manual review in views

### No Custom Filters

**Current**: Filters are pre-defined (by TODO state)

**Workaround**:
- Combine view + search
- Use TODO states creatively (e.g., LIST for all :music: tasks)
- Multiple configurations for different contexts

### No Multi-Select Filters

**Current**: Can't filter by "TODO OR RELEVANT"

**Workaround**:
- Search for common characteristics
- Use ALL view + sort
- Process views sequentially

## Tips for Effective Filtering

### 1. Learn the View Shortcuts

Muscle memory for `<space>ai`, `<space>ar`, `<space>at` makes filtering instant.

### 2. Use Search Liberally

Don't scroll. Type `/keyword` to find tasks.

### 3. Combine Filters

View shortcut + search + sort = powerful custom views.

Example:
```
<space>at + /meeting + spa
= TODO tasks about meetings, priority A first
```

### 4. Regular View Checks

Cycle through views daily:
- Inbox (process)
- Relevant (review)
- TODO (work)
- Waiting (follow up)

### 5. Keep TODO States Clean

Use states consistently:
- INBOX = unprocessed
- RELEVANT = needs attention
- TODO = ready to work
- etc.

Makes filtering more effective.

### 6. Sort by Context

Different times, different sorts:
- Morning: Sort by priority (focus on important)
- End of day: Sort by creation date (capture everything)

### 7. Use Project View for Focus

When working on project:
```
gp → see only project tasks
Ignore everything else
```

### 8. Archive Regularly

Move DONE/TRASH to archive file.
Keeps active views clean and fast.

### 9. Leverage Smart Case

Use lowercase for broad searches, uppercase for specific:
- `/api` → finds "API", "api", "Api"
- `/API` → finds only "API"

### 10. Experiment

Try different combinations:
- `<space>ar` + `scd` (relevant tasks, newest first)
- `<space>aw` + `spa` (waiting tasks, by priority)
- `<space>aa` + `/urgent` (all urgent tasks)

Find what works for your workflow.
