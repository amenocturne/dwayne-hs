# Org Mode Format

Understanding the plain-text format used by Dwayne for task storage.

## What is Org Mode?

**Org Mode** is a plain-text markup format originally from Emacs. It's designed for:
- Hierarchical outlines
- Task management
- Note-taking
- Documentation

**Benefits**:
- Human-readable
- Version control friendly (Git)
- Editor-agnostic
- Future-proof (plain text)
- Cross-platform

Dwayne uses a subset of Org Mode syntax for task management.

## File Structure

### Basic File

```org
#+TITLE: My Tasks

* TODO First task
* TODO Second task
** TODO Subtask
```

### File Header

Optional file title:
```org
#+TITLE: Inbox
#+AUTHOR: Your Name
#+DATE: 2025-11-14
```

Dwayne only uses `#+TITLE`. Other headers are preserved but ignored.

## Task Syntax

### Minimal Task

```org
* TODO Task title
```

**Components**:
- `*` - Task level (hierarchy)
- `TODO` - TODO keyword (state)
- `Task title` - Title text

### Complete Task

```org
* TODO [#A] Task title :tag1:tag2:
SCHEDULED: <2025-11-15 Fri 09:00>
DEADLINE: <2025-11-17 Sun>
CLOSED: [2025-11-14 Thu 15:30]
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu 10:30]
  :CUSTOM: value
  :END:
  Task description starts here.

  Can have multiple paragraphs.

  Links: [[https://example.com]]
```

## Hierarchy Levels

### Level Indicators

```org
* Level 1 (top-level)
** Level 2 (child of level 1)
*** Level 3 (grandchild)
**** Level 4
***** Level 5
... (unlimited)
```

**Syntax**: Number of `*` characters indicates level.

**Rules**:
- Minimum one `*`
- Must have space after `*` and before TODO keyword or title
- All subtasks must be at higher level than parent

### Example Hierarchy

```org
* PROJECT Main project
** TODO First task
** TODO Second task
*** TODO Sub-task of second
**** TODO Sub-sub-task
** TODO Third task
```

## TODO Keywords

### Syntax

```org
* KEYWORD Task title
```

**Available keywords** (in Dwayne):
- INBOX
- RELEVANT
- SOMEDAY
- NOTES
- LIST
- WAITING
- PROJECT
- TODO
- DONE
- TRASH

**Rules**:
- Keyword is uppercase
- Comes after level indicator (`*`)
- Before priority and title
- Space-separated from title

### Examples

```org
* INBOX Process this later
* RELEVANT Important task
* PROJECT Multi-step outcome
* DONE Completed task
```

## Priorities

### Syntax

```org
* TODO [#A] High priority task
* TODO [#B] Medium priority task
* TODO [#C] Low priority task
* TODO No priority task
```

**Available priorities**:
- `[#A]` - Highest
- `[#B]` - Medium
- `[#C]` - Lowest
- (none) - No priority

**Rules**:
- Format: `[#X]` where X is A, B, or C
- After TODO keyword
- Before task title
- Space before and after

## Tags

### Syntax

```org
* TODO Task title :tag1:tag2:tag3:
```

**Format**: `:tagname:`

**Rules**:
- At end of title line
- Colon before and after each tag
- No spaces in tag names
- Tags joined together (`:tag1:tag2:` not `:tag1: :tag2:`)

### Examples

```org
* TODO Work task :work:urgent:
* PROJECT Website :web:design:
* TODO Call dentist :personal:health:phone:
* INBOX Random idea :someday:maybe:
```

### Common Tags

**Context tags**:
- `:work:` - Work-related
- `:personal:` - Personal
- `:home:` - At home
- `:office:` - At office
- `:computer:` - Need computer
- `:phone:` - Phone call
- `:online:` - Need internet
- `:offline:` - No internet needed

**Category tags**:
- `:urgent:` - High urgency
- `:important:` - High importance
- `:someday:` - Future task
- `:quick:` - Quick task (<5 min)

**Project tags**:
- `:project_name:` - Link to specific project
- `:client_name:` - Client work

## Timestamps

### Scheduled Date

**Syntax**:
```org
* TODO Task title
SCHEDULED: <2025-11-15 Fri>
```

**With time**:
```org
SCHEDULED: <2025-11-15 Fri 09:00>
```

**Rules**:
- Angle brackets: `< >`
- Format: `YYYY-MM-DD DayOfWeek`
- Optional time: `HH:MM`
- On separate line after title (before properties)

### Deadline

**Syntax**:
```org
* TODO Task title
DEADLINE: <2025-11-17 Sun>
```

**Same format rules as SCHEDULED.**

### Closed Date

**Syntax**:
```org
* DONE Task title
CLOSED: [2025-11-14 Thu 15:30]
```

**Rules**:
- Square brackets: `[ ]` (not angle brackets)
- Auto-set when task marked DONE
- Usually includes time

### Date Format

**Date only**:
```
<2025-11-14 Thu>
```

**Date with time**:
```
<2025-11-14 Thu 10:30>
```

**Components**:
- Year: 4 digits
- Month: 2 digits (01-12)
- Day: 2 digits (01-31)
- Day of week: 3 letters (Mon, Tue, Wed, Thu, Fri, Sat, Sun)
- Time: HH:MM (24-hour format)

## Properties

### Syntax

```org
* TODO Task title
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu 10:30]
  :CUSTOM_KEY: Custom value
  :ANOTHER_KEY: Another value
  :END:
```

**Rules**:
- Start with `:PROPERTIES:` line
- End with `:END:` line
- Each property: `:KEY: value`
- Indented (usually 2 spaces)
- After timestamps, before description

### Common Properties

**Automatic**:
```org
:CREATED: [2025-11-14 Thu 10:30]
```

**Custom**:
```org
:PROJECT: Project name
:EFFORT: 2h
:COST: $100
:ASSIGNED: Person name
:STATUS: In progress
:CUSTOM: Any value
```

### Examples

```org
* TODO Task
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu 10:30]
  :EFFORT: 4h
  :PROJECT: Website Redesign
  :END:
```

## Task Description

### Syntax

```org
* TODO Task title
  Task description starts here.

  Multiple paragraphs allowed.

  - Bullet points
  - More points

  Blank lines separate paragraphs.
```

**Rules**:
- After properties (or title if no properties)
- Indented (usually 2 spaces)
- Plain text
- Blank lines for paragraphs
- Continues until next task (`*`) or end of file

### Formatting

**Plain text** (mostly):
```org
* TODO Task
  This is plain text.

  Links: [[https://example.com]]
  Bold: *bold text*
  Italic: /italic text/
  Code: =code=
```

Dwayne displays description as-is (minimal formatting).

## Links

### Org-Style Links

```org
[[https://example.com]]
[[https://example.com][Link text]]
```

**Format**:
- Double square brackets
- URL or file path
- Optional link text after URL in second brackets

### Plain URLs

```org
https://example.com
```

Just paste URL directly.

### In Tasks

```org
* TODO Read article
  Article: [[https://blog.example.com/post]]

* TODO Review documentation [[https://docs.example.com]]
```

## Repeat Intervals

### Repeat Types

**Next date** (`+`):
```org
SCHEDULED: <2025-11-14 Thu +1w>
```
Repeats exactly 1 week after each completion.

**Next future date** (`++`):
```org
SCHEDULED: <2025-11-14 Thu ++1w>
```
Shifts until next future date (skips past occurrences).

**Plus completion date** (`.+`):
```org
SCHEDULED: <2025-11-14 Thu .+1w>
```
Repeats 1 week after completion date (not original date).

### Time Units

- `h` - Hour
- `d` - Day
- `w` - Week
- `m` - Month
- `y` - Year

**Examples**:
```org
<2025-11-14 Thu +1d>   - Daily
<2025-11-14 Thu +1w>   - Weekly
<2025-11-14 Thu +2w>   - Bi-weekly
<2025-11-14 Thu +1m>   - Monthly
<2025-11-14 Thu +3m>   - Quarterly
<2025-11-14 Thu +1y>   - Yearly
```

## Delay Intervals

**All occurrences** (`-`):
```org
SCHEDULED: <2025-11-14 Thu +1w -2d>
```
Notify 2 days before each occurrence.

**First occurrence** (`--`):
```org
SCHEDULED: <2025-11-14 Thu +1w --2d>
```
Notify only for first occurrence.

## Complete Examples

### Simple Task

```org
* TODO Call plumber
```

### Task with Metadata

```org
* TODO [#A] Finish report :work:urgent:
DEADLINE: <2025-11-17 Sun>
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu 10:30]
  :END:
  Q4 financial report for board meeting.
```

### Project with Subtasks

```org
* PROJECT [#A] Launch product :work:
DEADLINE: <2025-12-31 Sun>
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu]
  :BUDGET: $100000
  :END:
  Launch new product line for Q1 2026.

** TODO [#A] Finalize features
   SCHEDULED: <2025-11-20 Wed>
   :PROPERTIES:
   :CREATED: [2025-11-14 Thu]
   :END:

** TODO Market research
   :PROPERTIES:
   :CREATED: [2025-11-14 Thu]
   :END:

*** TODO Survey customers
*** TODO Analyze competitors

** WAITING Legal review
   :PROPERTIES:
   :CREATED: [2025-11-14 Thu]
   :END:
   Sent to legal on 2025-11-10.

** TODO Create marketing materials
   SCHEDULED: <2025-12-01 Fri>
   :PROPERTIES:
   :CREATED: [2025-11-14 Thu]
   :END:
```

### Recurring Task

```org
* TODO [#B] Weekly review :gtd:
SCHEDULED: <2025-11-15 Fri +1w>
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu]
  :END:
  Review all projects and tasks.
  Update priorities and next actions.
```

## Editing Org Files

### In External Editor

Use any text editor:
```bash
vim ~/tasks/inbox.org
nano ~/tasks/inbox.org
code ~/tasks/inbox.org
```

**Syntax highlighting**: Most editors have Org Mode plugins.

### In Dwayne

Press `Enter` on task to edit.

## Org Mode Compatibility

### Compatible with Emacs

Files created by Dwayne can be opened in Emacs org-mode.

**Shared features**:
- Task hierarchy
- TODO keywords (custom definitions needed in Emacs)
- Timestamps
- Properties
- Tags

**Dwayne-specific**:
- Custom TODO keyword definitions
- Specific workflow (GTD-oriented)

### Portable Format

Files are plain text:
- Edit in any editor
- Version control with Git
- Sync with cloud services
- Read on any device
- Future-proof

## Best Practices

### 1. Consistent Formatting

Keep consistent style:
- Always indent properties/descriptions
- Use standard date format
- Consistent tag naming

### 2. Meaningful Properties

Add properties that help you:
```org
:EFFORT: 2h    - Time estimate
:CONTEXT: home - Where to do it
:ENERGY: low   - Energy level needed
```

### 3. Detailed Descriptions

Future you will thank you:
```org
* TODO Fix bug #1234
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu]
  :END:
  Bug: User login fails on Safari.
  Reproducible: Yes
  Steps:
  1. Go to /login
  2. Enter credentials
  3. Click submit
  4. Error: "Invalid session"

  Potential cause: Cookie settings
  Related: Issue #1200
```

### 4. Link References

Add links for context:
```org
* TODO Review PR
  PR: [[https://github.com/user/repo/pull/123]]
  Related issue: [[https://github.com/user/repo/issues/100]]
```

### 5. Use Tags Strategically

Consistent tag vocabulary:
```org
:work: :personal: :urgent: :someday:
:project_name: :client_name:
:phone: :computer: :online:
```

## File Organization

### Single File

```org
#+TITLE: All Tasks

* INBOX
** INBOX Task 1
** INBOX Task 2

* Projects
** PROJECT Project A
*** TODO Task
** PROJECT Project B
*** TODO Task

* Next Actions
** TODO Action 1
** TODO Action 2
```

### Multiple Files

**inbox.org**:
```org
#+TITLE: Inbox

* INBOX Unprocessed task 1
* INBOX Unprocessed task 2
```

**projects.org**:
```org
#+TITLE: Projects

* PROJECT Project A
** TODO Task
* PROJECT Project B
** TODO Task
```

**next.org**:
```org
#+TITLE: Next Actions

* TODO Action 1
* TODO Action 2
```

## Troubleshooting

### Parsing Errors

**Symptom**: Dwayne fails to load file

**Common causes**:
- Missing space after `*`
- Invalid date format
- Unclosed `:PROPERTIES:` block
- Invalid TODO keyword

**Fix**: Check syntax, look for error line number

### Formatting Issues

**Symptom**: Task displays incorrectly

**Fix**: Ensure:
- Proper indentation
- Valid date formats
- Closed properties blocks
- Tags at end of title line

### Lost Data

**Prevention**:
- Use version control (Git)
- Regular backups
- Test edits in copy first

## Resources

- **Org Mode Manual**: https://orgmode.org/manual/
- **Org Syntax**: https://orgmode.org/worg/dev/org-syntax.html
- **Emacs Org Mode**: https://www.gnu.org/software/emacs/ (optional)

Dwayne uses a subset of Org Mode. Full manual helpful for advanced features.
