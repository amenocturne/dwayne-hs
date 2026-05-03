# Projects & Organization

Guide to managing multi-step projects and organizing hierarchical tasks in Dwayne.

## What is a Project?

In GTD methodology, a **project** is any outcome requiring more than one action step.

In Dwayne, a **PROJECT** is:
- A task with TODO state `PROJECT`
- Usually contains subtasks (child tasks)
- Lives in the projects file
- Has special operations and views

## Creating Projects

### Method 1: Convert Existing Task

**Workflow**:
1. Create or navigate to a task
2. Press `tp` to mark as PROJECT
3. System validates task location
4. If not in projects file, offers to move it

**Example**:
```
Start:
* TODO Organize conference (in inbox.org)

After 'tp':
System: "This PROJECT task should be in projects.org. Move it? (y/n)"
Press 'y':
Task moves to projects.org
```

### Method 2: Create in Editor

**Workflow**:
1. Open projects file in editor
2. Add PROJECT task manually:
   ```org
   * PROJECT Conference planning :work:
     :PROPERTIES:
     :CREATED: [2025-11-14 Thu 10:30]
     :END:
   ** TODO Book venue
   ** TODO Find speakers
   ** TODO Create agenda
   ```
3. Save and reload Dwayne

### Method 3: Add to Inbox, Then Refile

**Workflow**:
1. `at` - Create task in inbox
2. Add "PROJECT" in title temporarily
3. During processing, `tp` to mark as PROJECT
4. System moves to projects file
5. Edit task to add subtasks

## Project Structure

### Basic Project

```org
* PROJECT Project name :tag1:tag2:
  Project description and goals.
** TODO First task
** TODO Second task
** TODO Third task
```

### Complex Project

```org
* PROJECT [#A] Q4 Marketing Campaign :work:marketing:
DEADLINE: <2025-12-31 Sun>
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu 10:30]
  :BUDGET: $10000
  :OWNER: Jane Smith
  :END:
  Complete marketing campaign for Q4 product launch.
  Target: 1000 new customers.

** TODO [#A] Research target audience
   SCHEDULED: <2025-11-15 Fri>
   :PROPERTIES:
   :CREATED: [2025-11-14 Thu 10:31]
   :END:
   Analyze demographics and preferences.

** TODO Create content calendar
   :PROPERTIES:
   :CREATED: [2025-11-14 Thu 10:32]
   :END:

*** TODO Blog posts (10 articles)
*** TODO Social media schedule
*** TODO Email campaign sequence

** WAITING Design approval from leadership
   SCHEDULED: <2025-11-20 Wed>
   :PROPERTIES:
   :CREATED: [2025-11-14 Thu 10:33]
   :END:

** TODO Launch campaign
   SCHEDULED: <2025-12-01 Fri>
   :PROPERTIES:
   :CREATED: [2025-11-14 Thu 10:34]
   :END:
```

**Structure elements**:
- **Level 1**: PROJECT task (the project itself)
- **Level 2**: Main subtasks
- **Level 3+**: Sub-subtasks (as needed)
- **Metadata**: Priority, dates, properties on any task
- **States**: Subtasks can be TODO, DONE, WAITING, etc.

## Viewing Projects

### List All Projects

**Command**: `<space>ap`

**Shows**: All PROJECT tasks (sorted by priority)

**Use**: Get overview of all active projects

### View Single Project

**Command**: `gp` (when on a project or its subtask)

**Shows**:
- Project task itself (always first)
- All subtasks (direct and nested)
- Custom sorting within project

**Use**: Focus on one project's tasks

**Example**:
```
Navigate to any task in "Marketing Campaign" project
Press 'gp'

View shows:
1. PROJECT Q4 Marketing Campaign (the project)
2. TODO Research target audience (subtask)
3. TODO Create content calendar (subtask)
4. TODO Blog posts (sub-subtask)
5. TODO Social media schedule (sub-subtask)
... etc
```

### Navigate Project Hierarchy

From project view (`gp`):
- `j`/`k` - Navigate through tasks
- `gp` on subtask - View that task's project
- `<space>aa` - Return to all tasks
- `<space>ap` - Return to all projects

## Adding Tasks to Projects

### Method 1: Refile Command

**Command**: `gr` (refile)

**Workflow**:
1. Navigate to task to move (anywhere)
2. Press `gr`
3. Refile dialog shows all projects
4. Navigate with `j`/`k`
5. Press `Enter` to select project
6. Task moves under project as subtask

**Example**:
```
Current view:
* TODO Research competitors (in inbox.org)

Press 'gr':
Refile dialog opens with:
- PROJECT Marketing Campaign
- PROJECT Website Redesign
- PROJECT Hire Developer
... etc

Navigate to "Marketing Campaign", press Enter:

Result (in projects.org):
* PROJECT Marketing Campaign
** TODO Research target audience
** TODO Create content calendar
** TODO Research competitors (moved here!)
```

**Level adjustment**:
Task level adjusts to fit under project:
- Project at level 1
- Moved task becomes level 2
- If moved task had subtasks, they become level 3+

### Method 2: Direct Edit

**Workflow**:
1. Navigate to project
2. Press `Enter` to edit
3. Add subtask manually:
   ```org
   * PROJECT My project
   ** TODO Existing subtask
   ** TODO New subtask (add this line)
   ```
4. Save and close

### Method 3: Create in Project File

**Workflow**:
1. Open projects.org in external editor
2. Navigate to project section
3. Add subtask under project
4. Ensure level is higher than project (project = `*`, subtask = `**`)
5. Save

## Managing Project Tasks

### Change Subtask State

Navigate to subtask in project view, use normal state commands:
- `tt` - Mark as TODO
- `td` - Mark as DONE
- `tw` - Mark as WAITING
- etc.

### Set Subtask Priority

Navigate to subtask:
- `Shift-Up` - Increase priority
- `Shift-Down` - Decrease priority

### Add Tags to Subtask

Navigate to subtask:
- `a,m` - Add music tag
- Or edit task and add tags manually

### Schedule Subtask

Navigate to subtask, press `Enter`, edit:
```org
** TODO Subtask
   SCHEDULED: <2025-11-20 Wed>
```

### Complete Subtask

Navigate to subtask, press `td`.

**Project stays active** until you explicitly mark it DONE.

### Delete Subtask

Navigate to subtask, press `tx` (mark as TRASH).

Or edit project and remove the line.

## Project Workflows

### Starting a New Project

1. **Capture the idea**:
   ```
   at → "Organize team offsite"
   ```

2. **Mark as project**:
   ```
   Navigate to task → tp (mark as PROJECT)
   System moves to projects.org
   ```

3. **Break down into tasks**:
   ```
   Press Enter (edit)
   Add:
   * PROJECT Organize team offsite
   ** TODO Choose location
   ** TODO Set date
   ** TODO Book venue
   ** TODO Plan activities
   ** TODO Send invitations
   Save and close
   ```

4. **Identify next action**:
   ```
   gp (view project)
   Navigate to "Choose location"
   Shift-Up (set priority A)
   tt (mark as TODO if not already)
   ```

5. **Work the project**:
   ```
   Daily: gp to view project
   Complete tasks: td
   Add new tasks as discovered: Enter, edit, add lines
   ```

### Completing a Project

**Option 1: Mark Done When All Subtasks Complete**
```
1. gp (view project)
2. Verify all subtasks are DONE
3. Navigate to project task itself
4. td (mark PROJECT as DONE)
```

**Option 2: Archive Subtasks, Then Mark Done**
```
1. gp (view project)
2. V (selection mode)
3. Select all DONE subtasks
4. tx (mark all as TRASH)
5. Navigate to project
6. td (mark PROJECT as DONE)
```

**Option 3: Move to Archive File**
```
1. Edit projects.org
2. Cut entire project (including subtasks)
3. Open archive.org
4. Paste project
5. Save both files
```

### Pausing a Project

**Mark as SOMEDAY**:
```
Navigate to project task → ts
```

**Or mark as WAITING** (if blocked):
```
Navigate to project task → tw
Edit description to note what you're waiting for
```

**Reactivate later**:
```
<space>as (view SOMEDAY projects)
Navigate to project → tp (back to PROJECT)
Or → tt if you want to work on it now
```

## Project Best Practices

### 1. One Next Action Always

Every project should have at least one TODO subtask.

During weekly review:
```
<space>ap (view all projects)
gp on each project
Verify at least one TODO exists
If all DONE, add next task or complete project
```

### 2. Use Subtask Hierarchy

Break complex tasks into smaller pieces:
```org
* PROJECT Launch product
** TODO Marketing
*** TODO Social media campaign
**** TODO Create posts
**** TODO Schedule posts
*** TODO Email announcement
** TODO Sales
*** TODO Contact top customers
*** TODO Prepare sales deck
```

### 3. Schedule Regular Reviews

Weekly project review:
```
1. <space>ap (all projects)
2. For each project: gp, review progress
3. Update next actions
4. Adjust priorities
5. Archive completed projects
```

### 4. Link Related Tasks

Add links in descriptions:
```org
* PROJECT Website Redesign
** TODO Update homepage
   See mockup: [[file:~/docs/homepage-mockup.png]]
   Related: [[https://figma.com/project123]]
```

### 5. Track Project Metadata

Use properties for project tracking:
```org
* PROJECT [#A] Client project :work:
  :PROPERTIES:
  :CREATED: [2025-11-14 Thu]
  :DEADLINE: 2025-12-31
  :BUDGET: $50000
  :CLIENT: Acme Corp
  :CONTACT: john@acme.com
  :STATUS: In Progress
  :COMPLETION: 60%
  :END:
```

### 6. Separate Active and Someday

Keep active projects in PROJECT state.
Move inactive to SOMEDAY.
Archive completed to DONE or separate file.

### 7. Limit Active Projects

Focus on 3-7 active projects max.
More than that, move extras to SOMEDAY.

## Project Validation

### Automatic Validation

Dwayne checks:
- PROJECT tasks should be in projects file
- If not, offers to move them

**Example**:
```
Create PROJECT task in inbox.org
On startup or after 'tp':
Dialog: "Found PROJECT task in wrong file. Move to projects.org?"
Accept: Task moves automatically
Reject: Task stays (but you'll be warned again)
```

### Manual Validation

Check project structure yourself:
```
1. <space>ap (view all projects)
2. Verify each project has subtasks
3. gp on each to see structure
4. Ensure no orphaned subtasks
```

## Moving Projects Between Files

Not recommended, but possible:

**Workflow**:
1. Edit source file (e.g., projects.org)
2. Cut entire project section (project + all subtasks)
3. Edit destination file
4. Paste project
5. Adjust levels if needed
6. Save both files
7. Restart Dwayne

**Better approach**: Use refile (`gr`) to move individual tasks.

## Archiving Projects

### Method 1: Mark as DONE

```
Navigate to completed project → td
Later: <space>ad (view DONE)
Periodically: Edit file, delete DONE projects
```

### Method 2: Move to Archive File

```
1. Edit projects.org
2. Cut completed project (with subtasks)
3. Edit archive.org
4. Paste at bottom
5. Save both
```

### Method 3: Mark as TRASH

```
Navigate to project → tx
Later: <space>ax (view TRASH)
Periodically: Edit file, delete TRASH projects
```

## Troubleshooting

### Project Not Showing in `gp` View

**Cause**: Not on a project task or its subtask

**Solution**: Navigate to any task within the project first, then `gp`

### Refile Dialog Empty

**Cause**: No PROJECT tasks exist

**Solution**: Create at least one PROJECT task first

### Subtasks Not Moving with Project

**Cause**: Manual move without selecting subtasks

**Solution**: When editing files, cut the entire section (project + all subtasks)

### Project in Wrong File

**Cause**: Created outside projects file

**Solution**:
1. Press `tp` on the task
2. System will detect and offer to move
3. Accept the move

Or manually move via file editing.

## Advanced Project Patterns

### Multi-Phase Projects

```org
* PROJECT [#A] Product Launch
** PROJECT Phase 1: Development
*** TODO Design features
*** TODO Implement core
*** TODO Testing
** PROJECT Phase 2: Marketing
*** TODO Create materials
*** TODO Launch campaign
** PROJECT Phase 3: Support
*** TODO Setup help desk
*** TODO Train support team
```

Each phase can be viewed independently with `gp`.

### Recurring Projects

```org
* PROJECT [#B] Monthly report :work:
  Template for monthly reports.
** TODO Gather data
** TODO Analyze trends
** TODO Write summary
** TODO Submit to management

(Mark all subtasks DONE, then WAITING/TODO to "reset" for next month)
```

### Collaborative Projects

```org
* PROJECT Team hackathon :work:team:
** TODO Setup repository (assigned: Alice)
** TODO Define API spec (assigned: Bob)
** WAITING Design review (blocked on: Carol)
** TODO Write tests (assigned: Me)
```

Use properties for assignment:
```org
** TODO Setup repository
   :PROPERTIES:
   :ASSIGNED: Alice
   :END:
```

## Tips for Project Management

1. **Start small**: Begin with 2-3 active projects
2. **Regular reviews**: Weekly check on all projects
3. **Clear next actions**: Always know what's next
4. **Use priorities**: Focus on high-priority projects
5. **Archive completed**: Keep active list clean
6. **Link resources**: Add URLs and file references
7. **Track time**: Use properties for estimates and actuals
8. **Stay flexible**: Restructure as project evolves
9. **Celebrate progress**: Review DONE subtasks regularly
10. **Learn and iterate**: Reflect on completed projects
