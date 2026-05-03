# Configuration Guide

Complete guide to configuring Dwayne for your workflow.

## Configuration File

### Location

Default location:
```
~/.config/dwayne/config.yml
```

Override with environment variable:
```bash
DWAYNE_CONFIG=/path/to/config.yml dwayne
```

### Format

YAML format with the following structure:

```yaml
files:
  - /path/to/file1.org
  - /path/to/file2.org
inboxFile: /path/to/inbox.org
projectsFile: /path/to/projects.org
scrollingMargin: 6
keyTimeoutMs: 1000
colorScheme: "default"
```

## Configuration Options

### files
**Type**: List of file paths
**Required**: Yes
**Description**: List of Org Mode files to load and manage.

**Example**:
```yaml
files:
  - ~/tasks/inbox.org
  - ~/tasks/projects.org
  - ~/tasks/work.org
  - ~/tasks/personal.org
```

**Notes**:
- All files must exist before starting Dwayne
- Files can be relative or absolute paths
- Inbox and projects files will be automatically included even if not listed

### inboxFile
**Type**: File path
**Required**: Yes
**Description**: File where new tasks are created (`at` command).

**Example**:
```yaml
inboxFile: ~/tasks/inbox.org
```

**Notes**:
- Should match one of the files in the `files` list
- This is where all new captures go
- Keep this file lightweight for fast processing

### projectsFile
**Type**: File path
**Required**: Yes
**Description**: File where PROJECT tasks should be stored.

**Example**:
```yaml
projectsFile: ~/tasks/projects.org
```

**Notes**:
- System validates PROJECT tasks are in this file
- Offers to move misplaced PROJECT tasks here
- Should contain only PROJECT tasks and their subtasks

### scrollingMargin
**Type**: Integer
**Required**: No
**Default**: 6
**Description**: Number of lines from viewport edge before scrolling.

**Example**:
```yaml
scrollingMargin: 6
```

**Notes**:
- Higher values = cursor stays more centered
- Lower values = more screen real estate, cursor near edges
- Typical range: 3-10

### keyTimeoutMs
**Type**: Integer (milliseconds)
**Required**: No
**Default**: 1000
**Description**: Timeout for multi-key sequences.

**Example**:
```yaml
keyTimeoutMs: 1000
```

**Notes**:
- How long to wait for next key in sequence (e.g., `sca`)
- Lower values = faster but easier to miss sequences
- Higher values = easier to complete sequences but slower
- Typical range: 500-2000 ms

### colorScheme
**Type**: String
**Required**: No
**Default**: "default"
**Description**: Color theme to use.

**Example**:
```yaml
colorScheme: "default"
```

**Notes**:
- Currently only "default" (Catppuccin Mocha) is implemented
- Framework supports custom schemes (requires code modification)

## Environment Variables

### DWAYNE_CONFIG
Override configuration file location:
```bash
DWAYNE_CONFIG=~/my-config.yml dwayne
```

### EDITOR
External editor for task editing:
```bash
export EDITOR=nvim
# or
export EDITOR="code --wait"
```

**Supported editors**:
- Terminal: vim, nvim, emacs, nano, micro
- GUI: VS Code, Sublime Text (with `--wait` flag)

**Requirements**:
- Must be a blocking command (waits for editing to complete)
- For GUI editors, use wait flag (e.g., `code --wait`)

## Sample Configurations

### Minimal Configuration
```yaml
files:
  - ~/tasks/tasks.org
inboxFile: ~/tasks/tasks.org
projectsFile: ~/tasks/tasks.org
```

Single file for everything. Good for getting started.

### GTD Setup
```yaml
files:
  - ~/gtd/inbox.org
  - ~/gtd/projects.org
  - ~/gtd/next.org
  - ~/gtd/someday.org
  - ~/gtd/reference.org
inboxFile: ~/gtd/inbox.org
projectsFile: ~/gtd/projects.org
scrollingMargin: 8
keyTimeoutMs: 1200
colorScheme: "default"
```

Dedicated files for different GTD contexts.

### Work/Personal Split
```yaml
files:
  - ~/tasks/inbox.org
  - ~/tasks/projects.org
  - ~/work/tasks.org
  - ~/work/projects.org
  - ~/personal/tasks.org
inboxFile: ~/tasks/inbox.org
projectsFile: ~/tasks/projects.org
scrollingMargin: 6
keyTimeoutMs: 1000
colorScheme: "default"
```

Separate work and personal tasks in different directories.

### Developer Setup
```yaml
files:
  - ~/tasks/inbox.org
  - ~/tasks/projects.org
  - ~/dev/open-source.org
  - ~/dev/learning.org
  - ~/dev/bugs.org
inboxFile: ~/tasks/inbox.org
projectsFile: ~/tasks/projects.org
scrollingMargin: 10
keyTimeoutMs: 800
colorScheme: "default"
```

Development-focused with separate files for different activities.

## File Organization Strategies

### Strategy 1: Single File
**Use when**: Just getting started, simple needs

**Structure**:
```
tasks.org
  - All tasks in one file
  - Simple to manage
  - Easy to search
```

**Pros**: Simplest setup, everything in one place
**Cons**: Can get crowded with many tasks

### Strategy 2: GTD Split
**Use when**: Following GTD methodology strictly

**Structure**:
```
inbox.org       - Capture area
projects.org    - Active projects
next.org        - Next actions (TODO)
someday.org     - Someday/maybe
reference.org   - Notes and reference
```

**Pros**: Clear separation of concerns
**Cons**: More files to manage

### Strategy 3: Context-Based
**Use when**: Different areas of life with different contexts

**Structure**:
```
inbox.org       - Capture
projects.org    - All projects
work.org        - Work tasks
personal.org    - Personal tasks
learning.org    - Learning and development
```

**Pros**: Easy to focus on one area
**Cons**: Projects might span multiple files

### Strategy 4: Archive-Based
**Use when**: Want to keep completed tasks separate

**Structure**:
```
inbox.org       - Capture
active.org      - Current work
projects.org    - Active projects
archive.org     - Completed/trashed tasks
```

**Pros**: Keep active files lean
**Cons**: Need to manually archive (move DONE/TRASH to archive.org)

## Configuration Workflow

### Initial Setup

1. **Create config directory**:
   ```bash
   mkdir -p ~/.config/dwayne
   ```

2. **Create task directories**:
   ```bash
   mkdir -p ~/tasks
   ```

3. **Create initial files**:
   ```bash
   touch ~/tasks/inbox.org
   touch ~/tasks/projects.org
   ```

4. **Create config file**:
   ```bash
   cat > ~/.config/dwayne/config.yml << EOF
   files:
     - ~/tasks/inbox.org
     - ~/tasks/projects.org
   inboxFile: ~/tasks/inbox.org
   projectsFile: ~/tasks/projects.org
   scrollingMargin: 6
   keyTimeoutMs: 1000
   colorScheme: "default"
   EOF
   ```

5. **Test configuration**:
   ```bash
   dwayne
   ```

### Adjusting Configuration

#### Add More Files
Edit config, add to `files` list:
```yaml
files:
  - ~/tasks/inbox.org
  - ~/tasks/projects.org
  - ~/tasks/new-file.org  # New file
```

Create the file:
```bash
touch ~/tasks/new-file.org
```

Restart Dwayne.

#### Change Scrolling Behavior
Prefer cursor more centered:
```yaml
scrollingMargin: 10
```

Prefer more visible tasks:
```yaml
scrollingMargin: 3
```

#### Adjust Key Timeout
Slower typist:
```yaml
keyTimeoutMs: 1500
```

Fast typist:
```yaml
keyTimeoutMs: 700
```

## Troubleshooting

### Config Not Found
**Error**: "Config file not found"

**Solution**:
1. Check file exists: `ls ~/.config/dwayne/config.yml`
2. Create if missing (see Initial Setup)
3. Use `DWAYNE_CONFIG` to specify alternative location

### Invalid YAML
**Error**: "Parse error in config"

**Solution**:
1. Validate YAML syntax (use online validator)
2. Check indentation (use spaces, not tabs)
3. Quote strings with special characters

### File Not Found
**Error**: "File does not exist: /path/to/file.org"

**Solution**:
1. Create missing file: `touch /path/to/file.org`
2. Check path is correct (absolute vs relative)
3. Verify permissions (readable/writable)

### Editor Not Found
**Error**: "Editor not found" or editor doesn't open

**Solution**:
1. Set EDITOR: `export EDITOR=vim`
2. Add to shell profile (`~/.bashrc`, `~/.zshrc`):
   ```bash
   export EDITOR=vim
   ```
3. For GUI editors, use wait flag:
   ```bash
   export EDITOR="code --wait"
   ```

## Advanced Configuration

### Custom Color Schemes
Currently requires code modification. See [Development Guide](./development.md).

To add a new color scheme:
1. Edit `src/Tui/ColorScheme.hs`
2. Define new color scheme
3. Update config parsing to recognize new name
4. Rebuild and reinstall

### Custom Keybindings
Requires code modification. See [Development Guide](./development.md).

To customize keybindings:
1. Edit `src/Tui/Keybindings.hs`
2. Modify `handleKeyPress` function
3. Rebuild and reinstall

### Multiple Configurations
Use shell aliases for different contexts:

```bash
# In ~/.bashrc or ~/.zshrc
alias dwayne-work='DWAYNE_CONFIG=~/.config/dwayne/work.yml dwayne'
alias dwayne-personal='DWAYNE_CONFIG=~/.config/dwayne/personal.yml dwayne'
```

Then use:
```bash
dwayne-work      # Uses work config
dwayne-personal  # Uses personal config
```

## Version Control

Track your configuration and tasks with Git:

```bash
cd ~/tasks
git init
git add .
git commit -m "Initial commit"

# Add .gitignore for sensitive files if needed
echo "secrets.org" > .gitignore
```

Config file can also be versioned:
```bash
cd ~/.config/dwayne
git init
git add config.yml
git commit -m "Dwayne configuration"
```

## Backup Strategy

### Local Backups
```bash
# Daily backup script
#!/bin/bash
tar -czf ~/backups/tasks-$(date +%Y%m%d).tar.gz ~/tasks/
```

### Cloud Sync
Use Git with remote:
```bash
cd ~/tasks
git remote add origin https://github.com/yourusername/tasks.git
git push -u origin main
```

Or use Dropbox/Google Drive:
```yaml
files:
  - ~/Dropbox/tasks/inbox.org
  - ~/Dropbox/tasks/projects.org
```

### Automatic Commits
Git hook for auto-commit on save (advanced):
```bash
# In ~/tasks/.git/hooks/post-commit
#!/bin/bash
git push origin main
```
