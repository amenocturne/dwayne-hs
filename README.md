# Dwayne

A TUI application for a GTD-style workflow using Org Mode files, built with
Haskell.

## Features

- **Org Mode Syntax**: Manages tasks in plain text `.org` files.
- **Vim-like Keybindings**: Navigate and manage tasks efficiently with familiar
  keybindings.
- **GTD Workflow**: Supports keywords like `TODO`, `DONE`, `INBOX`, etc., to
  organize your tasks.
- **Search**: Quickly find tasks with built-in search functionality.
- **External Editor Integration**: Edit tasks in your preferred text editor
  (`$EDITOR`).
- **Undo/Redo**: Safely make changes with undo/redo support.
- **Configurable**: Customize files, colors, and behavior through a `config.yml`
  file.

## Prerequisites

- cabal and GHC.

## Installation

1.  Clone the repository:

    ```sh
    git clone https://github.com/Skril3366/dwayne-hs
    cd dwayne-hs
    ```

2.  Install the executable using Cabal:
    ```sh
    just install
    ```

## Usage

Once installed, you can run the application with:

```sh
dwayne
```

Dwayne will look for a configuration file at `~/.config/dwayne/config.yml`. You
can also specify a custom path using the `DWAYNE_CONFIG` environment variable.

### Configuration

You can configure Dwayne by creating a `config.yml` file. By default, Dwayne
looks for it in `$XDG_CONFIG_HOME/dwayne/config.yml` or
`~/.config/dwayne/config.yml`.

Here is an example configuration:

```yaml
files:
  - ./resources/Sample.org
  - ./resources/SampleInbox.org
inboxFile: ./resources/SampleInbox.org
scrollingMargin: 6
keyTimeoutMs: 1000
autoSave: false
colorScheme: "default"
```

- `files`: A list of Org Mode files to load.
- `inboxFile`: The default file for new tasks.
- `scrollingMargin`: Number of lines from the top/bottom of the screen to start
  scrolling.
- `keyTimeoutMs`: Timeout in milliseconds for multi-key sequences.
- `autoSave`: Automatically save files on changes (e.g., when changing a TODO
  state).
- `colorScheme`: The color scheme to use (currently only "default" is
  supported).

### Keybindings

Dwayne uses Vim-like keybindings for navigation and actions.

#### General

| Key(s)   | Description                          |
| -------- | ------------------------------------ |
| `u`      | Undo last change                     |
| `Ctrl-r` | Redo last change                     |
| `Esc`    | Clear key buffer / Exit command mode |

#### Command Mode (`:`)

| Command | Description                            |
| ------- | -------------------------------------- |
| `:q`    | Quit (if there are no unsaved changes) |
| `:q!`   | Force quit                             |
| `:w`    | Write changes to file(s)               |
| `:w!`   | Force write changes                    |
| `:wq`   | Write and quit                         |

#### Movement

| Key(s)       | Description                        |
| ------------ | ---------------------------------- |
| `j` / `Down` | Move down                          |
| `k` / `Up`   | Move up                            |
| `G`          | Jump to the last task              |
| `gg`         | Jump to the first task             |
| `Ctrl-o`     | Jump back in navigation history    |
| `Tab`        | Jump forward in navigation history |

#### Task Management

| Key(s)  | Description                                             |
| ------- | ------------------------------------------------------- |
| `Enter` | Edit selected task in `$EDITOR`                         |
| `at`    | Add a new task to the inbox file                        |
| `gx`    | Open URL found in task title or description             |
| `t{x}`  | Change TODO state (e.g., `ti` for INBOX, `td` for DONE) |
| `a,{x}` | Add a tag (e.g., `a,m` for `music`)                     |
| `d,{x}` | Delete a tag (e.g., `d,m` for `music`)                  |

#### Modes

| Key(s) | Description        |
| ------ | ------------------ |
| `/`    | Enter search mode  |
| `:`    | Enter command mode |

#### Views

| Key(s)  | Description                                  |
| ------- | -------------------------------------------- |
| ` aa`   | Show all tasks (space + aa)                  |
| ` a{x}` | Filter by TODO state (e.g., ` ai` for INBOX) |

## Development

All the important commands (building, running, testing, profiling) can be found
inside [justfile](./justfile)

## Contributing

Contributions are welcome! Feel free to open an issue or submit a pull request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file
for details.
