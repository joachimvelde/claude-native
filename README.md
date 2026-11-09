# claude-native

Claude Code in an Emacs buffer. It runs `claude` headless and draws the
session the way the terminal UI does.

## Dependencies

- **Emacs 30.1+**. No other packages; evil is supported if installed.
- **[Claude Code](https://docs.claude.com/en/docs/claude-code)**, logged in, with `claude` on your PATH.
- **macOS `sips`** (optional): converts HEIC and large images before attaching.
- **Python 3** (tests only).

## Install

```sh
git clone https://github.com/joachimvelde/claude-native
```

```elisp
(add-to-list 'load-path "/path/to/claude-native")
(require 'claude-native)
(keymap-global-set "C-c c" #'claude-native) ; optional
```

## Commands

| Command | |
|---|---|
| `claude-native` | open this project's session (`C-u`: resume the last conversation) |
| `claude-native-continue` | resume the last conversation |
| `claude-native-new` | start another session for this project |
| `claude-native-resume` | pick a past session to resume |
| `claude-native-dired-attach` | attach the marked files in dired |

## Keys

| Key | |
|---|---|
| `RET` | send; on `… +N lines`, expand |
| `S-RET`, `C-j` | new line |
| `TAB` | accept the suggested prompt, or complete `/commands` and `@files` |
| `M-p` / `M-n` | previous / next prompt |
| `S-TAB` | cycle permission mode |
| `C-c C-c` | interrupt |
| `C-c C-a` | attach a file |
| `C-c C-m` | set model and effort |
| `C-c C-e` | set effort |
| `C-c /` | run a slash command from a list |
| `C-c C-p` | answer a postponed permission request |
| `C-c C-k` | end the session |
| `C-c C-a` in dired | attach the marked files |

`/resume`, `/model`, `/effort`, `/clear` and `/exit` open Emacs prompts.
Other slash commands go to Claude Code as typed.

## Attaching files

Images show in the input as `[Image #N]` and PDFs as `[PDF #N]`. They are sent
with the prompt on `RET`; delete the marker to drop the file. Other files are
added as `@path`. On macOS, HEIC and images over 3.75 MB are converted to JPEG.

## Options

| Option | Default |
|---|---|
| `claude-native-permission-mode` | `"bypassPermissions"` |
| `claude-native-model` | `nil` (Claude Code's default) |
| `claude-native-program` | `claude` on PATH |
| `claude-native-extra-args` | `nil` |
| `claude-native-permission-function` | ask in the minibuffer |
| `claude-native-image-max-bytes` | `3750000` |

## Tests

Most tests replay a recorded session; record one with `dev/record.py`.

```sh
emacs --batch -L . -l test/claude-native-test.el -f ert-run-tests-batch-and-exit
```

## License

GPL-3.0-or-later.
