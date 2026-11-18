# agent-native

Coding agents in an Emacs buffer: **Claude Code** and **Codex**. The agent
runs headless and the session is drawn here, the way Claude Code's terminal UI
draws it. Both agents get the same buffer, keys, commands and popups.

## Dependencies

- **Emacs 30.1+**. No other packages; evil is supported if installed.
- One or both agents, logged in:
  - **[Claude Code](https://docs.claude.com/en/docs/claude-code)**, with `claude` on your PATH.
  - **[Codex](https://github.com/openai/codex)**, with `codex` on your PATH. It runs as `codex app-server`.
- **macOS `sips`** (optional): converts HEIC and large images before attaching.
- **Python 3** (tests only).

## Install

```sh
git clone https://github.com/joachimvelde/agent-native
```

```elisp
(add-to-list 'load-path "/path/to/agent-native")
(require 'agent-native)
(keymap-global-set "C-c c" #'agent-native) ; optional
```

## Commands

| Command | |
|---|---|
| `agent-native` | open this project's session: the one used last, else a new one with `agent-native-backend` (`C-u`: resume the last conversation) |
| `agent-native-claude` | open this project's Claude Code session |
| `agent-native-codex` | open this project's Codex session |
| `agent-native-continue` | resume the last conversation |
| `agent-native-new` | start another session for this project (`C-u`: pick the agent) |
| `agent-native-choose-model` | pick a provider (Claude or OpenAI), then a model, then its effort |
| `agent-native-resume` | pick a past session to resume, from both agents |
| `agent-native-usage` | show plan limits, context use and what the session used |
| `agent-native-agents` | list the sub-agents at work |
| `agent-native-toggle-remote-control` | Claude only: turn Remote Control on; when on: open or copy its link, or turn it off |
| `agent-native-dired-attach` | attach the marked files in dired |

Session buffers are named after their agent: `*claude: ~/project*`,
`*codex: ~/project*`. When a session's agent has ended (`C-c C-k`, a crash),
opening the session again carries on with its conversation, with the same
model, effort and permission mode; `/clear` keeps them too. A session resumed
from the list starts with the agent's default model and effort.

## Keys

| Key | |
|---|---|
| `RET` | send; on `… +N lines`, expand; on a link, open it |
| `S-RET`, `C-j` | new line |
| `TAB` | accept the suggested prompt, or complete `/commands` and `@files` |
| `↑` / `↓` | previous / next prompt, from the input's first / last line |
| `M-p` / `M-n` | previous / next prompt |
| `S-TAB` | cycle permission mode |
| `C-c C-c` | interrupt |
| `C-c C-a` | attach a file |
| `C-c C-m` | pick a provider (Claude or OpenAI), a model and its effort |
| `C-c C-e` | set effort |
| `C-c /` | run a slash command from a list |
| `C-c C-p` | answer a postponed permission request |
| `C-c C-t` | list the sub-agents at work |
| `C-c C-r` | Remote Control (Claude) |
| `C-c C-k` | end the session |
| `C-c C-a` in dired | attach the marked files |

These slash commands are done in Emacs, the same for both agents:

- `/resume`, `/model`, `/effort`, `/clear`, `/exit` open Emacs prompts.
  `/model NAME` and `/effort LEVEL` set them directly.
- `/model` (or `C-c C-m`) asks for the provider first, Claude or OpenAI,
  then the model, then its effort. Picking this session's provider changes
  its model. Picking the other one opens this project's session with that
  agent (starting it if needed) and sets the model there: a conversation
  cannot move between Claude Code and Codex, so this one stays in its buffer.
  It also works from any buffer, for the project you are in.
- `/usage` (also `/cost`, `/stats`) shows plan limits and context use as bars,
  with what the session used, in a popup below; `q` closes it.
- `/tasks` (or `C-c C-t`) lists the sub-agents at work in a popup below: each
  with what it does now, then its model, effort, tool uses, tokens and time so
  far, updated every second. `RET` goes to the agent's call in the session.
  The status line shows how many are at work.

Other slash commands go to the agent. With Codex, `/compact` compacts the
conversation and each skill is a command: `/skill-name what to do`.

A permission question takes `y` (yes), `a` (always) or `n` (no, which also
stops the turn); `?` says what "always" keeps, which the agent decides. Claude
may switch the mode or keep a rule in the project's `.claude/settings.local.json`.
Codex keeps a command rule for good, in every project (in `~/.codex/rules`).
`C-g` leaves the question for later: `C-c C-p` asks it again.

## Claude and Codex

| | Claude Code | Codex |
|---|---|---|
| Runs as | `claude -p` with stream-json | `codex app-server` (JSON-RPC) |
| Permission modes (`S-TAB`) | default, accept edits, plan, bypass | default (asks before commands), auto, plan (read-only, plan mode), full access |
| Model and effort | change at once | change with the next turn |
| `/usage` | plan limits, context, cost, what used the plan most | plan limits, context, tokens, credits, recent days |
| Sub-agents | Agent calls | spawned agents |
| A prompt sent while it works | taken into the running turn at its next step | joins the running turn |
| Next-prompt suggestion | yes | when Codex offers it (not yet, as of 0.170) |
| Remote Control | yes | no |

Tool calls are drawn the same way: `Shell(…)`, `Read(…)`, `Edit(…)` with a
diff, `Write(…)`, a plan as a todo list.

## Remote Control (Claude)

A Claude session can be followed and steered from claude.ai and the Claude
app: what happens in Emacs shows there, and a prompt sent there runs here and
is drawn here. Sessions start with it when Claude Code is set to ("Enable
Remote Control for all sessions" in its `/config`), or always or never with
`agent-native-remote-control`. `⇄ remote` before the model name says it is
connected. `C-c C-r` (or `/remote-control`, `/rc`) turns it on; when it is on,
it offers to open the session's page, copy its link, or turn it off.
Ending a session (`C-c C-k`, `/exit`, `/clear`, killing the buffer) archives it
on claude.ai.

## Attaching files

Images show in the input as `[Image #N]` and PDFs as `[PDF #N]`. They are sent
with the prompt on `RET`; delete the marker to drop the file. Other files are
added as `@path`. On macOS, HEIC and images over 3.75 MB are converted to JPEG.
Codex gets images as images and PDFs as paths to read.

## Org

```elisp
(setq agent-native-format 'org)
```

The agent is told to write its replies in Org (`agent-native-org-prompt`,
added to its instructions), and its text is drawn with your Org setup:
headings, aligned tables, highlighted source blocks, links that `RET` or a
click opens. Copied text is plain Org. Files, commits and tool inputs keep
their own formats. Replies written in Markdown (older sessions, slash command
output) are still drawn as Markdown.

Tool calls and their output are dim, so the agent's text stands out; the faces
are `agent-native-tool-name`, `agent-native-tool-summary` and
`agent-native-tool-result`.

## Options

| Option | Default |
|---|---|
| `agent-native-backend` | `claude`: the agent a project's first session uses |
| `agent-native-permission-mode` | `bypass`; also `default`, `accept-edits`, `plan` |
| `agent-native-claude-model` | `nil` (Claude Code's default) |
| `agent-native-codex-model` | `nil` (Codex's default) |
| `agent-native-claude-program` | `claude` on PATH |
| `agent-native-codex-program` | `codex` on PATH |
| `agent-native-claude-extra-args` | `nil` |
| `agent-native-codex-extra-args` | `nil`, e.g. `("-c" "model_verbosity=\"low\"")` |
| `agent-native-permission-function` | ask in the minibuffer |
| `agent-native-image-max-bytes` | `3750000` |
| `agent-native-notify` | `t`: echo-area message when a hidden session finishes |
| `agent-native-remote-control` | `auto`: start under Remote Control when Claude Code is set to; `t` always, `nil` never |
| `agent-native-format` | `markdown`; `org` has the agent write Org, drawn as in your Org buffers |
| `agent-native-input-padding` | `0`: empty space above and below the input, in lines |

## Adding a feature

`agent-native-backend.el` is the contract between the session buffer and the
agents. The buffer calls operations (`agent-native-backend-send`, `-usage`,
`-set-model`, …) and draws events (`tool`, `text-delta`, `turn-end`, …). Each
backend file (`agent-native-claude.el`, `agent-native-codex.el`) implements the
operations and turns its agent's messages into events. A feature is written
once in the buffer, plus a small method in each backend; one an agent lacks
falls back to the generic function's default, such as "not available with
Codex".

## Tests

Most tests replay recorded sessions; record them with `dev/record-claude.py`
and `dev/record-codex.py`. Recordings hold account details and stay out of git.

```sh
emacs --batch -L . -l test/agent-native-test.el -f ert-run-tests-batch-and-exit
```

`dev/live/` runs every command against the real `claude` and `codex`, in a GUI
Emacs, in different orders (before the agent is ready, mid-turn, after a crash,
switching between the two). It uses your plan and leaves sessions behind; how
to run it is at the top of `dev/live/harness.el`.

## License

GPL-3.0-or-later.
