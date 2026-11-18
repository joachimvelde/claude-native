;;; agent-native.el --- coding agents (Claude Code, Codex), drawn natively in Emacs -*- lexical-binding: t -*-

;; Version: 0.2.0
;; URL: https://github.com/joachimvelde/agent-native
;; Keywords: tools, ai
;; Package-Requires: ((emacs "30.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; `M-x agent-native' opens a coding agent session for the current project:
;; the transcript on top, an input area at the bottom, and a status line,
;; like Claude Code's terminal UI. The agent runs headless and everything
;; is drawn here, the same for each backend: Claude Code (`claude') and
;; Codex (`codex app-server'). `agent-native-claude' and
;; `agent-native-codex' pick one; `agent-native-backend' is the default.
;;
;; In the input area:
;;   RET          send (S-RET or C-j for a new line); on a link, open it
;;   C-c C-c      interrupt the agent
;;   S-TAB        cycle permission mode
;;   C-c C-m      pick a provider (Claude or OpenAI), a model, its effort
;;   C-c C-e      pick an effort level
;;   M-p / M-n    earlier / later prompts
;;   TAB          complete /commands and @files
;;   C-c C-p      answer a permission request you put off
;;   C-c /        run a slash command, picked from a list
;;   C-c C-t      list the sub-agents at work
;;
;; /resume, /model, /effort and /exit open native pickers instead of being
;; sent; /usage shows plan limits, context and cost as bars, in a popup;
;; /tasks lists the sub-agents at work, in a popup.
;;
;; Files: agent-native-backend.el (what a backend is: operations and
;; events), agent-native-claude.el and agent-native-codex.el (the
;; backends), agent-native-render.el (drawing), agent-native-usage.el (the
;; /usage report), agent-native-agents.el (the sub-agents at work),
;; agent-native-attach.el (files sent with a prompt). Tests in test/,
;; recordings of real sessions in dev/.

;;; Code:

(require 'agent-native-backend)
(require 'agent-native-claude)
(require 'agent-native-codex)
(require 'agent-native-render)
(require 'agent-native-attach)
(require 'agent-native-usage)
(require 'agent-native-agents)
(require 'project)

(defcustom agent-native-backend 'claude
  "The backend `agent-native' starts when this project has no session yet."
  :type '(choice (const :tag "Claude Code" claude) (const :tag "Codex" codex))
  :group 'agent-native)

(defcustom agent-native-permission-mode 'bypass
  "Permission mode sessions start in.
`bypass' runs every tool without asking; `default' asks before most;
`accept-edits' edits files without asking; `plan' plans before it acts
\(Codex: read-only, with its plan mode). S-TAB cycles through them."
  :type '(choice (const bypass) (const default) (const accept-edits) (const plan))
  :group 'agent-native)

(defcustom agent-native-notify t
  "Say in the echo area when a turn ends in a session you can't see."
  :type 'boolean
  :group 'agent-native)

(defcustom agent-native-permission-function #'agent-native--ask-permission
  "Function deciding a permission request.
Called with the request, a plist with :name (the tool), :input and
:tool-id; returns one of `allow', `always', `deny'."
  :type 'function
  :group 'agent-native)

;;;; Session state (buffer-local)

(defvar-local agent-native--backend nil "The backend: `claude' or `codex'.")
(defvar-local agent-native--conn nil "The connection to the agent.")
(defvar-local agent-native--input nil "Marker at the start of the input area.")
(defvar-local agent-native--busy nil "Non-nil while the agent works: when it started.")
(defvar-local agent-native--status "" "What the agent is doing, for the status line.")
(defvar-local agent-native--model nil "The model in use.")
(defvar-local agent-native--effort nil "The effort level in use.")
(defvar-local agent-native--mode agent-native-permission-mode "The permission mode.")
(defvar-local agent-native--cost 0 "Total cost in USD so far.")
(defvar-local agent-native--session-id nil "The session's id, for resuming.")
(defvar-local agent-native--remote nil
  "Remote Control's state as the agent reports it (\"connected\"), nil when off.")
(defvar-local agent-native--remote-url nil "The session's page on claude.ai.")
(defvar-local agent-native--commands nil "Slash commands: name -> description.")
(defvar-local agent-native--command-hints nil
  "Slash commands that take arguments: name -> hint (e.g. \"<file>\").")
(defvar-local agent-native--models nil
  "Models the agent offers: plists (:value :aliases :name :description :efforts).")
(defvar-local agent-native--pending nil "Permission requests not yet answered.")
(defvar-local agent-native--history nil "Prompts sent, newest first.")
(defvar-local agent-native--history-index -1 "Where M-p/M-n are in the history.")
(defvar-local agent-native--history-draft "" "The unsent input, kept while going through history.")
(defvar-local agent-native--spinner nil "Timer animating the status line.")
(defvar-local agent-native--thinking-tokens 0 "Estimated thinking tokens this turn.")
(defvar-local agent-native--interrupted nil "Non-nil after C-c C-c, until the turn ends.")
(defvar-local agent-native--queued nil
  "Prompts sent but not yet taken up by the agent (it echoes each when it does).")
(defvar-local agent-native--drawn nil
  "Prompts drawn when sent (the agent was idle), whose echo is then skipped.")
(defvar-local agent-native--suggestion nil
  "The agent's guess at your next prompt, shown dim in the empty input.")
(defvar-local agent-native--asking nil "Non-nil while a permission question is open.")
(defvar-local agent-native--waiting-notes nil
  "Request id -> region of its \"Waiting for permission…\" note.")

(defconst agent-native--spinner-frames ["✻" "✽" "✶" "✳" "✢" "·" "✢" "✳" "✶" "✽"])

(defun agent-native--name ()
  "This session's agent, by name: \"Claude\", \"Codex\"."
  (agent-native-backend-name (or agent-native--backend agent-native-backend)))

;;;; The buffer

(defun agent-native--rule ()
  "The line above the input. In a window system, a hairline in a row a few
pixels tall; in a terminal, an underlined row."
  (if (display-graphic-p)
      (let ((padding (agent-native--input-padding)))
        ;; A 1px line; a background would fill the whole (9px) row. It is
        ;; drawn under the row; with `agent-native-input-padding', over
        ;; it, and the padding (the row's line-spacing) lies below.
        (concat (propertize " " 'display '(space :align-to right)
                            'face `(:height 0.1
                                    ,@(if (> padding 0)
                                          '(:overline t)
                                        '(:underline (:color foreground-color :position t)))
                                    :inherit agent-native-dim))
                ;; The row's newline: tiny, so the row is thin.
                (propertize "\n" 'face '(:height 0.1) 'line-spacing padding)))
    (concat (propertize " " 'display '(space :align-to right)
                        'face '(:inherit agent-native-dim :underline t))
            "\n")))

(defun agent-native--label ()
  "Remote Control when it is connected, then the model and effort, as
the terminal shows them."
  (string-join (delq nil (list (and (equal agent-native--remote "connected") "⇄ remote")
                               (and agent-native--model
                                    (string-remove-prefix "claude-" agent-native--model))
                               (and agent-native--effort (concat "◐ " agent-native--effort))))
               " · "))

(defvar-local agent-native--label-overlay nil
  "Overlay on the input that shows the model and effort at its right.")

(defun agent-native--ghost ()
  "The suggestion to show in the input: when it is empty, else nil."
  (and agent-native--suggestion
       (not (string-empty-p agent-native--suggestion))
       (string-empty-p (agent-native--input-text))
       agent-native--suggestion))

(defun agent-native--label-fits-p (label)
  "Whether LABEL fits after the input's last line, in the window showing it.
When it does not, Emacs draws it glued to the wrapped text, so it hides."
  (let* ((window (or (get-buffer-window) (selected-window)))
         (last (or (agent-native--ghost)
                   (car (last (split-string (agent-native--input-text) "\n"))))))
    ;; 2 for the ❯ (or wrap prefix), 1 for a gap. It selects WINDOW, which
    ;; would move point to WINDOW's point, mid-edit when typed text runs this.
    (< (+ 2 (string-width last) 1 (string-width label))
       (save-excursion (window-max-chars-per-line window)))))

(defun agent-native--update-label ()
  "Show the current model and effort at the right end of the input line,
when there is room for them."
  (let* ((label (agent-native--label))
         (ghost (agent-native--ghost))
         (shown (concat
                 ;; The suggestion, with the cursor at its start (as typed
                 ;; text would be): TAB takes it.
                 (when ghost
                   (concat (propertize (substring ghost 0 1) 'face 'agent-native-dim 'cursor t)
                           (propertize (substring ghost 1) 'face 'agent-native-dim)))
                 (unless (or (string-empty-p label) (not (agent-native--label-fits-p label)))
                   ;; `cursor': draw the cursor before the label, at the
                   ;; input's end, not after it at the window's edge.
                   (concat (propertize " " 'display `(space :align-to (- right ,(1+ (string-width label))))
                                       'cursor (not ghost))
                           (propertize label 'face 'agent-native-dim))))))
    (unless (overlayp agent-native--label-overlay)
      ;; Grows with the input (rear-advance), so the label stays after it.
      (setq agent-native--label-overlay
            (make-overlay agent-native--input (point-max) nil nil t)))
    (unless (equal shown (overlay-get agent-native--label-overlay 'after-string))
      (overlay-put agent-native--label-overlay 'after-string shown))))

(defvar-local agent-native--input-edited nil
  "Non-nil when the input changed during the current command.")

(defun agent-native--input-changed (beg end _len)
  "Typing changed the input from BEG to END: keep it plain text (a paste
from the transcript brings its heights and faces), and show or hide the
label as room allows."
  (when (and agent-native--input (>= beg agent-native--input))
    (when (< beg end)
      (with-silent-modifications (set-text-properties beg end nil)))
    ;; Lay out again once the command is done: the input may have gained
    ;; or lost rows, and the log must move to keep it at the bottom.
    (setq agent-native--input-edited t)
    (agent-native--update-label)))

(defun agent-native--after-input-edit ()
  "After a command that changed the input, lay out before redisplay."
  (when agent-native--input-edited
    (setq agent-native--input-edited nil)
    ;; An operator over transcript and input (gg V G d) edits only the
    ;; input but leaves the cursor up top: bring it to the edit.
    (when (< (point) agent-native--input)
      (goto-char agent-native--input))
    (agent-native--follow)))

(defvar-keymap agent-native-mode-map
  "RET" #'agent-native-send
  "S-<return>" #'newline
  "C-j" #'newline
  "C-c C-c" #'agent-native-interrupt
  "<backtab>" #'agent-native-cycle-mode
  "C-c C-m" #'agent-native-choose-model
  "C-c C-e" #'agent-native-set-effort
  "C-c C-p" #'agent-native-answer-pending
  "C-c C-k" #'agent-native-kill
  "C-c /" #'agent-native-command
  "C-c C-a" #'agent-native-attach
  "C-c C-t" #'agent-native-agents
  "C-c C-r" #'agent-native-toggle-remote-control
  "M-p" #'agent-native-previous-prompt
  "M-n" #'agent-native-next-prompt
  "<up>" #'agent-native-up
  "<down>" #'agent-native-down
  "TAB" #'agent-native-tab
  "<remap> <previous-line>" #'agent-native-previous-line
  "<remap> <evil-previous-line>" #'agent-native-previous-line
  "<remap> <evil-previous-visual-line>" #'agent-native-previous-line
  ;; The key itself: visual-line-mode's own remap would win over a remap.
  "<home>" #'agent-native-beginning-of-line
  "<remap> <evil-open-below>" #'agent-native-open-line
  "<remap> <evil-open-above>" #'agent-native-open-line
  "<remap> <move-beginning-of-line>" #'agent-native-beginning-of-line
  "<remap> <beginning-of-visual-line>" #'agent-native-beginning-of-line
  "<remap> <evil-beginning-of-line>" #'agent-native-beginning-of-line
  "<remap> <evil-beginning-of-visual-line>" #'agent-native-beginning-of-line
  "<remap> <evil-first-non-blank>" #'agent-native-beginning-of-line
  "<remap> <evil-first-non-blank-of-visual-line>" #'agent-native-beginning-of-line)

(defun agent-native-beginning-of-line ()
  "To the line's start; on the input's first line, to the input's start
\(the ❯ before it is not a place the cursor may be)."
  (interactive)
  (let ((start (if (fboundp 'beginning-of-visual-line)
                   (save-excursion (beginning-of-visual-line) (point))
                 (line-beginning-position))))
    (goto-char (if (and (>= (point) agent-native--input) (< start agent-native--input))
                   agent-native--input
                 start))))

(defun agent-native-previous-line (&optional n)
  "Move up N lines; from the input's first line, to the transcript's end.
\(Line by line, the cursor would land in the strip above the input, which
it may not enter, and be put back.)"
  (interactive "p")
  (if (agent-native--in-input-edge-p t)
      (goto-char agent-native--out)
    (if (and (bound-and-true-p evil-mode) (fboundp 'evil-previous-visual-line))
        (evil-previous-visual-line (or n 1))
      (line-move (- (or n 1))))))

(define-derived-mode agent-native-mode fundamental-mode "Agent"
  "An agent session: transcript above, input below."
  (setq-local truncate-lines nil
              word-wrap t
              ;; Wrapped lines line up under the text, past the bullet.
              wrap-prefix agent-native--indent
              buffer-undo-list t
              completion-at-point-functions (list #'agent-native--complete)
              ;; Status goes under the log (`agent-native--refresh-status'),
              ;; so nothing but the input sits at the bottom.
              mode-line-format nil
              ;; Scroll just enough to show the cursor, never recenter: a
              ;; recenter on the input would show blank space under it.
              scroll-conservatively 101
              scroll-margin 0)
  (visual-line-mode 1)
  (cursor-intangible-mode 1)
  ;; A restart (/clear, /resume) keeps the buffer: drop its old overlays.
  (remove-overlays)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; The line before the input, then the input itself. The transcript
    ;; grows in front of that line: its marker moves past what is inserted.
    ;; End of the transcript's last line, the spacer line, the rule.
    (insert "\n\n" (agent-native--rule) (propertize "❯ " 'face 'agent-native-accent))
    (agent-native--protect (point-min) (point))
    ;; The cursor never stops in this strip (spacer, rule, ❯): from the
    ;; input, up or left goes straight to the transcript's end. The mode
    ;; reads stickiness, so the property sticks forward from the strip's
    ;; second char; the first ends the transcript's last line, which stays
    ;; reachable.
    (put-text-property (point-min) (point) 'cursor-intangible t)
    (put-text-property (1+ (point-min)) (point) 'front-sticky '(read-only cursor-intangible))
    (setq agent-native--out (copy-marker (point-min) t)
          agent-native--spacer (copy-marker (1+ (point-min)))
          agent-native--input (point-max-marker)))
  ;; Typing always goes to the input: entering insert state (evil) or
  ;; typing (without it) while up in the transcript jumps to the input's end.
  (add-hook 'evil-insert-state-entry-hook #'agent-native--to-input nil t)
  (add-hook 'pre-command-hook #'agent-native--typing-to-input nil t)
  ;; Killing the buffer ends its agent.
  (add-hook 'kill-buffer-hook (lambda () (agent-native-stop agent-native--conn)) nil t)
  (add-hook 'kill-buffer-hook #'agent-native--delete-temp-files nil t)
  ;; A resized window lays out again.
  ;; The hook runs with the window as argument, not in its buffer.
  (add-hook 'window-size-change-functions
            (lambda (window)
              (with-current-buffer (window-buffer window)
                (agent-native--follow)
                (agent-native--update-label)))
            nil t)
  (add-hook 'after-change-functions #'agent-native--input-changed nil t)
  ;; Every redisplay holds the layout (see `agent-native--keep-layout'),
  ;; whatever moved the window: j, G, C-f, zt, the mouse wheel.
  (add-hook 'pre-redisplay-functions #'agent-native--before-redisplay)
  (add-hook 'post-command-hook #'agent-native--after-input-edit nil t)
  ;; With evil, leaving insert mode keeps the cursor in the input rather
  ;; than stepping back onto the ❯.
  (setq-local evil-move-cursor-back nil
              evil-move-beyond-eol t)
  (setq buffer-undo-list nil))

;; Evil's line operators (dd, cc, S) on the input would take the read-only
;; ❯ before it and fail: a range reaching into the input starts at the
;; input instead, and is plain text, not whole lines.
(defun agent-native--clamp-to-input (args)
  "Clamp an evil operator's ARGS (BEG END TYPE ...) to the input."
  (if (and (derived-mode-p 'agent-native-mode)
           (>= (nth 1 args) agent-native--input)
           (< (nth 0 args) agent-native--input))
      `(,(marker-position agent-native--input) ,(nth 1 args)
        ,(if (eq (nth 2 args) 'line) 'exclusive (nth 2 args)) ,@(nthcdr 3 args))
    args))

(defun agent-native--clamp-change (args)
  "Clamp evil's change ARGS like a delete's; cc keeps the line's newline,
so the lines below do not join it."
  (let ((clamped (agent-native--clamp-to-input args)))
    (if (and (not (eq clamped args)) (eq (nth 2 args) 'line)
             (> (nth 1 clamped) (nth 0 clamped))
             (eq (char-before (nth 1 clamped)) ?\n))
        `(,(nth 0 clamped) ,(1- (nth 1 clamped)) ,@(nthcdr 2 clamped))
      clamped)))

(defun agent-native--clamp-yank (args)
  "Clamp evil's yank ARGS (BEG END TYPE ...) that start on the ❯, so yy
in the input copies what you typed, not the prompt."
  (if (and (derived-mode-p 'agent-native-mode)
           (< (nth 0 args) agent-native--input)
           (>= (nth 0 args) (save-excursion (goto-char agent-native--input) (line-beginning-position))))
      `(,(marker-position agent-native--input) ,(max (nth 1 args) agent-native--input)
        ,(if (eq (nth 2 args) 'line) 'exclusive (nth 2 args)) ,@(nthcdr 3 args))
    args))

(declare-function evil-get-register "evil-common")
(declare-function evil-visual-state-p "evil-states")
(declare-function evil-exit-visual-state "evil-states")
(defvar evil-state)
(defun agent-native--paste (orig after count &optional register yank-handler)
  "Evil's p (AFTER) and P (ORIG, with COUNT, REGISTER, YANK-HANDLER): in the input,
paste as plain text, nothing landing before the ❯. In visual state, over
the selection. Whole lines (from dd or yy) go on a line of their own,
except into an empty input, which would only grow a blank row."
  (if (not (derived-mode-p 'agent-native-mode))
      (funcall orig count register yank-handler)
    (when-let* ((raw (if register (evil-get-register register) (current-kill 0))))
      (let* ((raw (substring-no-properties raw))
             (text (string-trim-right raw "\n"))
             (lines (and (string-suffix-p "\n" raw)
                         (not (string-empty-p (agent-native--input-text)))
                         (not (and (fboundp 'evil-visual-state-p) (evil-visual-state-p))))))
        (when (and (fboundp 'evil-visual-state-p) (evil-visual-state-p))
          (let ((beg (max (region-beginning) agent-native--input))
                (end (max (region-end) agent-native--input)))
            (evil-exit-visual-state)
            (delete-region beg end)
            (goto-char beg)
            (setq after nil)))
        (agent-native--to-input)
        (when (< (point) agent-native--input) (goto-char agent-native--input))
        (cond
         ((and lines after) (end-of-line) (insert "\n") (setq text (string-join (make-list (prefix-numeric-value count) text) "\n") count 1))
         (lines (goto-char (max (line-beginning-position) agent-native--input))
                (save-excursion (insert "\n")) (setq text (string-join (make-list (prefix-numeric-value count) text) "\n") count 1))
         ((and after (< (point) (point-max)) (not (eolp))) (forward-char)))
        (dotimes (_ (prefix-numeric-value count)) (insert text))
        (when (and (bound-and-true-p evil-mode) (eq evil-state 'normal)
                   (> (point) agent-native--input))
          (backward-char))))))

(defun agent-native--paste-after (orig &rest args)
  (apply #'agent-native--paste orig t args))
(defun agent-native--paste-before (orig &rest args)
  (apply #'agent-native--paste orig nil args))

(with-eval-after-load 'evil
  (advice-add 'evil-delete :filter-args #'agent-native--clamp-to-input)
  (advice-add 'evil-change :filter-args #'agent-native--clamp-change)
  (advice-add 'evil-yank :filter-args #'agent-native--clamp-yank)
  (advice-add 'evil-paste-after :around #'agent-native--paste-after)
  (advice-add 'evil-paste-before :around #'agent-native--paste-before))

;; With evil: RET sends from normal state too, and sessions open in insert.
(declare-function evil-define-key* "evil-core")
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-insert-state "evil-states")
(declare-function evil-open-above "evil-commands")
(declare-function evil-open-below "evil-commands")
(with-eval-after-load 'evil
  (evil-define-key* 'normal agent-native-mode-map (kbd "RET") #'agent-native-send)
  (evil-define-key* '(normal insert) agent-native-mode-map
    (kbd "<up>") #'agent-native-up (kbd "<down>") #'agent-native-down)
  (evil-set-initial-state 'agent-native-mode 'insert))

(defun agent-native-open-line (count)
  "Evil's o and O: in the input, open a line as usual; up in the read-only
transcript, go to the input's end to type there."
  (interactive "p")
  (cond
   ((< (point) agent-native--input)
    (goto-char (point-max)) (evil-insert-state))
   ;; Above the input's first line would be before the ❯: open the line
   ;; at the input's start instead.
   ((and (eq this-original-command 'evil-open-above)
         (< (line-beginning-position) agent-native--input))
    (goto-char agent-native--input)
    (dotimes (_ count) (insert "\n"))
    (goto-char agent-native--input)
    (evil-insert-state))
   ((eq this-original-command 'evil-open-above) (evil-open-above count))
   (t (evil-open-below count))))

(defun agent-native--to-input ()
  "Move to the input's end if the cursor is up in the transcript."
  (when (< (point) agent-native--input)
    (goto-char (point-max))))

(defun agent-native--typing-to-input ()
  "Before a typing command up in the transcript, move to the input."
  (when (memq this-command '(self-insert-command yank yank-pop newline))
    (agent-native--to-input)))

(defun agent-native--input-text ()
  "What is typed in the input area."
  (buffer-substring-no-properties agent-native--input (point-max)))

(defun agent-native--set-input (text)
  "Replace the input area with TEXT."
  (let ((inhibit-read-only t))
    (delete-region agent-native--input (point-max))
    (goto-char (point-max))
    (insert text)))

;;;; Status line

(defun agent-native--refresh-status ()
  "Redraw the status line under the log."
  (agent-native--show-status (agent-native--status-line)))

(defun agent-native--status-line ()
  "The status under the log: only what needs you, else nil.
The agent working, a permission request, a mode other than default."
  (let* ((mode (and agent-native--backend
                    (alist-get agent-native--mode (agent-native-backend-modes agent-native--backend))))
         (parts (delq nil
                      (list (when agent-native--busy
                              (propertize (format "%s %s (%ds%s)"
                                                  (aref agent-native--spinner-frames
                                                        (mod (truncate (* 8 (float-time))) (length agent-native--spinner-frames)))
                                                  agent-native--status
                                                  (truncate (- (float-time) agent-native--busy))
                                                  (if (> agent-native--thinking-tokens 0)
                                                      (format " · %d thinking tokens" agent-native--thinking-tokens) ""))
                                          'face 'agent-native-accent))
                            (when agent-native--pending
                              (propertize "permission needed: C-c C-p" 'face 'warning))
                            (when agent-native--queued
                              (format "%d queued" (length agent-native--queued)))
                            (when agent-native--agents
                              (format "%d agent%s: C-c C-t" (length agent-native--agents)
                                      (if (cdr agent-native--agents) "s" "")))
                            (when (and mode (not (string-empty-p mode))) mode)
                            (unless (agent-native-live-p agent-native--conn) "ended")))))
    (when parts
      (concat (propertize (string-join parts "  ·  ") 'face 'agent-native-dim)))))

(defun agent-native--set-busy (busy &optional status)
  "Mark the session BUSY (or idle), saying STATUS, and run the spinner."
  (setq agent-native--status (or status "Working…"))
  (cond
   ((and busy (not agent-native--busy))
    (setq agent-native--busy (float-time)
          agent-native--thinking-tokens 0)
    (let ((buffer (current-buffer))
          timer)
      (setq timer (run-at-time 0 0.12 (lambda ()
                                        (if (buffer-live-p buffer)
                                            (with-current-buffer buffer (agent-native--refresh-status))
                                          (cancel-timer timer))))
            agent-native--spinner timer)))
   ((not busy)
    (setq agent-native--busy nil)
    (when agent-native--spinner (cancel-timer agent-native--spinner))
    (setq agent-native--spinner nil)))
  (agent-native--refresh-status))

;;;; Events from the backend

(defun agent-native--event (buffer type props)
  "Draw event TYPE with PROPS (see agent-native-backend.el) in session BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; A resumed session is drawn, not worked on: no spinner.
      (let ((live (not agent-native--drawing-history)))
        (pcase type
          ('session (agent-native--take-session props))
          ('prompt (agent-native--prompt-taken (plist-get props :text)))
          ('working (when live (agent-native--set-busy t (plist-get props :status))))
          ('thinking-tokens (setq agent-native--thinking-tokens (or (plist-get props :count) 0)))
          ('text-start (when live (agent-native--set-busy t "Writing…"))
                       (agent-native--text-start))
          ('text-delta (agent-native--text-delta (plist-get props :text)))
          ('text-end (agent-native--block-stop))
          ('text (agent-native--text-whole (plist-get props :text)))
          ('tool (agent-native--tool-use (plist-get props :id) (plist-get props :name) (plist-get props :input)))
          ('tool-result (agent-native--tool-result (plist-get props :id) (plist-get props :content)
                                                   (plist-get props :error)))
          ('agent-step (agent-native--agent-step (plist-get props :tool-id) (plist-get props :name)
                                                 (plist-get props :input)))
          ('agent-start (agent-native--agent-start props))
          ('agent-progress (agent-native--agent-progress props))
          ('agent-model (agent-native--agent-model (plist-get props :tool-id) (plist-get props :model)))
          ('agent-end (agent-native--agent-end (plist-get props :id)))
          ('request (agent-native--request props))
          ('request-done (agent-native--drop-request (plist-get props :id)))
          ('interrupted
           (agent-native--note (format "%s⎿  Interrupted · What should %s do instead?"
                                       agent-native--indent (agent-native--name))
                               'agent-native-tool-error))
          ('note (agent-native--note (plist-get props :text) (plist-get props :face)))
          ('turn-end (agent-native--turn-ended props))
          ('suggestion (setq agent-native--suggestion (plist-get props :text))
                       (agent-native--update-label))
          ('remote (agent-native--remote-changed props))
          ('history-start (setq agent-native--drawing-history t))
          ('history-end (setq agent-native--drawing-history nil)
                        (agent-native--note "↑ resumed" 'agent-native-dim)
                        (agent-native--follow)))
        (agent-native--refresh-status)))))

(defconst agent-native--own-commands
  '(("resume" . "Resume a past session of this directory")
    ("model" . "Pick a provider (Claude or OpenAI), a model and its effort")
    ("effort" . "Pick an effort level")
    ("clear" . "Start a new conversation in this buffer")
    ("usage" . "Show plan limits, context use and what this session used")
    ("tasks" . "List the sub-agents at work")
    ("exit" . "End this session"))
  "Commands done here, for every backend.")

(defun agent-native--take-session (props)
  "Keep what the agent says about its session (PROPS of a `session'
event); a key that is there with nil means \"none\"."
  (cl-flet ((has (key) (plist-member props key))
            (get (key) (plist-get props key)))
    (when (has :id) (setq agent-native--session-id (get :id)))
    (when (get :model) (setq agent-native--model (get :model)))
    (when (has :effort) (setq agent-native--effort (get :effort)))
    (when (get :mode) (setq agent-native--mode (get :mode)))
    (when (has :models) (setq agent-native--models (get :models)))
    (when (has :commands)
      (setq agent-native--commands
            (append (get :commands)
                    (seq-remove (lambda (own) (assoc (car own) (get :commands)))
                                agent-native--own-commands))
            agent-native--command-hints (get :hints))))
  (agent-native--update-label))

(defun agent-native--agent-step (parent name input)
  "Show a sub-agent's tool call NAME with INPUT as one updating line under
its Agent call PARENT, as the terminal does."
  (let* ((entry (alist-get parent agent-native--subagents nil nil #'equal))
         (count (1+ (or (cdr entry) 0)))
         (summary (agent-native--tool-summary name input))
         (line (format "%s%s · %d tool use%s" name
                       (if (string-empty-p summary) "" (format "(%s)" summary))
                       count (if (= count 1) "" "s"))))
    (agent-native--remove (car entry))
    (setf (alist-get parent agent-native--subagents nil nil #'equal)
          (cons (agent-native--tool-note parent line) count))))

(defun agent-native--prompt-taken (text)
  "The agent took up prompt TEXT: draw it (unless drawn when sent) and
start the turn's spinner. In a resumed session, draw it, and let M-p
bring it back."
  (cond
   (agent-native--drawing-history
    (agent-native--user text)
    (push text agent-native--history)
    (agent-native--count-attachments text))
   ((member text agent-native--drawn)
    (setq agent-native--drawn (agent-native--remove-first text agent-native--drawn)))
   ((member text agent-native--queued)
    (setq agent-native--queued (agent-native--remove-first text agent-native--queued))
    (agent-native--user text)
    (agent-native--new-turn))
   (t
    ;; Prompts sent close together come back as one, joined by newlines,
    ;; when the agent takes them up together: some drawn already, some queued.
    (let ((rest text) new)
      (while rest
        (let ((head (seq-find (lambda (p) (and p (or (equal rest p) (string-prefix-p (concat p "\n") rest))))
                              (list (car agent-native--drawn) (car agent-native--queued)))))
          (cond
           ((null head) (push rest new) (setq rest nil))
           (t (cond ((equal head (car agent-native--drawn)) (pop agent-native--drawn))
                    (t (pop agent-native--queued) (push head new)))
              (setq rest (and (not (equal rest head)) (substring rest (1+ (length head)))))))))
      (when new (agent-native--user (string-join (nreverse new) "\n")))
      (agent-native--new-turn)))))

(defun agent-native--new-turn ()
  "A new turn: restart the clock (and the one spinner timer)."
  (agent-native--set-busy nil)
  (agent-native--set-busy t "Working…"))

(defun agent-native--remove-first (item list)
  "LIST without its first ITEM (by `equal')."
  (let ((tail (member item list)))
    (if tail (append (butlast list (length tail)) (cdr tail)) list)))

(defun agent-native--turn-ended (props)
  "A turn ended (PROPS of a `turn-end' event): say how long it took, or
what went wrong."
  (setq agent-native--drawn nil)
  ;; Prompts still queued: their turns come next, so keep the spinner.
  ;; Slash commands are never echoed back, so the next one starts its
  ;; turn here instead.
  (if (and agent-native--queued (string-prefix-p "/" (car agent-native--queued)))
      (agent-native--user (pop agent-native--queued))
    (unless agent-native--queued (agent-native--set-busy nil)))
  ;; Requests the turn left unanswered are dead now.
  (dolist (pending agent-native--pending)
    (agent-native--drop-request (plist-get pending :id)))
  (when (plist-get props :cost)
    (setq agent-native--cost (plist-get props :cost)))
  ;; An interrupted turn may end as an error; "Interrupted" already says so.
  (when (and (plist-get props :error) (not agent-native--interrupted))
    (agent-native--note (format "Error: %s" (plist-get props :error)) 'agent-native-tool-error))
  (let ((ms (plist-get props :duration-ms)))
    (when (and ms (> ms 1000))
      (agent-native--note (format "✻ Worked for %ds" (round ms 1000)))))
  (unless (or agent-native--interrupted agent-native--queued)
    (agent-native--notify props))
  (setq agent-native--interrupted nil))

(defun agent-native--notify (props)
  "Tell of the turn that ended (PROPS of its `turn-end' event) in the echo
area, unless this buffer is in sight."
  (when (and agent-native-notify (not (get-buffer-window (current-buffer) 'visible)))
    (let* ((error (plist-get props :error))
           (text (string-trim (or (plist-get props :text) error "")))
           (head (format "%s %s in %s: " (agent-native--name) (if error "failed" "finished") (buffer-name)))
           (line (car (split-string text "\n" t))))
      (message "%s%s" (propertize head 'face (if error 'agent-native-tool-error 'agent-native-accent))
               (truncate-string-to-width (or line "") (max 0 (- (frame-width) (string-width head) 1)) nil nil "…")))))

;;;; Permission requests

(defun agent-native--request (request)
  "The agent asks something: REQUEST, the plist of a `request' event."
  (setq agent-native--pending (append agent-native--pending (list request)))
  (push (cons (plist-get request :id)
              (agent-native--tool-note (plist-get request :tool-id) "Waiting for permission…"))
        agent-native--waiting-notes)
  (agent-native--refresh-status)
  ;; Out of the process filter, so the question may use the minibuffer.
  (let ((buffer (current-buffer)))
    (run-at-time 0 nil (lambda () (when (buffer-live-p buffer)
                                    (with-current-buffer buffer (agent-native-answer-pending t)))))))

(defun agent-native--ask-permission (request)
  "Ask in the minibuffer whether the agent may run REQUEST's tool."
  (let* ((name (plist-get request :name))
         (summary (agent-native--tool-summary name (plist-get request :input)))
         (prompt (format "%sAllow %s%s? "
                         ;; From a session out of sight, say which.
                         (if (get-buffer-window (current-buffer) 'visible) "" (format "[%s] " (buffer-name)))
                         name (if (string-empty-p summary) "" (format "(%s)" summary))))
         (answer (read-multiple-choice
                  prompt
                  `((?y "yes" "Run it this time")
                    ;; What is kept, and for how long, is the agent's to say.
                    (?a "always" ,(or (plist-get request :always) "Yes, and don't ask again for this"))
                    (?n "no" ,(format "No, and tell %s what to do differently" (agent-native--name)))))))
    (pcase (car answer) (?y 'allow) (?a 'always) (?n 'deny))))

(defun agent-native--ask-questions (questions)
  "Ask QUESTIONS (plists :id :question :options) in the minibuffer, one
at a time; the answer `agent-native-backend-answer' takes."
  (cons 'answers
        (mapcar (lambda (question)
                  (cons (plist-get question :id)
                        (completing-read (concat (plist-get question :question) " ")
                                         (plist-get question :options))))
                questions)))

(defun agent-native--drop-request (id)
  "Forget permission request ID (answered, cancelled, or its turn over)
and remove its \"Waiting for permission…\" note."
  (setq agent-native--pending
        (seq-remove (lambda (request) (equal (plist-get request :id) id)) agent-native--pending))
  (agent-native--remove (alist-get id agent-native--waiting-notes nil nil #'equal))
  (setq agent-native--waiting-notes (assoc-delete-all id agent-native--waiting-notes))
  (agent-native--refresh-status))

(defun agent-native-answer-pending (&optional quietly)
  "Answer the oldest permission request, one question at a time.
C-g leaves it waiting. QUIETLY (from the timer): say nothing when none waits."
  (interactive)
  (let ((request (car agent-native--pending)))
    (cond
     (agent-native--asking nil)
     ((null request) (unless quietly (message "No permission request is waiting")))
     (t
      (let* ((id (plist-get request :id))
             (tool-id (plist-get request :tool-id))
             (decision (let ((agent-native--asking t))
                         (condition-case nil
                             (if (plist-get request :questions)
                                 (agent-native--ask-questions (plist-get request :questions))
                               (funcall agent-native-permission-function request))
                           (quit nil)))))
        ;; Asked, but cancelled or ended meanwhile: nothing to answer.
        (when (and decision (seq-find (lambda (r) (equal (plist-get r :id) id)) agent-native--pending))
          (agent-native-backend-answer agent-native--conn request decision)
          ;; The waiting note goes; only a denial stays, as in the terminal.
          (agent-native--drop-request id)
          (when (eq decision 'deny)
            ;; Saying no also interrupts the turn, which may end as an
            ;; error; "Denied" and "Interrupted" say enough.
            (push tool-id agent-native--denied)
            (setq agent-native--interrupted t)
            (agent-native--tool-note tool-id "Denied" 'agent-native-tool-error)))
        ;; The next request, asked fresh (not inside this one); also when
        ;; this one was cancelled while asked, so none is left unasked.
        (when (and decision agent-native--pending)
          (let ((buffer (current-buffer)))
            (run-at-time 0 nil (lambda () (when (buffer-live-p buffer)
                                            (with-current-buffer buffer (agent-native-answer-pending t))))))))))))

;;;; Commands

(defun agent-native--need-agent ()
  "Stop the command unless this session's agent runs."
  (unless (agent-native-live-p agent-native--conn)
    (user-error "%s is not running; M-x agent-native starts it again" (agent-native--name))))

(defun agent-native--project-root ()
  "The directory a session for here should run in."
  (if-let* ((project (project-current))) (project-root project) default-directory))

(defun agent-native--buffer-name (backend directory)
  "The name of BACKEND's session buffer for DIRECTORY."
  (format "*%s: %s*" (downcase (agent-native-backend-name backend))
          (abbreviate-file-name (directory-file-name directory))))

(defun agent-native--session-buffers (directory &optional backend)
  "Session buffers of DIRECTORY (only BACKEND's, if given), most recently
used first."
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (seq-filter (lambda (buffer)
                  (with-current-buffer buffer
                    (and (derived-mode-p 'agent-native-mode)
                         (equal (file-name-as-directory (expand-file-name default-directory)) directory)
                         (or (null backend) (eq agent-native--backend backend)))))
                (buffer-list))))

(defun agent-native--visit (backend directory &optional continue)
  "Show BACKEND's session for DIRECTORY, starting it if needed; with
CONTINUE, a new start resumes BACKEND's last conversation there."
  (let ((buffer (or (car (agent-native--session-buffers directory backend))
                    (get-buffer-create (agent-native--buffer-name backend directory)))))
    (with-current-buffer buffer
      (unless (agent-native-live-p agent-native--conn)
        ;; The agent ended here (C-c C-k, a crash): carry on with its
        ;; conversation, as it was set up, and keep what was typed but not sent.
        (let* ((ended (and (derived-mode-p 'agent-native-mode) agent-native--session-id))
               (draft (and (derived-mode-p 'agent-native-mode) (agent-native--input-text)))
               (session (if continue
                            (plist-get (car (agent-native-backend-sessions backend directory)) :id)
                          ended))
               (same (and ended (equal session ended))))
          (setq default-directory directory)
          (if same
              (agent-native--reopen session)
            (agent-native--open backend session))
          (when (and draft (not (string-empty-p draft)))
            (agent-native--set-input draft)))))
    ;; In this window, like opening a file; no split.
    (pop-to-buffer-same-window buffer)
    (goto-char (point-max))))

;;;###autoload
(defun agent-native (&optional continue)
  "Open this project's agent session, starting it if needed.
That is the session used last here, whatever its backend; with none, one
of `agent-native-backend'. With a prefix argument CONTINUE, a session
that starts resumes the last conversation here."
  (interactive "P")
  (let* ((directory (agent-native--project-root))
         (last (car (agent-native--session-buffers directory))))
    (agent-native--visit (if last (buffer-local-value 'agent-native--backend last) agent-native-backend)
                         directory continue)))

;;;###autoload
(defun agent-native-claude (&optional continue)
  "Open this project's Claude Code session, starting it if needed.
With a prefix argument CONTINUE, resume the last conversation."
  (interactive "P")
  (agent-native--visit 'claude (agent-native--project-root) continue))

;;;###autoload
(defun agent-native-codex (&optional continue)
  "Open this project's Codex session, starting it if needed.
With a prefix argument CONTINUE, resume the last conversation."
  (interactive "P")
  (agent-native--visit 'codex (agent-native--project-root) continue))

(defun agent-native--read-backend ()
  "Pick an installed backend in the minibuffer."
  (intern (completing-read "Agent: " (mapcar #'symbol-name (seq-filter #'agent-native-backend-available-p
                                                                       agent-native-backends))
                           nil t)))

;;;###autoload
(defun agent-native-new (&optional ask)
  "Start another session for this project, in a new buffer named like
the first with a <2>, <3> suffix. It runs this session's backend, else
`agent-native-backend'; with a prefix argument ASK, one picked from a list."
  (interactive "P")
  (let* ((directory (agent-native--project-root))
         (backend (cond (ask (agent-native--read-backend))
                        ((derived-mode-p 'agent-native-mode) agent-native--backend)
                        (t agent-native-backend)))
         (buffer (generate-new-buffer (agent-native--buffer-name backend directory))))
    (with-current-buffer buffer
      (setq default-directory directory)
      (agent-native--open backend))
    (pop-to-buffer-same-window buffer)
    (goto-char (point-max))))

;;;###autoload
(defun agent-native-continue ()
  "Open this project's session, resuming its last conversation.
The same as `agent-native' with a prefix, for setups where C-u scrolls."
  (interactive)
  (agent-native t))

(defun agent-native--open (backend &optional session-id)
  "Start this buffer's session with BACKEND afresh, or resume SESSION-ID."
  (agent-native-stop agent-native--conn)
  (agent-native-mode)
  (setq agent-native--backend backend
        agent-native--mode agent-native-permission-mode
        ;; Known now, not only once a turn runs: ended before then, the
        ;; session must still be the one to carry on.
        agent-native--session-id session-id)
  (agent-native--note (format "✻ %s  ·  %s" (agent-native-backend-title backend)
                              (abbreviate-file-name default-directory))
                      'agent-native-accent)
  (agent-native--start session-id))

(defun agent-native--reopen (&optional session-id)
  "Start this buffer's session again, resuming SESSION-ID (nil: a new
conversation), with the model, effort and permission mode it has now."
  (let ((model agent-native--model)
        (effort agent-native--effort)
        (agent-native-permission-mode agent-native--mode))
    (agent-native--open agent-native--backend session-id)
    (when model
      (agent-native-set-model-and-effort model effort))))

(defun agent-native--start (&optional session-id)
  "Start this buffer's agent; SESSION-ID resumes that session."
  (let ((buffer (current-buffer)))
    (setq agent-native--conn
          (agent-native-backend-start
           agent-native--backend default-directory
           :resume session-id :mode agent-native--mode
           :on-event (lambda (type props) (agent-native--event buffer type props))
           :on-exit (lambda (event)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (setq agent-native--agents nil)
                          (agent-native--remote-changed nil)
                          (agent-native--set-busy nil)
                          (agent-native--note (format "%s exited (%s). M-x agent-native starts again."
                                                      (agent-native--name) event)
                                              'agent-native-tool-error))))))))

(defun agent-native-send ()
  "Send what is in the input area; on a \"… +N lines\", expand it
instead; on a link, open it."
  (interactive)
  (cond ((get-text-property (point) 'agent-native-more) (agent-native-expand))
        ((get-text-property (point) 'agent-native-link) (agent-native-open-link))
        (t (agent-native--send-input))))

(defun agent-native--send-input ()
  "Send what is in the input area; a slash command done here runs here."
  (agent-native--need-agent)
  (let ((text (string-trim (agent-native--input-text))))
    (if-let* ((native (agent-native--native-command text)))
        (progn (agent-native--set-input "") (call-interactively native))
      (if (string-match "\\`/\\(model\\|effort\\) +\\([^ \n]+\\)\\'" text)
          (let ((command (if (equal (match-string 1 text) "model") #'agent-native-set-model #'agent-native-set-effort))
                (value (match-string 2 text)))
            (agent-native--set-input "")
            (funcall command value))
        (agent-native--send-text text)))))

(defun agent-native--send-text (text)
  "Send TEXT as your next prompt and draw it."
  (unless (string-empty-p text)
    ;; Find attached files first: a missing one stops the send, input kept.
    (let ((attachments (agent-native--attachments-in text)))
      (push text agent-native--history)
      (setq agent-native--suggestion nil
            agent-native--history-index -1
            agent-native--history-draft "")
      (agent-native--set-input "")
      ;; Idle: draw it now (the agent echoes it only once the answer
      ;; starts). Working: draw it on the echo (see
      ;; `agent-native--prompt-taken'), so it lands after the current
      ;; turn, not inside its text.
      (if agent-native--busy
          (setq agent-native--queued (append agent-native--queued (list text)))
        (agent-native--user text)
        (setq agent-native--drawn (append agent-native--drawn (list text))))
      ;; The sent input's undo history is of no use now, and a short list
      ;; keeps shifting it (see `agent-native--shift-undo') cheap.
      (setq buffer-undo-list nil)
      (unless agent-native--busy (agent-native--set-busy t "Working…"))
      (agent-native--refresh-status)
      (agent-native-backend-send agent-native--conn text attachments))))

;;;; Slash commands as Emacs commands

(defconst agent-native--native-commands
  '(("resume" . agent-native-resume)
    ("model" . agent-native-choose-model)
    ("effort" . agent-native-set-effort)
    ("clear" . agent-native-clear)
    ("usage" . agent-native-usage)
    ("cost" . agent-native-usage)
    ("stats" . agent-native-usage)
    ("tasks" . agent-native-agents)
    ("remote-control" . agent-native-toggle-remote-control)
    ("rc" . agent-native-toggle-remote-control)
    ("exit" . agent-native-exit)
    ("quit" . agent-native-exit))
  "Slash commands done here, the same for every backend.")

(defun agent-native--native-command (text)
  "The Emacs command for TEXT if it is a slash command done natively."
  (when (string-match "\\`/\\([^ \n]+\\)\\s-*\\'" text)
    (cdr (assoc (match-string 1 text) agent-native--native-commands))))

(defun agent-native-command (name &optional args)
  "Run slash command NAME with ARGS, picked from the commands there are."
  (interactive
   (let* ((names (mapcar #'car agent-native--commands))
          (name (completing-read
                 "Command: "
                 (lambda (string pred action)
                   (if (eq action 'metadata)
                       `(metadata (annotation-function
                                   . ,(lambda (n) (when-let* ((doc (cdr (assoc n agent-native--commands))))
                                                    (concat "  " (truncate-string-to-width doc 70 nil nil "…"))))))
                     (complete-with-action action names string pred)))
                 nil t)))
     ;; Ask for arguments only when the command takes them.
     (list name (when-let* ((hint (cdr (assoc name agent-native--command-hints))))
                  (read-string (format "/%s %s: " name hint))))))
  (if-let* ((native (cdr (assoc name agent-native--native-commands))))
      (call-interactively native)
    (agent-native--send-text (string-trim (concat "/" name " " (or args ""))))))

(defun agent-native--ago (time)
  "TIME as the terminal shows it: \"5m ago\", \"3h ago\", \"2d ago\"."
  (let ((seconds (float-time (time-subtract nil time))))
    (cond ((< seconds 3600) (format "%dm ago" (max 1 (round seconds 60))))
          ((< seconds 86400) (format "%dh ago" (round seconds 3600)))
          (t (format "%dd ago" (round seconds 86400))))))

(defun agent-native--session-directory ()
  "The directory whose sessions are meant here: a session buffer's own,
elsewhere the project's."
  (if (derived-mode-p 'agent-native-mode) default-directory (agent-native--project-root)))

(defun agent-native--past-sessions (directory)
  "The past sessions of DIRECTORY with every installed backend, newest
first; each with its :backend. A backend that fails is left out."
  (let (all)
    (dolist (backend agent-native-backends)
      (when (agent-native-backend-available-p backend)
        (dolist (session (condition-case err (agent-native-backend-sessions backend directory)
                           (error (message "%s: %s" (agent-native-backend-name backend) (error-message-string err))
                                  nil)))
          (push (append (list :backend backend) session) all))))
    (sort all (lambda (a b) (time-less-p (plist-get b :time) (plist-get a :time))))))

(defun agent-native-resume (session-id &optional backend)
  "Resume SESSION-ID, a past session of this project, with BACKEND.
Sessions of every installed backend are listed, by name, newest first,
with when and how they began. In a session buffer of the same backend,
the session takes that buffer over; else it opens in the project's
session buffer for its backend."
  (interactive
   (let* ((sessions (or (agent-native--past-sessions (agent-native--session-directory))
                        (user-error "No past sessions here")))
          (several (cdr (delete-dups (mapcar (lambda (s) (plist-get s :backend)) sessions))))
          ;; A session's name, else its first prompt's first line; names may
          ;; repeat, so a number keeps choices apart.
          (choices (let ((seen (make-hash-table :test #'equal)))
                     (mapcar (lambda (session)
                               ;; Not cut short: every word stays searchable.
                               (let* ((name (string-trim (or (plist-get session :title)
                                                             (car (split-string (or (plist-get session :prompt) "") "\n")))))
                                      (n (cl-incf (gethash name seen 0))))
                                 (cons (if (= n 1) name (format "%s <%d>" name n)) session)))
                             sessions)))
          ;; When it was, which agent (when there are several), and how a
          ;; named session began.
          (annotate (lambda (choice)
                      (when-let* ((session (cdr (assoc choice choices))))
                        (concat (propertize " " 'display '(space :align-to 58))
                                (propertize
                                 (concat (format "%-8s" (agent-native--ago (plist-get session :time)))
                                         (when several
                                           (format " %-6s" (downcase (agent-native-backend-name (plist-get session :backend)))))
                                         (when (plist-get session :title)
                                           (concat " " (truncate-string-to-width
                                                        (replace-regexp-in-string "\n" " " (or (plist-get session :prompt) ""))
                                                        60 nil nil "…"))))
                                 'face 'completions-annotations)))))
          (choice (completing-read
                   "Resume: "
                   (lambda (string pred action)
                     (if (eq action 'metadata)
                         ;; Newest first, as listed; the date and first prompt beside.
                         `(metadata (display-sort-function . identity)
                                    (cycle-sort-function . identity)
                                    (annotation-function . ,annotate))
                       (complete-with-action action choices string pred)))
                   nil t))
          (session (cdr (assoc choice choices))))
     (list (plist-get session :id) (plist-get session :backend))))
  (let ((backend (or backend agent-native--backend agent-native-backend)))
    ;; Never the buffer at hand unless it is that backend's session:
    ;; opening a session erases it, and a read-only one (dired) would stay
    ;; read-only, input and all.
    (unless (and (derived-mode-p 'agent-native-mode) (eq agent-native--backend backend))
      (let ((directory (agent-native--session-directory)))
        (pop-to-buffer-same-window (or (car (agent-native--session-buffers directory backend))
                                       (get-buffer-create (agent-native--buffer-name backend directory))))
        (setq default-directory directory)))
    (agent-native--open backend session-id)
    (goto-char (point-max))))

;;;; Remote Control

(defun agent-native--remote-changed (props)
  "Keep Remote Control's state from PROPS of a `remote' event (nil: off)."
  (let ((state (plist-get props :state)))
    (setq agent-native--remote state
          agent-native--remote-url (and state (or (plist-get props :url) agent-native--remote-url))))
  (when-let* ((message (plist-get props :message)))
    (message "%s" message))
  (agent-native--update-label))

(defun agent-native-toggle-remote-control ()
  "Turn Remote Control on; when it is on, open or copy its link, or turn it off.
Under Remote Control a Claude session is listed on claude.ai and in the
Claude app: what happens here shows there, and prompts sent there run here."
  (interactive)
  (agent-native--need-agent)
  (cond
   ((not agent-native--remote)
    (agent-native-backend-remote-control agent-native--conn t)
    (message "Remote Control: connecting…"))
   ((not agent-native--remote-url)
    (message "Remote Control: still connecting…"))
   (t
    (pcase (car (read-multiple-choice
                 (format "Remote Control is on · %s" agent-native--remote-url)
                 '((?o "open" "Open the session on claude.ai")
                   (?c "copy" "Copy its link")
                   (?d "disconnect" "Turn Remote Control off")
                   (?q "quit" "Leave it on"))))
      (?o (browse-url agent-native--remote-url))
      (?c (kill-new agent-native--remote-url)
          (message "Copied %s" agent-native--remote-url))
      (?d (agent-native-backend-remote-control agent-native--conn nil))))))

(defun agent-native-clear ()
  "Start over: a new conversation in this buffer, the old one cleared away,
with the same model, effort and permission mode, as the agent's /clear.
\(Sent to the agent, /clear would clear its context but not this buffer.)"
  (interactive)
  (agent-native--reopen)
  (goto-char (point-max)))

(defun agent-native-usage ()
  "Show plan limits, context use and what this session used, as bars, in
a window below; q closes it."
  (interactive)
  (agent-native--need-agent)
  (let ((buffer (current-buffer))
        (name (agent-native--name)))
    (agent-native-backend-usage
     agent-native--conn
     (lambda (report)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (if (stringp report)
               (message "%s" report)
             (agent-native--show-usage name report))))))))

(defun agent-native-exit ()
  "End this session and close its buffer."
  (interactive)
  (agent-native-stop agent-native--conn)
  (kill-buffer (current-buffer)))

(defun agent-native-interrupt ()
  "Stop what the agent is doing."
  (interactive)
  (when (and (agent-native-live-p agent-native--conn) agent-native--busy)
    (setq agent-native--interrupted t)
    (agent-native-backend-interrupt agent-native--conn)))

(defun agent-native-cycle-mode ()
  "Switch to the next permission mode."
  (interactive)
  (agent-native--need-agent)
  (let* ((modes (mapcar #'car (agent-native-backend-modes agent-native--backend)))
         (next (or (cadr (memq agent-native--mode modes)) (car modes))))
    (agent-native-backend-set-mode agent-native--conn next)
    (setq agent-native--mode next)
    (agent-native--refresh-status)))

(defun agent-native--read-model ()
  "Pick a model in the minibuffer, each with its description beside it."
  (let ((names (mapcar (lambda (m) (plist-get m :value)) agent-native--models)))
    (completing-read
     "Model: "
     (lambda (string pred action)
       (if (eq action 'metadata)
           `(metadata (display-sort-function . identity)
                      (annotation-function
                       . ,(lambda (name)
                            (when-let* ((m (agent-native--model-info name)))
                              (concat "  " (propertize (format "%s · %s" (plist-get m :name) (plist-get m :description))
                                                       'face 'completions-annotations))))))
         (complete-with-action action names string pred)))
     ;; With no list from the agent, any name.
     nil (and names t) nil nil
     (and (member agent-native--model names) agent-native--model))))

(defun agent-native--model-info (model)
  "What the agent says about MODEL (a name like \"opus\" or a full id)."
  (seq-find (lambda (m) (member model (plist-get m :aliases))) agent-native--models))

(defun agent-native-set-model (model)
  "Use MODEL from now on."
  (interactive (progn (agent-native--need-agent) (list (agent-native--read-model))))
  (agent-native-backend-set-model agent-native--conn model nil)
  (setq agent-native--model model)
  (agent-native--update-label))

(defun agent-native--effort-levels (&optional model)
  "The effort levels MODEL (default: the current one) supports; nil if none."
  (let ((info (agent-native--model-info (or model agent-native--model))))
    (if info
        (plist-get info :efforts)
      '("low" "medium" "high" "xhigh" "max"))))

(defun agent-native--read-effort (model)
  "Pick an effort level for MODEL; nil for a model without levels. The
default is the one in use, else the model's own."
  (when-let* ((levels (agent-native--effort-levels model)))
    (completing-read (format "Effort for %s: " model) levels nil t nil nil
                     (seq-find (lambda (level) (member level levels))
                               (list agent-native--effort
                                     (plist-get (agent-native--model-info model) :default-effort))))))

(defun agent-native-set-model-and-effort (model level)
  "Use MODEL at effort LEVEL: pick the model, then its effort right after.
LEVEL is nil for a model without effort levels."
  (interactive
   (let ((model (progn (agent-native--need-agent) (agent-native--read-model))))
     (list model (agent-native--read-effort model))))
  (agent-native-backend-set-model agent-native--conn model level)
  ;; Shown at once; the agent corrects them if it differs.
  (setq agent-native--model model)
  (when level (setq agent-native--effort level))
  (agent-native--update-label))

(defun agent-native--read-provider ()
  "Pick who makes the model: Claude or OpenAI, among the installed
backends. This session's comes first; with one backend, no question."
  (let* ((current (and (derived-mode-p 'agent-native-mode) agent-native--backend))
         (backends (seq-filter #'agent-native-backend-available-p agent-native-backends))
         (backends (if (memq current backends) (cons current (remq current backends)) backends))
         (choices (mapcar (lambda (backend) (cons (agent-native-backend-provider backend) backend)) backends)))
    (cond
     ((null choices) (user-error "Neither claude nor codex is installed"))
     ((null (cdr choices)) (cdar choices))
     (t (cdr (assoc (completing-read
                     "Provider: "
                     (lambda (string pred action)
                       (if (eq action 'metadata)
                           `(metadata (display-sort-function . identity)
                                      (annotation-function
                                       . ,(lambda (name)
                                            (let ((backend (cdr (assoc name choices))))
                                              (propertize (format "  %s%s" (agent-native-backend-title backend)
                                                                  (if (eq backend current) " · this session" ""))
                                                          'face 'completions-annotations)))))
                         (complete-with-action action choices string pred)))
                     nil t nil nil (caar choices))
                    choices))))))

(defun agent-native--await-models ()
  "Wait for the agent to list its models (it may be starting)."
  (let ((end (+ (float-time) 30)))
    (unless agent-native--models
      (message "Starting %s…" (agent-native--name)))
    (while (and (not agent-native--models) (agent-native-live-p agent-native--conn) (< (float-time) end))
      (accept-process-output (agent-native-conn-process agent-native--conn) 0.1))
    (unless agent-native--models
      (user-error "%s did not list its models" (agent-native--name)))
    (message nil)))

(defun agent-native-choose-model ()
  "Pick a provider (Claude or OpenAI), then a model, then its effort.
The provider of this session changes its model. The other one opens this
project's session with that agent, started if needed, and sets the model
there: a conversation cannot move between Claude Code and Codex, so this
one stays in its own buffer. Works from any buffer."
  (interactive)
  (let ((backend (agent-native--read-provider)))
    (unless (and (derived-mode-p 'agent-native-mode) (eq agent-native--backend backend)
                 (agent-native-live-p agent-native--conn))
      (agent-native--visit backend (agent-native--session-directory)))
    (agent-native--need-agent)
    (agent-native--await-models)
    (let ((model (agent-native--read-model)))
      (agent-native-set-model-and-effort model (agent-native--read-effort model)))))

(defun agent-native-set-effort (level)
  "Use effort LEVEL from now on (this session)."
  (interactive
   (progn (agent-native--need-agent)
          (list (completing-read (format "Effort (now %s): " (or agent-native--effort "?"))
                                 (agent-native--effort-levels) nil t))))
  (agent-native-backend-set-effort agent-native--conn level))

(defun agent-native-kill ()
  "End this session."
  (interactive)
  (agent-native-stop agent-native--conn))

(defun agent-native-previous-prompt (n)
  "Put the Nth earlier prompt in the input area."
  (interactive "p")
  (let ((index (+ agent-native--history-index n)))
    (when (and (>= index 0) (< index (length agent-native--history)))
      (when (< agent-native--history-index 0)
        (setq agent-native--history-draft (agent-native--input-text)))
      (setq agent-native--history-index index)
      (agent-native--set-input (nth index agent-native--history)))))

(defun agent-native-next-prompt (n)
  "Put the Nth later prompt in the input area; past the newest, the
unsent text that was there before."
  (interactive "p")
  (let ((index (- agent-native--history-index n)))
    (cond ((< agent-native--history-index 0))
          ((< index 0)
           (setq agent-native--history-index -1)
           (agent-native--set-input agent-native--history-draft))
          (t (agent-native-previous-prompt (- n))))))

(defun agent-native--in-input-edge-p (up)
  "Whether point is in the input on its first (UP) or last screen line."
  (and (>= (point) agent-native--input)
       (save-excursion
         ;; The input's field would stop these at its edge.
         (let ((inhibit-field-text-motion t)
               (visual (eq (window-buffer) (current-buffer))))
           (if up
               (progn (if visual (beginning-of-visual-line) (beginning-of-line))
                      (<= (point) agent-native--input))
             (if visual (end-of-visual-line) (end-of-line))
             (>= (point) (point-max)))))))

(defun agent-native-up (n)
  "As in Claude Code: on the input's first line, the previous prompt;
elsewhere, up a line."
  (interactive "p")
  (if (agent-native--in-input-edge-p t)
      (agent-native-previous-prompt n)
    (agent-native-previous-line n)))

(defun agent-native-down (n)
  "On the input's last line, the next prompt; elsewhere, down a line."
  (interactive "p")
  (if (agent-native--in-input-edge-p nil)
      (agent-native-next-prompt n)
    (if (and (bound-and-true-p evil-mode) (fboundp 'evil-next-visual-line))
        (evil-next-visual-line n)
      (line-move n t))))

;;;; Completion: /commands and @files

(defun agent-native-tab ()
  "Take the suggested prompt into the empty input; else complete."
  (interactive)
  (if-let* ((ghost (agent-native--ghost)))
      (progn (agent-native--set-input ghost)
             (goto-char (point-max)))
    (completion-at-point)))

(defun agent-native--complete ()
  "Complete a /command at the start of the input, or an @file anywhere."
  (when (>= (point) agent-native--input)
    (save-excursion
      (let ((end (point)))
        (cond
         ;; A /command is only a command at the very start of the input.
         ((and (looking-back "/[^ \n/]*" agent-native--input)
               (= (match-beginning 0) agent-native--input))
          (list (1+ (match-beginning 0)) end (mapcar #'car agent-native--commands)
                :annotation-function
                (lambda (name) (when-let* ((doc (cdr (assoc name agent-native--commands))))
                                 (concat "  " (truncate-string-to-width doc 60 nil nil "…"))))))
         ((looking-back "@\\([^ \n]*\\)" agent-native--input)
          (list (match-beginning 1) end (agent-native--files))))))))

(defun agent-native--files ()
  "Files to offer after @: the project's, else this directory's."
  (mapcar (lambda (file) (file-relative-name file default-directory))
          (if-let* ((project (project-current)))
              (project-files project)
            (directory-files default-directory t "\\`[^.]"))))

(provide 'agent-native)
;;; agent-native.el ends here
