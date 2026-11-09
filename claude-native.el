;;; claude-native.el --- Claude Code, drawn natively in Emacs -*- lexical-binding: t -*-

;; Version: 0.1.0
;; URL: https://github.com/joachimvelde/claude-native
;; Keywords: tools, ai
;; Package-Requires: ((emacs "30.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; `M-x claude-native' opens a Claude Code session for the current project:
;; the transcript on top, an input area at the bottom, and a status line,
;; like the terminal UI. It runs claude headless and draws everything here.
;;
;; In the input area:
;;   RET          send (S-RET or C-j for a new line)
;;   C-c C-c      interrupt Claude
;;   S-TAB        cycle permission mode (default, accept edits, plan, bypass)
;;   C-c C-m      pick a model, then its effort level
;;   C-c C-e      pick an effort level
;;   M-p / M-n    earlier / later prompts
;;   TAB          complete /commands and @files
;;   C-c C-p      answer a permission request you put off
;;   C-c /        run a slash command, picked from a list
;;
;; /resume, /model and /exit open native pickers instead of being sent.
;;
;; Files: claude-native-process.el (the pipe), claude-native-render.el
;; (drawing), claude-native-history.el (past sessions). Tests in test/,
;; recordings of real sessions in dev/.

;;; Code:

(require 'claude-native-process)
(require 'claude-native-render)
(require 'claude-native-history)
(require 'claude-native-attach)
(require 'project)

(defcustom claude-native-model nil
  "Model to start sessions with (e.g. \"opus\"), or nil for claude's default."
  :type '(choice (const nil) string)
  :group 'claude-native)

(defcustom claude-native-permission-mode "bypassPermissions"
  "Permission mode sessions start in.
\"bypassPermissions\" runs every tool without asking."
  :type '(choice (const "bypassPermissions") (const "default")
                 (const "acceptEdits") (const "plan"))
  :group 'claude-native)

(defcustom claude-native-permission-function #'claude-native--ask-permission
  "Function deciding a permission request.
Called with the request alist; returns one of `allow', `always', `deny'."
  :type 'function
  :group 'claude-native)

;;;; Session state (buffer-local)

(defvar-local claude-native--conn nil "The claude connection.")
(defvar-local claude-native--input nil "Marker at the start of the input area.")
(defvar-local claude-native--busy nil "Non-nil while Claude works: when it started.")
(defvar-local claude-native--status "" "What Claude is doing, for the status line.")
(defvar-local claude-native--model nil "The model in use.")
(defvar-local claude-native--effort nil "The effort level in use.")
(defvar-local claude-native--mode claude-native-permission-mode "The permission mode.")
(defvar-local claude-native--cost 0 "Total cost in USD so far.")
(defvar-local claude-native--session-id nil "The session's id, for resuming.")
(defvar-local claude-native--commands nil "Slash commands: name -> description.")
(defvar-local claude-native--command-hints nil
  "Slash commands that take arguments: name -> hint (e.g. \"<file>\").")
(defvar-local claude-native--models nil "Models claude offers: list of alists.")
(defvar-local claude-native--pending nil "Permission requests not yet answered.")
(defvar-local claude-native--history nil "Prompts sent, newest first.")
(defvar-local claude-native--history-index -1 "Where M-p/M-n are in the history.")
(defvar-local claude-native--spinner nil "Timer animating the status line.")
(defvar-local claude-native--thinking-tokens 0 "Estimated thinking tokens this turn.")
(defvar-local claude-native--interrupted nil "Non-nil after C-c C-c, until the turn ends.")
(defvar-local claude-native--queued nil
  "Prompts sent but not yet taken up by claude (it echoes each when it does).")
(defvar-local claude-native--quiet nil
  "Messages sent for you (e.g. /effort from a picker) whose echo is not drawn.")
(defvar-local claude-native--drawn nil
  "Prompts drawn when sent (Claude was idle), whose echo is then skipped.")
(defvar-local claude-native--suggestion nil
  "Claude Code's guess at your next prompt, shown dim in the empty input.")
(defvar-local claude-native--asking nil "Non-nil while a permission question is open.")
(defvar-local claude-native--waiting-notes nil
  "request_id -> region of its \"Waiting for permission…\" note.")

(defconst claude-native--spinner-frames ["✻" "✽" "✶" "✳" "✢" "·" "✢" "✳" "✶" "✽"])
(defconst claude-native--modes '("default" "acceptEdits" "plan" "bypassPermissions")
  "Permission modes S-TAB cycles through.")
(defconst claude-native--mode-labels
  '(("default" . "") ("acceptEdits" . "⏵⏵ accept edits on") ("plan" . "⏸ plan mode on")
    ("bypassPermissions" . "⏵⏵ bypass permissions on")))

;;;; The buffer

(defun claude-native--rule ()
  "The line above the input. In a window system, a hairline in a row a few
pixels tall; in a terminal, an underlined row."
  (if (display-graphic-p)
      ;; A 1px underline; a background would fill the whole (9px) line.
      (concat (propertize " " 'display '(space :align-to right)
                          'face '(:height 0.1 :underline (:color foreground-color :position t)
                                  :inherit claude-native-dim))
              ;; The line's newline: tiny, and without the frame's line-spacing,
              ;; so the line is as thin as the hairline.
              (propertize "\n" 'face '(:height 0.1) 'line-spacing 0))
    (concat (propertize " " 'display '(space :align-to right)
                        'face '(:inherit claude-native-dim :underline t))
            "\n")))

(defun claude-native--label ()
  "The model and effort, as the terminal shows them."
  (string-join (delq nil (list (and claude-native--model
                                    (string-remove-prefix "claude-" claude-native--model))
                               (and claude-native--effort (concat "◐ " claude-native--effort))))
               " · "))

(defvar-local claude-native--label-overlay nil
  "Overlay on the input that shows the model and effort at its right.")

(defun claude-native--ghost ()
  "The suggestion to show in the input: when it is empty, else nil."
  (and claude-native--suggestion
       (not (string-empty-p claude-native--suggestion))
       (string-empty-p (claude-native--input-text))
       claude-native--suggestion))

(defun claude-native--label-fits-p (label)
  "Whether LABEL fits after the input's last line, in the window showing it.
When it does not, Emacs draws it glued to the wrapped text, so it hides."
  (let* ((window (or (get-buffer-window) (selected-window)))
         (last (or (claude-native--ghost)
                   (car (last (split-string (claude-native--input-text) "\n"))))))
    ;; 2 for the ❯ (or wrap prefix), 1 for a gap.
    (< (+ 2 (string-width last) 1 (string-width label))
       (window-max-chars-per-line window))))

(defun claude-native--update-label ()
  "Show the current model and effort at the right end of the input line,
when there is room for them."
  (let* ((label (claude-native--label))
         (ghost (claude-native--ghost))
         (shown (concat
                 ;; The suggestion, with the cursor at its start (as typed
                 ;; text would be): TAB takes it.
                 (when ghost
                   (concat (propertize (substring ghost 0 1) 'face 'claude-native-dim 'cursor t)
                           (propertize (substring ghost 1) 'face 'claude-native-dim)))
                 (unless (or (string-empty-p label) (not (claude-native--label-fits-p label)))
                   ;; `cursor': draw the cursor before the label, at the
                   ;; input's end, not after it at the window's edge.
                   (concat (propertize " " 'display `(space :align-to (- right ,(1+ (string-width label))))
                                       'cursor (not ghost))
                           (propertize label 'face 'claude-native-dim))))))
    (unless (overlayp claude-native--label-overlay)
      ;; Grows with the input (rear-advance), so the label stays after it.
      (setq claude-native--label-overlay
            (make-overlay claude-native--input (point-max) nil nil t)))
    (unless (equal shown (overlay-get claude-native--label-overlay 'after-string))
      (overlay-put claude-native--label-overlay 'after-string shown))))

(defvar-local claude-native--input-edited nil
  "Non-nil when the input changed during the current command.")

(defun claude-native--input-changed (beg end _len)
  "Typing changed the input from BEG to END: keep it plain text (a paste
from the transcript brings its heights and faces), and show or hide the
label as room allows."
  (when (and claude-native--input (>= beg claude-native--input))
    (when (< beg end)
      (with-silent-modifications (set-text-properties beg end nil)))
    ;; Lay out again once the command is done: the input may have gained
    ;; or lost rows, and the log must move to keep it at the bottom.
    (setq claude-native--input-edited t)
    (claude-native--update-label)))

(defun claude-native--after-input-edit ()
  "After a command that changed the input, lay out before redisplay."
  (when claude-native--input-edited
    (setq claude-native--input-edited nil)
    ;; An operator over transcript and input (gg V G d) edits only the
    ;; input but leaves the cursor up top: bring it to the edit.
    (when (< (point) claude-native--input)
      (goto-char claude-native--input))
    (claude-native--follow)))

(defun claude-native--refresh-settings ()
  "Ask claude which model and effort it uses now, and show them."
  (let ((buffer (current-buffer)))
    (claude-native-control
     claude-native--conn "get_settings" nil
     (lambda (response)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((applied (alist-get 'applied (alist-get 'response response))))
             (setq claude-native--model (or (alist-get 'model applied) claude-native--model)
                   claude-native--effort (or (alist-get 'effort applied) claude-native--effort))
             (claude-native--update-label))))))))

(defvar-keymap claude-native-mode-map
  "RET" #'claude-native-send
  "S-<return>" #'newline
  "C-j" #'newline
  "C-c C-c" #'claude-native-interrupt
  "<backtab>" #'claude-native-cycle-mode
  "C-c C-m" #'claude-native-set-model-and-effort
  "C-c C-e" #'claude-native-set-effort
  "C-c C-p" #'claude-native-answer-pending
  "C-c C-k" #'claude-native-kill
  "C-c /" #'claude-native-command
  "C-c C-a" #'claude-native-attach
  "M-p" #'claude-native-previous-prompt
  "M-n" #'claude-native-next-prompt
  "TAB" #'claude-native-tab
  "<remap> <previous-line>" #'claude-native-previous-line
  "<remap> <evil-previous-line>" #'claude-native-previous-line
  "<remap> <evil-previous-visual-line>" #'claude-native-previous-line
  ;; The key itself: visual-line-mode's own remap would win over a remap.
  "<home>" #'claude-native-beginning-of-line
  "<remap> <evil-open-below>" #'claude-native-open-line
  "<remap> <evil-open-above>" #'claude-native-open-line
  "<remap> <move-beginning-of-line>" #'claude-native-beginning-of-line
  "<remap> <beginning-of-visual-line>" #'claude-native-beginning-of-line
  "<remap> <evil-beginning-of-line>" #'claude-native-beginning-of-line
  "<remap> <evil-beginning-of-visual-line>" #'claude-native-beginning-of-line
  "<remap> <evil-first-non-blank>" #'claude-native-beginning-of-line
  "<remap> <evil-first-non-blank-of-visual-line>" #'claude-native-beginning-of-line)

(defun claude-native-beginning-of-line ()
  "To the line's start; on the input's first line, to the input's start
\(the ❯ before it is not a place the cursor may be)."
  (interactive)
  (let ((start (if (fboundp 'beginning-of-visual-line)
                   (save-excursion (beginning-of-visual-line) (point))
                 (line-beginning-position))))
    (goto-char (if (and (>= (point) claude-native--input) (< start claude-native--input))
                   claude-native--input
                 start))))

(defun claude-native-previous-line (&optional n)
  "Move up N lines; from the input's first line, to the transcript's end.
\(Line by line, the cursor would land in the strip above the input, which
it may not enter, and be put back.)"
  (interactive "p")
  (if (and (>= (point) claude-native--input)
           (<= (line-beginning-position) claude-native--input))
      (goto-char claude-native--out)
    (if (and (bound-and-true-p evil-mode) (fboundp 'evil-previous-visual-line))
        (evil-previous-visual-line (or n 1))
      (line-move (- (or n 1))))))

(define-derived-mode claude-native-mode fundamental-mode "Claude"
  "A Claude Code session: transcript above, input below."
  (setq-local truncate-lines nil
              word-wrap t
              ;; Wrapped lines line up under the text, past the bullet.
              wrap-prefix claude-native--indent
              buffer-undo-list t
              completion-at-point-functions (list #'claude-native--complete)
              ;; Status goes under the log (`claude-native--refresh-status'),
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
    (insert "\n\n" (claude-native--rule) (propertize "❯ " 'face 'claude-native-accent))
    (claude-native--protect (point-min) (point))
    ;; The cursor never stops in this strip (spacer, rule, ❯): from the
    ;; input, up or left goes straight to the transcript's end. The mode
    ;; reads stickiness, so the property sticks forward from the strip's
    ;; second char; the first ends the transcript's last line, which stays
    ;; reachable.
    (put-text-property (point-min) (point) 'cursor-intangible t)
    (put-text-property (1+ (point-min)) (point) 'front-sticky '(read-only cursor-intangible))
    (setq claude-native--out (copy-marker (point-min) t)
          claude-native--spacer (copy-marker (1+ (point-min)))
          claude-native--input (point-max-marker)))
  ;; Typing always goes to the input: entering insert state (evil) or
  ;; typing (without it) while up in the transcript jumps to the input's end.
  (add-hook 'evil-insert-state-entry-hook #'claude-native--to-input nil t)
  (add-hook 'pre-command-hook #'claude-native--typing-to-input nil t)
  ;; Killing the buffer ends its claude.
  (add-hook 'kill-buffer-hook (lambda () (claude-native-stop claude-native--conn)) nil t)
  ;; A resized window lays out again.
  ;; The hook runs with the window as argument, not in its buffer.
  (add-hook 'window-size-change-functions
            (lambda (window)
              (with-current-buffer (window-buffer window)
                (claude-native--follow)
                (claude-native--update-label)))
            nil t)
  (add-hook 'after-change-functions #'claude-native--input-changed nil t)
  ;; Every redisplay holds the layout (see `claude-native--keep-layout'),
  ;; whatever moved the window: j, G, C-f, zt, the mouse wheel.
  (add-hook 'pre-redisplay-functions #'claude-native--before-redisplay)
  (add-hook 'post-command-hook #'claude-native--after-input-edit nil t)
  ;; With evil, leaving insert mode keeps the cursor in the input rather
  ;; than stepping back onto the ❯.
  (setq-local evil-move-cursor-back nil
              evil-move-beyond-eol t)
  (setq buffer-undo-list nil))

;; Evil's line operators (dd, cc, S) on the input would take the read-only
;; ❯ before it and fail: a range reaching into the input starts at the
;; input instead, and is plain text, not whole lines.
(defun claude-native--clamp-to-input (args)
  "Clamp an evil operator's ARGS (BEG END TYPE ...) to the input."
  (if (and (derived-mode-p 'claude-native-mode)
           (>= (nth 1 args) claude-native--input)
           (< (nth 0 args) claude-native--input))
      `(,(marker-position claude-native--input) ,(nth 1 args)
        ,(if (eq (nth 2 args) 'line) 'exclusive (nth 2 args)) ,@(nthcdr 3 args))
    args))

(defun claude-native--clamp-yank (args)
  "Clamp evil's yank ARGS (BEG END TYPE ...) that start on the ❯, so yy
in the input copies what you typed, not the prompt."
  (if (and (derived-mode-p 'claude-native-mode)
           (< (nth 0 args) claude-native--input)
           (>= (nth 0 args) (save-excursion (goto-char claude-native--input) (line-beginning-position))))
      `(,(marker-position claude-native--input) ,(max (nth 1 args) claude-native--input)
        ,(if (eq (nth 2 args) 'line) 'exclusive (nth 2 args)) ,@(nthcdr 3 args))
    args))

(declare-function evil-get-register "evil-common")
(defvar evil-state)
(defun claude-native--paste (orig after count &optional register yank-handler)
  "Evil's p (AFTER) and P (ORIG, with COUNT, REGISTER, YANK-HANDLER): in the input,
paste as plain text where the cursor is. Lines cut with dd keep no newline,
so the input does not grow a row, and nothing lands before the ❯."
  (if (not (derived-mode-p 'claude-native-mode))
      (funcall orig count register yank-handler)
    (when-let* ((text (if register (evil-get-register register) (current-kill 0))))
      (let ((text (string-trim-right (substring-no-properties text) "\n")))
        (claude-native--to-input)
        (when (< (point) claude-native--input) (goto-char claude-native--input))
        (when (and after (< (point) (point-max)) (not (eolp))) (forward-char))
        (dotimes (_ (prefix-numeric-value count)) (insert text))
        (when (and (bound-and-true-p evil-mode) (eq evil-state 'normal)
                   (> (point) claude-native--input))
          (backward-char))))))

(defun claude-native--paste-after (orig &rest args)
  (apply #'claude-native--paste orig t args))
(defun claude-native--paste-before (orig &rest args)
  (apply #'claude-native--paste orig nil args))

(with-eval-after-load 'evil
  (advice-add 'evil-delete :filter-args #'claude-native--clamp-to-input)
  (advice-add 'evil-change :filter-args #'claude-native--clamp-to-input)
  (advice-add 'evil-yank :filter-args #'claude-native--clamp-yank)
  (advice-add 'evil-paste-after :around #'claude-native--paste-after)
  (advice-add 'evil-paste-before :around #'claude-native--paste-before))

;; With evil: RET sends from normal state too, and sessions open in insert.
(declare-function evil-define-key* "evil-core")
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-insert-state "evil-states")
(declare-function evil-open-above "evil-commands")
(declare-function evil-open-below "evil-commands")
(with-eval-after-load 'evil
  (evil-define-key* 'normal claude-native-mode-map (kbd "RET") #'claude-native-send)
  (evil-set-initial-state 'claude-native-mode 'insert))

(defun claude-native-open-line (count)
  "Evil's o and O: in the input, open a line as usual; up in the read-only
transcript, go to the input's end to type there."
  (interactive "p")
  (if (< (point) claude-native--input)
      (progn (goto-char (point-max)) (evil-insert-state))
    (if (eq this-original-command 'evil-open-above)
        (evil-open-above count)
      (evil-open-below count))))

(defun claude-native--to-input ()
  "Move to the input's end if the cursor is up in the transcript."
  (when (< (point) claude-native--input)
    (goto-char (point-max))))

(defun claude-native--typing-to-input ()
  "Before a typing command up in the transcript, move to the input."
  (when (memq this-command '(self-insert-command yank yank-pop newline))
    (claude-native--to-input)))

(defun claude-native--input-text ()
  "What is typed in the input area."
  (buffer-substring-no-properties claude-native--input (point-max)))

(defun claude-native--set-input (text)
  "Replace the input area with TEXT."
  (let ((inhibit-read-only t))
    (delete-region claude-native--input (point-max))
    (goto-char (point-max))
    (insert text)))

;;;; Status line

(defun claude-native--refresh-status ()
  "Redraw the status line under the log."
  (claude-native--show-status (claude-native--status-line)))

(defun claude-native--status-line ()
  "The status under the log: only what needs you, else nil.
Claude working, a permission request, a mode other than default."
  (let* ((mode (alist-get claude-native--mode claude-native--mode-labels nil nil #'equal))
         (parts (delq nil
                      (list (when claude-native--busy
                              (propertize (format "%s %s (%ds%s)"
                                                  (aref claude-native--spinner-frames
                                                        (mod (truncate (* 8 (float-time))) (length claude-native--spinner-frames)))
                                                  claude-native--status
                                                  (truncate (- (float-time) claude-native--busy))
                                                  (if (> claude-native--thinking-tokens 0)
                                                      (format " · %d thinking tokens" claude-native--thinking-tokens) ""))
                                          'face 'claude-native-accent))
                            (when claude-native--pending
                              (propertize "permission needed: C-c C-p" 'face 'warning))
                            (when claude-native--queued
                              (format "%d queued" (length claude-native--queued)))
                            (when (and mode (not (string-empty-p mode))) mode)
                            (unless (claude-native-live-p claude-native--conn) "ended")))))
    (when parts
      (concat (propertize (string-join parts "  ·  ") 'face 'claude-native-dim)))))

(defun claude-native--set-busy (busy &optional status)
  "Mark the session BUSY (or idle), saying STATUS, and run the spinner."
  (setq claude-native--status (or status "Working…"))
  (cond
   ((and busy (not claude-native--busy))
    (setq claude-native--busy (float-time)
          claude-native--thinking-tokens 0)
    (let ((buffer (current-buffer))
          timer)
      (setq timer (run-at-time 0 0.12 (lambda ()
                                        (if (buffer-live-p buffer)
                                            (with-current-buffer buffer (claude-native--refresh-status))
                                          (cancel-timer timer))))
            claude-native--spinner timer)))
   ((not busy)
    (setq claude-native--busy nil)
    (when claude-native--spinner (cancel-timer claude-native--spinner))
    (setq claude-native--spinner nil)))
  (claude-native--refresh-status))

;;;; Messages from claude

(defun claude-native--handle (buffer msg)
  "Draw message MSG from claude in session BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((type (alist-get 'type msg)))
        (if (alist-get 'parent_tool_use_id msg)
            ;; A subagent's work: one line under the call that started it.
            (claude-native--subagent msg)
        (pcase type
          ("stream_event" (claude-native--stream (alist-get 'event msg)))
          ("assistant" (claude-native--assistant (alist-get 'message msg)))
          ("user" (claude-native--user-message (alist-get 'message msg) (eq (alist-get 'isReplay msg) t)))
          ("control_cancel_request" (claude-native--drop-request (alist-get 'request_id msg)))
          ("system" (claude-native--system msg))
          ("result" (claude-native--result msg))
          ("prompt_suggestion"
           (setq claude-native--suggestion (alist-get 'suggestion msg))
           (claude-native--update-label))
          ("control_request" (claude-native--control-request msg))
          (_ nil)))))))

(defun claude-native--subagent (msg)
  "Show a subagent's tool call as one updating line under its Agent call,
as the terminal does; its text and streamed output stay out of the log."
  (when (equal (alist-get 'type msg) "assistant")
    (let ((parent (alist-get 'parent_tool_use_id msg)))
      (seq-doseq (block (alist-get 'content (alist-get 'message msg)))
        (when (equal (alist-get 'type block) "tool_use")
          (let* ((entry (alist-get parent claude-native--subagents nil nil #'equal))
                 (count (1+ (or (cdr entry) 0)))
                 (name (alist-get 'name block))
                 (summary (claude-native--tool-summary name (alist-get 'input block)))
                 (line (format "%s%s · %d tool use%s" name
                               (if (string-empty-p summary) "" (format "(%s)" summary))
                               count (if (= count 1) "" "s"))))
            (claude-native--remove (car entry))
            (setf (alist-get parent claude-native--subagents nil nil #'equal)
                  (cons (claude-native--tool-note parent line) count))))))))

(defun claude-native--stream (event)
  "Draw a stream EVENT: text arrives here as it is written."
  (pcase (alist-get 'type event)
    ("message_start"
     ;; Only the latest messages can still arrive whole; keep a few ids.
     (setq claude-native--streamed
           (seq-take (cons (alist-get 'id (alist-get 'message event)) claude-native--streamed) 20)))
    ("content_block_start"
     (let ((block (alist-get 'content_block event)))
       (pcase (alist-get 'type block)
         ("text" (claude-native--set-busy t "Writing…") (claude-native--text-start))
         ("thinking" (claude-native--set-busy t "Thinking…"))
         ("tool_use" (claude-native--set-busy t (format "%s…" (alist-get 'name block)))))))
    ("content_block_delta"
     (let ((delta (alist-get 'delta event)))
       (when (equal (alist-get 'type delta) "text_delta")
         (claude-native--text-delta (alist-get 'text delta)))))
    ("content_block_stop" (claude-native--block-stop))))

(defun claude-native--assistant (message)
  "Draw a whole assistant MESSAGE: tool calls, and text not streamed."
  (let ((streamed (member (alist-get 'id message) claude-native--streamed)))
    (seq-doseq (block (alist-get 'content message))
      (pcase (alist-get 'type block)
        ("tool_use" (claude-native--tool-use (alist-get 'id block) (alist-get 'name block) (alist-get 'input block)))
        ("text" (unless streamed (claude-native--text-whole (alist-get 'text block))))))))

(defun claude-native--user-message (message &optional replay)
  "Draw a user MESSAGE claude echoes: your prompt as it takes it up
\(REPLAY), tool results, interruptions."
  (let ((content (alist-get 'content message)))
    (cond
     ((and replay (claude-native--prompt-text content))
      (claude-native--prompt-taken (claude-native--prompt-text content)))
     ((stringp content) (claude-native--user-note content))
     (t
      (seq-doseq (block content)
        (pcase (alist-get 'type block)
          ("tool_result"
           (claude-native--tool-result (alist-get 'tool_use_id block) (alist-get 'content block)
                                       (eq (alist-get 'is_error block) t)))
          ("text" (claude-native--user-note (alist-get 'text block)))))))))

(defun claude-native--prompt-taken (text)
  "Claude took up prompt TEXT: draw it (unless sent quietly for you) and
start the turn's spinner."
  (cond
   ((member text claude-native--quiet)
    (setq claude-native--quiet (claude-native--remove-first text claude-native--quiet)))
   ((member text claude-native--drawn)
    (setq claude-native--drawn (claude-native--remove-first text claude-native--drawn)))
   (t
    (setq claude-native--queued (claude-native--remove-first text claude-native--queued))
    (claude-native--user text)
    (setq claude-native--busy nil)
    (claude-native--set-busy t "Working…"))))

(defun claude-native--remove-first (item list)
  "LIST without its first ITEM (by `equal')."
  (let ((tail (member item list)))
    (if tail (append (butlast list (length tail)) (cdr tail)) list)))

(defun claude-native--user-note (text)
  "Draw TEXT claude sent as the user's, e.g. an interruption."
  (when (string-prefix-p "[Request interrupted" text)
    (claude-native--note (concat claude-native--indent "⎿  Interrupted · What should Claude do instead?")
                         'claude-native-tool-error)))

(defun claude-native--system (msg)
  "Take what claude reports about itself: model, mode, thinking."
  (pcase (alist-get 'subtype msg)
    ("init"
     (setq claude-native--model (alist-get 'model msg)
           claude-native--session-id (alist-get 'session_id msg)
           claude-native--mode (or (alist-get 'permissionMode msg) claude-native--mode)))
    ("status"
     (when-let* ((mode (alist-get 'permissionMode msg)))
       (setq claude-native--mode mode)))
    ("thinking_tokens"
     (setq claude-native--thinking-tokens (or (alist-get 'estimated_tokens msg) 0))))
  (claude-native--refresh-status))

(defun claude-native--result (msg)
  "A turn ended: show how long it took; check model and effort again
(a /model or /effort may have changed them)."
  (setq claude-native--drawn nil)
  ;; Prompts still queued: their turns come next, so keep the spinner.
  (unless claude-native--queued (claude-native--set-busy nil))
  ;; Requests the turn left unanswered are dead now.
  (dolist (pending claude-native--pending)
    (claude-native--drop-request (alist-get 'request_id pending)))
  (claude-native--refresh-settings)
  (setq claude-native--cost (or (alist-get 'total_cost_usd msg) claude-native--cost))
  ;; An interrupted turn ends as an error; \"Interrupted\" already says so.
  (when (and (eq (alist-get 'is_error msg) t) (not claude-native--interrupted))
    (claude-native--note (format "Error: %s" (or (alist-get 'result msg) (alist-get 'subtype msg)))
                         'claude-native-tool-error))
  (let ((ms (alist-get 'duration_ms msg)))
    (when (and ms (> ms 1000) (not (eq (alist-get 'is_error msg) t)))
      (claude-native--note (format "✻ Worked for %ds" (round ms 1000)))))
  (setq claude-native--interrupted nil))

;;;; Permission requests

(defun claude-native--control-request (msg)
  "Answer a control request from claude: permission to use a tool."
  (let ((request (alist-get 'request msg)))
    (when (equal (alist-get 'subtype request) "can_use_tool")
      (setq claude-native--pending (append claude-native--pending (list msg)))
      (push (cons (alist-get 'request_id msg)
                  (claude-native--tool-note (alist-get 'tool_use_id request) "Waiting for permission…"))
            claude-native--waiting-notes)
      (claude-native--refresh-status)
      ;; Out of the process filter, so the question may use the minibuffer.
      (let ((buffer (current-buffer)))
        (run-at-time 0 nil (lambda () (when (buffer-live-p buffer)
                                        (with-current-buffer buffer (claude-native-answer-pending t)))))))))

(defun claude-native--ask-permission (request)
  "Ask in the minibuffer whether Claude may run REQUEST's tool."
  (let* ((name (alist-get 'tool_name request))
         (summary (claude-native--tool-summary name (alist-get 'input request)))
         (prompt (format "Allow %s%s? " name (if (string-empty-p summary) "" (format "(%s)" summary))))
         (answer (read-multiple-choice
                  prompt
                  '((?y "yes" "Run it this time")
                    (?a "always" "Yes, and don't ask again for this in this session")
                    (?n "no" "No, and tell Claude what to do differently")))))
    (pcase (car answer) (?y 'allow) (?a 'always) (?n 'deny))))

(defun claude-native--ask-questions (request)
  "Answer an AskUserQuestion REQUEST: each question in the minibuffer."
  (let* ((input (alist-get 'input request))
         (answers (mapcar (lambda (question)
                            (let* ((labels (mapcar (lambda (option) (alist-get 'label option))
                                                   (alist-get 'options question)))
                                   (choice (completing-read (concat (alist-get 'question question) " ")
                                                            labels)))
                              (cons (intern (alist-get 'question question)) choice)))
                          (alist-get 'questions input))))
    (append input `((answers . ,answers)))))

(defun claude-native--drop-request (request-id)
  "Forget permission request REQUEST-ID (answered, cancelled, or its turn
over) and remove its \"Waiting for permission…\" note."
  (setq claude-native--pending
        (seq-remove (lambda (m) (equal (alist-get 'request_id m) request-id)) claude-native--pending))
  (claude-native--remove (alist-get request-id claude-native--waiting-notes nil nil #'equal))
  (setq claude-native--waiting-notes (assoc-delete-all request-id claude-native--waiting-notes))
  (claude-native--refresh-status))

(defun claude-native-answer-pending (&optional quietly)
  "Answer the oldest permission request, one question at a time.
C-g leaves it waiting. QUIETLY (from the timer): say nothing when none waits."
  (interactive)
  (let ((msg (car claude-native--pending)))
    (cond
     (claude-native--asking nil)
     ((null msg) (unless quietly (message "No permission request is waiting")))
     (t
      (let* ((request (alist-get 'request msg))
             (id (alist-get 'request_id msg))
             (tool-id (alist-get 'tool_use_id request))
             (decision (let ((claude-native--asking t))
                         (condition-case nil
                             (if (equal (alist-get 'tool_name request) "AskUserQuestion")
                                 (claude-native--ask-questions request)
                               (funcall claude-native-permission-function request))
                           (quit nil)))))
        ;; Asked, but cancelled or ended meanwhile: nothing to answer.
        (when (and decision (seq-find (lambda (m) (equal (alist-get 'request_id m) id)) claude-native--pending))
          (claude-native-respond
           claude-native--conn id
           (pcase decision
             ('allow `((behavior . "allow") (updatedInput . ,(alist-get 'input request))))
             ('always `((behavior . "allow") (updatedInput . ,(alist-get 'input request))
                        (updatedPermissions . ,(or (alist-get 'permission_suggestions request) []))))
             ('deny `((behavior . "deny") (message . "The user said no.") (interrupt . t)))
             ;; AskUserQuestion: the input with the answers added.
             (_ `((behavior . "allow") (updatedInput . ,decision)))))
          ;; The waiting note goes; only a denial stays, as in the terminal.
          (claude-native--drop-request id)
          (when (eq decision 'deny)
            ;; Saying no also interrupts the turn, which ends as an error;
            ;; "Denied" and "Interrupted" say enough.
            (push tool-id claude-native--denied)
            (setq claude-native--interrupted t)
            (claude-native--tool-note tool-id "Denied" 'claude-native-tool-error))
          ;; The next request, asked fresh (not inside this one).
          (when claude-native--pending
            (let ((buffer (current-buffer)))
              (run-at-time 0 nil (lambda () (when (buffer-live-p buffer)
                                              (with-current-buffer buffer (claude-native-answer-pending t)))))))))))))

;;;; Commands

(defun claude-native--project-root ()
  "The directory a session for here should run in."
  (if-let* ((project (project-current))) (project-root project) default-directory))

(defun claude-native--buffer-name (directory)
  "The session buffer's name for DIRECTORY."
  (format "*claude: %s*" (abbreviate-file-name (directory-file-name directory))))

;;;###autoload
(defun claude-native (&optional continue)
  "Open the Claude Code session for this project, starting it if needed.
With a prefix argument CONTINUE, resume the last conversation here."
  (interactive "P")
  (let* ((directory (claude-native--project-root))
         (buffer (get-buffer-create (claude-native--buffer-name directory))))
    (with-current-buffer buffer
      (unless (claude-native-live-p claude-native--conn)
        (setq default-directory directory)
        (claude-native--open (and continue (plist-get (car (claude-native--sessions directory)) :id)))))
    ;; In this window, like opening a file; no split.
    (pop-to-buffer-same-window buffer)
    (goto-char (point-max))))

;;;###autoload
(defun claude-native-new ()
  "Start another Claude Code session for this project, in a new buffer
named like the first with a <2>, <3> suffix."
  (interactive)
  (let* ((directory (claude-native--project-root))
         (buffer (generate-new-buffer (claude-native--buffer-name directory))))
    (with-current-buffer buffer
      (setq default-directory directory)
      (claude-native--open))
    (pop-to-buffer-same-window buffer)
    (goto-char (point-max))))

(defun claude-native-continue ()
  "Open this project's session, resuming its last conversation.
The same as `claude-native' with a prefix, for setups where C-u scrolls."
  (interactive)
  (claude-native t))

(defun claude-native--open (&optional session-id)
  "Start this buffer's session afresh, or resume SESSION-ID with its
conversation drawn first."
  (claude-native-stop claude-native--conn)
  (claude-native-mode)
  (claude-native--note (format "✻ Claude Code  ·  %s" (abbreviate-file-name default-directory))
                       'claude-native-accent)
  (when session-id
    (claude-native--draw-history (claude-native--session-file default-directory session-id))
    (claude-native--note "↑ resumed" 'claude-native-dim))
  (claude-native--start session-id))

(defun claude-native--start (&optional session-id)
  "Start claude for this buffer; SESSION-ID resumes that session."
  (let ((buffer (current-buffer)))
    (setq claude-native--conn
          (claude-native-start
           :directory default-directory
           ;; The allow flag lets S-TAB reach bypass from any start mode.
           :args (append (list "--allow-dangerously-skip-permissions"
                               "--permission-mode" claude-native-permission-mode)
                         (when claude-native-model (list "--model" claude-native-model))
                         (when session-id (list "--resume" session-id)))
           :on-message (lambda (msg) (claude-native--handle buffer msg))
           :on-exit (lambda (event)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (claude-native--set-busy nil)
                          (claude-native--note (format "Claude exited (%s). M-x claude-native starts again." event)
                                               'claude-native-tool-error))))))
    (claude-native-control claude-native--conn "initialize" nil
                           (lambda (response)
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (claude-native--initialized (alist-get 'response response))))))))

(defconst claude-native--extra-commands
  '(("cost" . "Show usage and what this session cost")
    ("resume" . "Resume a past session of this directory")
    ("exit" . "End this session"))
  "Commands that work but the handshake does not list.")

(defun claude-native--initialized (info)
  "Keep what the handshake INFO says: commands, models, mode."
  (setq claude-native--commands
        (append (mapcar (lambda (command) (cons (alist-get 'name command) (alist-get 'description command)))
                        (alist-get 'commands info))
                (seq-remove (lambda (extra) (seq-find (lambda (c) (equal (alist-get 'name c) (car extra)))
                                                      (alist-get 'commands info)))
                            claude-native--extra-commands))
        claude-native--models (append (alist-get 'models info) nil))
  (setq claude-native--command-hints
        (delq nil (mapcar (lambda (command)
                            (let ((hint (alist-get 'argumentHint command)))
                              (and (stringp hint) (not (string-empty-p hint))
                                   (cons (alist-get 'name command) hint))))
                          (alist-get 'commands info))))
  (when-let* ((mode (alist-get 'current_permission_mode info)))
    (setq claude-native--mode mode))
  (claude-native--refresh-settings)
  (claude-native--refresh-status))

(defun claude-native-send ()
  "Send what is in the input area; on a \"… +N lines\", expand it instead."
  (interactive)
  (if (get-text-property (point) 'claude-native-more)
      (claude-native-expand)
    (claude-native--send-input)))

(defun claude-native--send-input ()
  "Send what is in the input area."
  (unless (claude-native-live-p claude-native--conn)
    (user-error "Claude is not running; M-x claude-native starts it"))
  (let ((text (string-trim (claude-native--input-text))))
    (if-let* ((native (claude-native--native-command text)))
        (progn (claude-native--set-input "") (call-interactively native))
      (claude-native--send-text text))))

(defun claude-native--send-text (text)
  "Send TEXT as your next prompt and draw it."
  (unless (string-empty-p text)
    ;; Read attached files first: a missing one stops the send, input kept.
    (let ((content (claude-native--prompt-content text)))
      (push text claude-native--history)
      (setq claude-native--suggestion nil
            claude-native--history-index -1)
      (claude-native--set-input "")
      ;; Idle: draw it now (claude echoes it only once the answer starts).
      ;; Working: draw it on the echo (see `claude-native--prompt-taken'), so
      ;; it lands after the current turn, not inside its text.
      (if claude-native--busy
          (setq claude-native--queued (append claude-native--queued (list text)))
	(claude-native--user text)
	(setq claude-native--drawn (append claude-native--drawn (list text))))
      ;; The sent input's undo history is of no use now, and a short list
      ;; keeps shifting it (see `claude-native--shift-undo') cheap.
      (setq buffer-undo-list nil)
      (unless claude-native--busy (claude-native--set-busy t "Working…"))
      (claude-native--refresh-status)
      (claude-native-send-user claude-native--conn content))))

(defun claude-native--send-quietly (text)
  "Send TEXT for you (a picker's /effort): its echo is not drawn."
  (push text claude-native--quiet)
  (claude-native-send-user claude-native--conn text))

;;;; Slash commands as Emacs commands

(defconst claude-native--native-commands
  '(("resume" . claude-native-resume)
    ("model" . claude-native-set-model)
    ("effort" . claude-native-set-effort)
    ("clear" . claude-native-clear)
    ("exit" . claude-native-exit)
    ("quit" . claude-native-exit))
  "Slash commands whose terminal dialogs have an Emacs version here.")

(defun claude-native--native-command (text)
  "The Emacs command for TEXT if it is a slash command done natively."
  (when (string-match "\\`/\\([^ \n]+\\)\\s-*\\'" text)
    (cdr (assoc (match-string 1 text) claude-native--native-commands))))

(defun claude-native-command (name &optional args)
  "Run slash command NAME with ARGS, picked from the commands Claude offers."
  (interactive
   (let* ((names (mapcar #'car claude-native--commands))
          (name (completing-read
                 "Command: "
                 (lambda (string pred action)
                   (if (eq action 'metadata)
                       `(metadata (annotation-function
                                   . ,(lambda (n) (when-let* ((doc (cdr (assoc n claude-native--commands))))
                                                    (concat "  " (truncate-string-to-width doc 70 nil nil "…"))))))
                     (complete-with-action action names string pred)))
                 nil t)))
     ;; Ask for arguments only when the command takes them.
     (list name (when-let* ((hint (cdr (assoc name claude-native--command-hints))))
                  (read-string (format "/%s %s: " name hint))))))
  (if-let* ((native (cdr (assoc name claude-native--native-commands))))
      (call-interactively native)
    (claude-native--send-text (string-trim (concat "/" name " " (or args ""))))))

(defun claude-native--ago (time)
  "TIME as the terminal shows it: \"5m ago\", \"3h ago\", \"2d ago\"."
  (let ((seconds (float-time (time-subtract nil time))))
    (cond ((< seconds 3600) (format "%dm ago" (max 1 (round seconds 60))))
          ((< seconds 86400) (format "%dh ago" (round seconds 3600)))
          (t (format "%dd ago" (round seconds 86400))))))

(defun claude-native-resume (session-id)
  "Resume SESSION-ID, a past session of this directory, in this buffer.
Sessions are listed by name, newest first, with when and how they began."
  (interactive
   (let* ((sessions (or (claude-native--sessions default-directory)
                        (user-error "No past sessions here")))
          ;; A session's name, else its first prompt's first line; names may
          ;; repeat, so a number keeps choices apart.
          (choices (let ((seen (make-hash-table :test #'equal)))
                     (mapcar (lambda (session)
                               ;; Not cut short: every word stays searchable.
                               (let* ((name (string-trim (or (plist-get session :title)
                                                             (car (split-string (plist-get session :prompt) "\n")))))
                                      (n (cl-incf (gethash name seen 0))))
                                 (cons (if (= n 1) name (format "%s <%d>" name n)) session)))
                             sessions)))
          ;; When it was, and how a named session began.
          (annotate (lambda (choice)
                      (when-let* ((session (cdr (assoc choice choices))))
                        (concat (propertize " " 'display '(space :align-to 58))
                                (propertize (concat (format "%-8s" (claude-native--ago (plist-get session :time)))
                                                    (when (plist-get session :title)
                                                      (concat " " (truncate-string-to-width
                                                                   (replace-regexp-in-string "\n" " " (plist-get session :prompt))
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
                   nil t)))
     (list (plist-get (cdr (assoc choice choices)) :id))))
  (claude-native--open session-id)
  (goto-char (point-max)))

(defun claude-native-clear ()
  "Start over: a new conversation in this buffer, the old one cleared away.
\(Sent to claude, /clear would clear its context but not this buffer.)"
  (interactive)
  (claude-native--open)
  (goto-char (point-max)))

(defun claude-native-exit ()
  "End this session and close its buffer."
  (interactive)
  (claude-native-stop claude-native--conn)
  (kill-buffer (current-buffer)))

(defun claude-native-interrupt ()
  "Stop what Claude is doing."
  (interactive)
  (when (and (claude-native-live-p claude-native--conn) claude-native--busy)
    (setq claude-native--interrupted t)
    (claude-native-control claude-native--conn "interrupt")))

(defun claude-native-cycle-mode ()
  "Switch to the next permission mode."
  (interactive)
  (let* ((rest (cdr (member claude-native--mode claude-native--modes)))
         (next (or (car rest) (car claude-native--modes))))
    (claude-native-control claude-native--conn "set_permission_mode" `((mode . ,next)))
    (setq claude-native--mode next)
    (claude-native--refresh-status)))

(defun claude-native--read-model ()
  "Pick a model in the minibuffer, each with its description beside it."
  (let ((names (or (mapcar (lambda (m) (alist-get 'value m)) claude-native--models)
                   '("opus" "sonnet" "haiku" "fable"))))
    (completing-read
     "Model: "
     (lambda (string pred action)
       (if (eq action 'metadata)
           `(metadata (display-sort-function . identity)
                      (annotation-function
                       . ,(lambda (name)
                            (when-let* ((m (seq-find (lambda (m) (equal (alist-get 'value m) name)) claude-native--models)))
                              (concat "  " (propertize (format "%s · %s" (alist-get 'displayName m) (alist-get 'description m))
                                                       'face 'completions-annotations))))))
         (complete-with-action action names string pred)))
     nil t)))

(defun claude-native--model-info (model)
  "What claude says about MODEL (a name like \"opus\" or a full id)."
  (seq-find (lambda (m) (member model (list (alist-get 'value m) (alist-get 'resolvedModel m))))
            claude-native--models))

(defun claude-native-set-model (model)
  "Use MODEL from now on."
  (interactive (list (claude-native--read-model)))
  (let ((buffer (current-buffer)))
    (claude-native-control claude-native--conn "set_model" `((model . ,model))
                           (lambda (_) (when (buffer-live-p buffer)
                                         (with-current-buffer buffer (claude-native--refresh-settings))))))
  (setq claude-native--model model)
  (claude-native--update-label))

(defun claude-native--effort-levels (&optional model)
  "The effort levels MODEL (default: the current one) supports; nil if none."
  (let ((info (claude-native--model-info (or model claude-native--model))))
    (if info
        (and (eq (alist-get 'supportsEffort info) t)
             (append (alist-get 'supportedEffortLevels info) nil))
      '("low" "medium" "high" "xhigh" "max"))))

(defun claude-native-set-model-and-effort (model level)
  "Use MODEL at effort LEVEL: pick the model, then its effort right after.
LEVEL is nil for a model without effort levels."
  (interactive
   (let* ((model (claude-native--read-model))
          (levels (claude-native--effort-levels model)))
     (list model (when levels
                   (completing-read (format "Effort for %s: " model) levels nil t)))))
  ;; Effort after the model: a new model may bring its own default effort.
  (let ((buffer (current-buffer)))
    (claude-native-control claude-native--conn "set_model" `((model . ,model))
                           (lambda (_)
                             ;; The response arrives in whatever buffer is current.
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (if level
                                     (claude-native--send-quietly (concat "/effort " level))
                                   (claude-native--refresh-settings)))))))
  (setq claude-native--model model)
  (claude-native--update-label))

(defun claude-native-set-effort (level)
  "Use effort LEVEL from now on (this session)."
  (interactive
   (list (completing-read (format "Effort (now %s): " (or claude-native--effort "?"))
                          (claude-native--effort-levels) nil t)))
  ;; /effort is how claude takes it; sent quietly, not as a prompt of yours.
  ;; Its one-line confirmation shows, and the label updates after.
  (claude-native--send-quietly (concat "/effort " level)))

(defun claude-native-kill ()
  "End this session."
  (interactive)
  (claude-native-stop claude-native--conn))

(defun claude-native-previous-prompt (n)
  "Put the Nth earlier prompt in the input area."
  (interactive "p")
  (let ((index (+ claude-native--history-index n)))
    (when (and (>= index 0) (< index (length claude-native--history)))
      (setq claude-native--history-index index)
      (claude-native--set-input (nth index claude-native--history)))))

(defun claude-native-next-prompt (n)
  "Put the Nth later prompt in the input area."
  (interactive "p")
  (let ((index (- claude-native--history-index n)))
    (if (< index 0)
        (progn (setq claude-native--history-index -1) (claude-native--set-input ""))
      (claude-native-previous-prompt (- n)))))

;;;; Completion: /commands and @files

(defun claude-native-tab ()
  "Take the suggested prompt into the empty input; else complete."
  (interactive)
  (if-let* ((ghost (claude-native--ghost)))
      (progn (claude-native--set-input ghost)
             (goto-char (point-max)))
    (completion-at-point)))

(defun claude-native--complete ()
  "Complete a /command at the start of the input, or an @file anywhere."
  (when (>= (point) claude-native--input)
    (save-excursion
      (let ((end (point)))
        (cond
         ;; A /command is only a command at the very start of the input.
         ((and (looking-back "/[^ \n/]*" claude-native--input)
               (= (match-beginning 0) claude-native--input))
          (list (1+ (match-beginning 0)) end (mapcar #'car claude-native--commands)
                :annotation-function
                (lambda (name) (when-let* ((doc (cdr (assoc name claude-native--commands))))
                                 (concat "  " (truncate-string-to-width doc 60 nil nil "…"))))))
         ((looking-back "@\\([^ \n]*\\)" claude-native--input)
          (list (match-beginning 1) end (claude-native--files))))))))

(defun claude-native--files ()
  "Files to offer after @: the project's, else this directory's."
  (mapcar (lambda (file) (file-relative-name file default-directory))
          (if-let* ((project (project-current)))
              (project-files project)
            (directory-files default-directory t "\\`[^.]"))))

(provide 'claude-native)
;;; claude-native.el ends here
