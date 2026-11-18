;;; agent-native-render.el --- draw the transcript like the terminal UI -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Draws an agent's session into its buffer the way Claude Code's
;; terminal UI does: "❯" for your prompts, "⏺" for the agent's text and tool
;; calls, "⎿" under a tool for its result, diffs for edits, checkboxes for
;; todos. Everything here inserts into the transcript, above the input area,
;; and leaves the input alone.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Faces

(defface agent-native-user '((t :inherit default :extend t))
  "Your prompts in the transcript."
  :group 'agent-native)
(defface agent-native-user-marker '((t :inherit shadow))
  "The \"❯\" before your prompts."
  :group 'agent-native)
(defface agent-native-bullet '((t :inherit default))
  "The \"⏺\" before the agent's text."
  :group 'agent-native)
(defface agent-native-tool-bullet '((t :inherit success))
  "The \"⏺\" before a tool call that worked."
  :group 'agent-native)
(defface agent-native-tool-pending '((t :inherit shadow))
  "The \"⏺\" before a tool call still running or waiting."
  :group 'agent-native)
(defface agent-native-tool-error '((t :inherit error))
  "The \"⏺\" before a tool call that failed, and error text."
  :group 'agent-native)
(defface agent-native-tool-name '((t :inherit shadow :weight bold))
  "A tool's name."
  :group 'agent-native)
(defface agent-native-tool-summary '((t :inherit shadow))
  "What a tool was called with: the text in parentheses after its name."
  :group 'agent-native)
(defface agent-native-tool-result '((t :inherit shadow))
  "A tool's output, quieter than the agent's text so the two tell apart."
  :group 'agent-native)
(defface agent-native-dim '((t :inherit shadow))
  "Quiet text: results, hints, the status line."
  :group 'agent-native)
(defface agent-native-diff-added '((t :inherit diff-refine-added :extend t))
  "Added lines in an edit."
  :group 'agent-native)
(defface agent-native-diff-removed '((t :inherit diff-refine-removed :extend t))
  "Removed lines in an edit."
  :group 'agent-native)
(defface agent-native-code '((t :inherit font-lock-string-face))
  "`code' in the agent's text."
  :group 'agent-native)
(defface agent-native-heading '((t :weight bold))
  "Markdown headings in the agent's text."
  :group 'agent-native)
(defface agent-native-accent '((t :inherit font-lock-function-name-face))
  "The welcome line and the input's \"❯\"."
  :group 'agent-native)

;;;; Format

(defcustom agent-native-format 'markdown
  "What the agent writes its replies in, and how they are drawn.
`markdown' is the agents' own, drawn with the terminal's light look.
With `org', the agent is told to write Org (`agent-native-org-prompt') and
its text looks as it does in your Org buffers: headings, tables, source
blocks, links (RET opens one). Sessions started after a change follow it."
  :type '(choice (const markdown) (const org))
  :group 'agent-native)

(defcustom agent-native-org-prompt
  "Your replies are shown in an Emacs Org mode buffer. Write the text of \
your replies in Org syntax, never Markdown:
- Headings: lines that start with \"* \" or \"** \", never \"#\".
- Emphasis: *bold*, /italic/, ~code~, =verbatim=. No backticks, no **double stars**.
- Code: #+begin_src LANG ... #+end_src (#+begin_example for plain output), never ``` fences.
- Lists: \"- \" or \"1. \" items. A line that starts with \"* \" is a heading, not an item.
- Links: [[https://example.com][description]]. Point at a file with \
[[file:path/to/file.el::42][file.el:42]].
- Tables: Org tables, \"| a | b |\" rows with a \"|---+---|\" rule under the header.
Give a reply the structure you would give it in Markdown, no more: most \
replies need no heading, and a reply does not open with one.
This holds only for what you say to the user. Files you write, commit \
messages, pull requests, code comments and tool inputs keep their own formats."
  "Added to the agent's instructions when `agent-native-format' is `org'."
  :type 'string
  :group 'agent-native)

(defcustom agent-native-input-padding 0
  "Empty space above and below the input, in lines: 0.5 is half a line.
The space above is under the rule. Graphical frames only."
  :type 'number
  :group 'agent-native)

(defun agent-native--input-padding ()
  "`agent-native-input-padding' in pixels; none in a terminal."
  (if (display-graphic-p)
      (round (* agent-native-input-padding (default-font-height)))
    0))

;;;; Transcript state (buffer-local, set up by `agent-native-mode')

(defvar-local agent-native--out nil
  "Marker at the end of the transcript; new output goes here.")
(defvar-local agent-native--tools nil
  "tool_use id -> plist (:name :input :bullet MARKER :end MARKER), a hash
table (a session has many tool calls; each result looks its call up).")

(defun agent-native--tool-table ()
  "This buffer's tool table, made on first use."
  (or agent-native--tools
      (setq agent-native--tools (make-hash-table :test #'equal))))

(defvar-local agent-native--block nil
  "Plist of the content block being streamed: (:type :start MARKER :org),
:org non-nil when its text is drawn as Org.")
(defvar-local agent-native--subagents nil
  "Agent tool_use id -> (REGION . TOOL-COUNT): the line under it showing the
subagent's latest step.")
(defvar-local agent-native--denied nil
  "Ids of tool calls you denied: their result (the agent's long rejection
note) is not drawn; \"Denied\" says it.")

(defconst agent-native--indent "  "
  "Indent under a bullet.")
(defconst agent-native--org-indent
  (propertize " " 'display '(space :align-to 2) 'face '(:height 0.1))
  "The indent drawn before a line of Org, as wide as `agent-native--indent'.
It is short, so a thin line (the rule of a table) stays thin.")
(defconst agent-native--result-max-lines 4
  "Result lines shown under a tool before \"… +N lines\".")

;;;; Inserting

(defun agent-native--protect (start end)
  "Make START..END read-only, with typing allowed right after it.
Nothing is rear-sticky, so text typed after it (the input) stays plain."
  (add-text-properties start end '(read-only t front-sticky (read-only) rear-nonsticky t)))

(defvar-local agent-native--follow-timer nil
  "Pending `agent-native--follow', so a burst of output lays out once.")
(defvar-local agent-native--spacer nil
  "Marker at the spacer: an empty line between transcript and input whose
height pushes the input to the bottom of the window.")

(defun agent-native--set-spacer (pixels)
  "Make the spacer line PIXELS tall (it is never less than one line).
A terminal ignores `line-height', so there the spacer shows as that many
empty lines (a terminal's pixel is a line)."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (end (1+ agent-native--spacer)))
    (if (display-graphic-p)
        (put-text-property agent-native--spacer end 'line-height (and (> pixels 0) pixels))
      (put-text-property agent-native--spacer end 'display
                         (and (> pixels 1) (make-string pixels ?\n))))))

(defun agent-native--spacer-value (&optional pixels)
  "The spacer property `agent-native--set-spacer' gives PIXELS; without
PIXELS, the one it has now."
  (let ((prop (if (display-graphic-p) 'line-height 'display)))
    (cond ((null pixels) (get-text-property agent-native--spacer prop))
          ((display-graphic-p) (and (> pixels 0) pixels))
          (t (and (> pixels 1) (make-string pixels ?\n))))))

(defvar-local agent-native--status-overlay nil
  "Shows the status (the agent working, a permission wait) on the spacer's
first row, under the log: the row the layout always keeps free.")

(defvar-local agent-native--drawing-history nil
  "Non-nil while a resumed session is drawn: lay out once at the end, not
per insert.")

(defun agent-native--bottom (window)
  "Where WINDOW starts when it shows the session's end: (START . SPACER),
the window start and the spacer's height in pixels. Cached until the text
or the window's size changes, so asking before each redisplay is cheap."
  (let ((key (list (buffer-chars-modified-tick) (window-body-height window t)
                   (window-body-width window t) (default-line-height)))
        (cached (window-parameter window 'agent-native-bottom)))
    (if (and cached (equal (car cached) key) (eq (nth 1 cached) (current-buffer)))
        (nthcdr 2 cached)
      (let ((bottom (agent-native--measure window)))
        (set-window-parameter window 'agent-native-bottom
                              (cons key (cons (current-buffer) bottom)))
        bottom))))

(defun agent-native--measure (window)
  "Compute `agent-native--bottom' for WINDOW, like a terminal: the log
from the top, the input on the bottom row, the spacer filling the gap;
once the log is taller than the window, its end just above the input.

Done in pixels: find the topmost screen line from which the log still
fits above the input (with the spacer at least one row), start there, and
give the spacer what is left, so the input ends exactly at the bottom
however tall each line is."
  ;; The status shares the spacer's row: measure without it, or it counts
  ;; twice; and with the spacer one row, as the walk assumes.
  (let* ((overlay agent-native--status-overlay)
         (status (and (overlayp overlay) (overlay-get overlay 'before-string)))
         (spacer (agent-native--spacer-value))
         (inhibit-read-only t)
         (buffer-undo-list t))
    (when status (overlay-put overlay 'before-string nil))
    (agent-native--set-spacer 0)
    (unwind-protect (agent-native--measure-1 window)
      (put-text-property agent-native--spacer (1+ agent-native--spacer)
                         (if (display-graphic-p) 'line-height 'display) spacer)
      (when status (overlay-put overlay 'before-string status)))))

(defun agent-native--measure-1 (window)
  "Compute `agent-native--measure' for WINDOW, the status hidden."
  ;; Selecting WINDOW moves point to WINDOW's point: keep it.
  (save-excursion
    (with-selected-window window
      (let* ((row (save-excursion (goto-char (point-max)) (line-pixel-height)))
             ;; Measuring leaves out the last line's line-spacing; drawing
             ;; does not, so keep that much free.
             (spacing (- row (default-font-height)))
             ;; The padding under the input is space left free at the bottom;
             ;; the line's spacing is already part of it.
             (body (- (window-body-height window t)
                      (max 0 (- (agent-native--input-padding) spacing))))
             (tail (cdr (window-text-pixel-size window (1+ agent-native--spacer) (point-max))))
             ;; Room for the log above the spacer, which is at least a row.
             (room (- body tail spacing row))
             (exact (lambda (start) (cdr (window-text-pixel-size window start agent-native--spacer))))
             ;; Walk up from the spacer a screen line at a time, adding up
             ;; line heights, to find the topmost line from which the log
             ;; still fits. Each line is looked at once: the cost is a
             ;; window's worth however long the session.
             (start (save-excursion
                      (goto-char agent-native--spacer)
                      (let ((best (point))
                            (sum 0))
			(while (and (> (point) (point-min))
                                    (progn (vertical-motion -1)
                                           (setq sum (+ sum (line-pixel-height)))
                                           (<= sum room)))
                          (setq best (point)))
			best)))
             ;; The sum is an estimate; the spacer gets what is exactly left,
             ;; one line lower if the estimate let one line too many in.
             (used (funcall exact start)))
	(while (and (> used room) (< start agent-native--spacer))
          (setq start (save-excursion (goto-char start) (vertical-motion 1) (point))
		used (funcall exact start)))
	(cons start (- body tail spacing used))))))

(defun agent-native--keep-layout (window &optional pin)
  "Hold WINDOW to the terminal layout. With the cursor in the input area
(or PIN), show the session's end: the input on the bottom row. Up in the
log, scroll freely, but never past that end, where only blank space is.
Runs before every redisplay, so no command or scroll can break it."
  (let* ((bottom (agent-native--bottom window))
         (start (car bottom)))
    ;; The spacer is one for all windows: only the main one (the selected,
    ;; else the first) sizes it, or two windows of other heights would
    ;; resize it in turn at every redisplay.
    (when (and (eq window (get-buffer-window (current-buffer) t))
               (not (equal (agent-native--spacer-value)
                           (agent-native--spacer-value (cdr bottom)))))
      (agent-native--set-spacer (cdr bottom)))
    (when (and (or pin
                   (>= (window-point window) agent-native--spacer)
                   (> (window-start window) start))
               (not (and (= (window-start window) start)
                         (zerop (window-vscroll window t)))))
      (set-window-vscroll window 0 t)
      (set-window-start window start t))))

(defun agent-native--layout (window)
  "Lay out WINDOW with the session's end in view."
  (agent-native--keep-layout window t))

(defun agent-native--before-redisplay (window)
  "For `pre-redisplay-functions': keep a session WINDOW laid out."
  (let ((buffer (window-buffer window)))
    (when (and (buffer-local-value 'agent-native--spacer buffer)
               (not (buffer-local-value 'agent-native--drawing-history buffer)))
      (with-current-buffer buffer
        (when (marker-buffer agent-native--spacer)
          (agent-native--keep-layout window))))))

(defun agent-native--show-status (text)
  "Show TEXT (nil for none) as the status line under the log."
  (when agent-native--spacer
    (unless (and (overlayp agent-native--status-overlay)
                 (overlay-buffer agent-native--status-overlay))
      (setq agent-native--status-overlay
            (make-overlay agent-native--spacer agent-native--spacer)))
    (unless (equal text (overlay-get agent-native--status-overlay 'before-string))
      (overlay-put agent-native--status-overlay 'before-string text))))


(defun agent-native--follow ()
  "Lay out each window that shows this session, unless you scrolled up."
  (setq agent-native--follow-timer nil)
  (when (and agent-native--spacer (not agent-native--drawing-history))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      ;; Up in the transcript means reading; anywhere from the spacer down
      ;; (the rule, the ❯, the input) means following.
      (when (>= (window-point window) agent-native--spacer)
        (agent-native--layout window)))))

(defun agent-native--schedule-follow ()
  "Run `agent-native--follow' soon, once for many inserts."
  (unless agent-native--follow-timer
    (let ((buffer (current-buffer)))
      (setq agent-native--follow-timer
            (run-at-time 0.02 nil (lambda ()
                                    (when (buffer-live-p buffer)
                                      (with-current-buffer buffer (agent-native--follow)))))))))

(defun agent-native--shift-undo (pos delta)
  "Shift the undo entries at or after POS by DELTA.
Transcript changes are kept out of undo (so undo never takes the agent's text
away), but they move your input; its recorded edits move with it."
  (when (and (listp buffer-undo-list) (/= delta 0))
    ;; In place: `undo' keeps its place in this list (pending-undo-list,
    ;; undo-equiv-table), and a copy would lose it.
    (let ((cell buffer-undo-list))
      (while (consp cell)
        (let ((e (car cell)))
          (pcase e
            ((pred integerp) (when (>= e pos) (setcar cell (+ e delta))))
            (`(,(and (pred integerp) beg) . ,(and (pred integerp) end))
             (when (>= beg pos) (setcar e (+ beg delta)) (setcdr e (+ end delta))))
            (`(,(pred stringp) . ,(and (pred integerp) at))
             ;; A negative position means point was at the text's end.
             (when (>= (abs at) pos) (setcdr e (if (< at 0) (- at delta) (+ at delta)))))
            (`(nil ,_ ,_ ,(and (pred integerp) beg) . ,(and (pred integerp) end))
             (when (>= beg pos)
               (let ((tail (nthcdr 3 e)))
                 (setcar tail (+ beg delta))
                 (setcdr tail (+ end delta)))))))
        (setq cell (cdr cell))))))

(defmacro agent-native--without-undo (&rest body)
  "Run BODY, a transcript change, with read-only off and undo not recording."
  `(let ((inhibit-read-only t)
         (buffer-undo-list t))
     ,@body))

(defun agent-native--insert-at (marker &rest strings)
  "Insert STRINGS at MARKER as read-only transcript text."
  (let ((start (marker-position marker))
        (size (apply #'+ (mapcar #'length strings))))
    (agent-native--without-undo
     (save-excursion
       (goto-char marker)
       (apply #'insert strings)
       (agent-native--protect start (point))))
    (agent-native--shift-undo start size))
  ;; Lay out now, before the next redisplay: a timer would let one frame
  ;; show the input pushed below the window.
  (agent-native--follow))

(defun agent-native--insert (&rest strings)
  "Insert STRINGS at the end of the transcript."
  (apply #'agent-native--insert-at agent-native--out strings))

(defun agent-native--fresh-line ()
  "Start the next output on a new line, with a blank line before blocks."
  (save-excursion
    (goto-char agent-native--out)
    (unless (bobp)
      (agent-native--insert (if (bolp) "\n" "\n\n")))))

(defun agent-native--indent-lines (text &optional first)
  "TEXT with every line but the first indented; FIRST prefixes the first."
  (concat (or first "") (replace-regexp-in-string "\n" (concat "\n" agent-native--indent) text)))

;;;; Text

(defun agent-native--fontify-markdown (start end)
  "Give START..END the light markdown look the terminal has."
  (agent-native--without-undo
    (save-excursion
      ;; Fenced code blocks first; inline markup does not apply inside them.
      ;; From the line's start: a block may open right after the "⏺ ".
      (goto-char start)
      (beginning-of-line)
      (while (re-search-forward "^\\(?:⏺ \\)? *\\(```+\\).*\n\\(?:.*\n\\)*? *\\1`* *$" end t)
        (let ((block-start (match-beginning 1))
              (block-end (match-end 0)))
          (add-face-text-property block-start block-end 'agent-native-code)
          (put-text-property block-start block-end 'agent-native-code-block t)
          ;; The fences go, each with its newline; the code stays, in its colour.
          (save-excursion
            (goto-char block-start)
            (put-text-property block-start
                               (min block-end
                                    (+ (1+ (line-end-position))
                                       ;; On the "⏺ " line, the code's first line
                                       ;; takes its place: without the indent.
                                       (if (and (> block-start (line-beginning-position))
                                                (string-prefix-p agent-native--indent
                                                                 (buffer-substring (1+ (line-end-position)) (min end (+ (line-end-position) 1 (length agent-native--indent))))))
                                           (length agent-native--indent) 0)))
                               'invisible t)
            (goto-char block-end)
            (put-text-property (line-beginning-position) (min end (1+ block-end)) 'invisible t))))
      ;; Inline markup: style the text, hide the markers (as the terminal
      ;; does). REGEXP's group 1 is the opening marker, 2 the text, 3 the
      ;; closing one.
      (cl-flet ((each (regexp face)
                  ;; From the line's start: the block begins after its "⏺ ".
                  (goto-char start)
                  (beginning-of-line)
                  (while (re-search-forward regexp end t)
                    (if (or (get-text-property (match-beginning 1) 'agent-native-code-block)
                            (get-text-property (match-beginning 2) 'agent-native-code-block))
                        ;; In code: look again just after this opening
                        ;; marker, so a real one later on the line is found.
                        (goto-char (1+ (match-beginning 1)))
                      (add-face-text-property (match-beginning 2) (match-end 2) face)
                      (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
                      (when (match-beginning 3)
                        (put-text-property (match-beginning 3) (match-end 3) 'invisible t)
                        (goto-char (match-end 3)))))))
        (each "\\(`\\)\\([^`\n]+\\)\\(`\\)" 'agent-native-code)
        ;; Inline code is code: no markup inside it (as for code blocks).
        (goto-char start)
        (beginning-of-line)
        (while (re-search-forward "`[^`\n]+`" end t)
          (put-text-property (match-beginning 0) (match-end 0) 'agent-native-code-block t))
        (each "\\(\\*\\*\\)\\([^*\n]+\\)\\(\\*\\*\\)" 'bold)
        ;; *it*: the stars hug the text, and neither touches a word char
        ;; or another star (2*3*4 is maths, f(*a, **kw) is code).
        (each "\\(?:^\\|[^*[:alnum:]_]\\)\\(\\*\\)\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\(\\*\\)\\(?:[^*[:alnum:]_]\\|$\\)"
              'italic)
        ;; A heading may open the block, right after its "⏺ ".
        (each "^\\(?:⏺ \\)? *\\(#+ \\)\\(.*\\)$" 'agent-native-heading)))))

;; Org is loaded on first use, and only for the `org' format.
(defvar org-inhibit-startup)
(defvar org-hide-emphasis-markers)
(defvar org-pretty-entities)
(defvar org-verbatim-re)
(declare-function org-table-map-tables "org-table")
(declare-function org-table-align "org-table")
(declare-function org-link-make-string "ol")
(declare-function org-link-open-from-string "ol")

(defun agent-native--markdown-p (text)
  "Non-nil if TEXT has Markdown's marks and none of Org's: a reply from
before the format was Org, or a slash command's output."
  (require 'org)
  (and (string-match-p "^\\(?:⏺ \\)? *```\\|^#+ \\|\\*\\*[^*\n]+\\*\\*\\|`[^`\n]+`" text)
       (let ((prose (replace-regexp-in-string
                     "`[^`\n]+`" ""
                     (replace-regexp-in-string "^ *```\\(?:.*\n\\)*? *```.*$" "" text))))
         (not (string-match-p (concat "^[ \t]*#\\+\\|\\[\\[\\|^#\\+begin_\\|" org-verbatim-re) prose)))))

(defun agent-native--org-buffer ()
  "The hidden Org buffer the agent's text is fontified in, with your Org setup."
  (let ((buffer (get-buffer-create " *agent-native-org*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        ;; No folding, no inline images: the text only passes through.
        (let ((org-inhibit-startup t)) (org-mode))
        ;; Marks hide, as Markdown's do; snake_case stays, not a subscript.
        (setq-local org-hide-emphasis-markers t
                    org-pretty-entities nil)))
    buffer))

(defvar-keymap agent-native-link-map
  :doc "On a link in the agent's text."
  "<mouse-2>" #'agent-native-open-link-at-mouse
  "<follow-link>" 'mouse-face)

(defun agent-native--pin-display (spec pinned)
  "Display SPEC with each variable local to this buffer replaced by its
value: a spec may size itself by one (org-modern's table rules do), and
is shown in another buffer. PINNED, a hash table, keeps one result per
SPEC, since specs that are `eq' draw as one."
  (cond ((not (consp spec))
         (if (and spec (symbolp spec) (local-variable-p spec) (numberp (symbol-value spec)))
             (symbol-value spec)
           spec))
        ((gethash spec pinned))
        (t (puthash spec (cons (agent-native--pin-display (car spec) pinned)
                               (agent-native--pin-display (cdr spec) pinned))
                    pinned))))

(defun agent-native--org-render (text)
  "TEXT as Org shows it: tables aligned; faces, hidden marks and link
targets as text properties."
  (with-current-buffer (agent-native--org-buffer)
    ;; Major modes of source blocks report as they start; not here.
    (let ((inhibit-message t)
          (message-log-max nil))
      (erase-buffer)
      (insert text)
      (org-table-map-tables #'org-table-align t)
      ;; This buffer is never displayed: what its modes work out before
      ;; each redisplay (org-modern, the font's width) is worked out here.
      (dolist (function pre-redisplay-functions)
        (unless (eq function t) (funcall function (selected-window))))
      ;; Not `font-lock-ensure': it may take a reused buffer for done.
      (font-lock-fontify-region (point-min) (point-max)))
    (let ((drawn (buffer-substring-no-properties (point-min) (point-max)))
          (pinned (make-hash-table :test #'eq))
          (pos (point-min)))
      (while (< pos (point-max))
        (let* ((next (next-property-change pos nil (point-max)))
               (from (- pos (point-min)))
               (to (- next (point-min)))
               (display (get-text-property pos 'display))
               (link (plist-get (get-text-property pos 'htmlize-link) :uri)))
          (dolist (prop '(face invisible))
            (when-let* ((value (get-text-property pos prop)))
              (put-text-property from to prop value drawn)))
          (when display
            (put-text-property from to 'display (agent-native--pin-display display pinned) drawn))
          (when (stringp link)
            (add-text-properties from to (list 'agent-native-link link 'help-echo link
                                               'mouse-face 'highlight
                                               'keymap agent-native-link-map)
                                 drawn))
          (setq pos next)))
      drawn)))

(defun agent-native--fontify-org (start end)
  "Give START..END, text the agent wrote in Org, the look Org gives it."
  (let* ((start (if (markerp start) (marker-position start) start))
         (text (buffer-substring-no-properties start end))
         (drawn (agent-native--org-render text))
         (new-end (+ start (length drawn)))
         (pos 0))
    (agent-native--without-undo
     ;; Aligning a table changed the text: change only what differs, so
     ;; the cursor and markers stay where they are. (A function is the
     ;; one form of this argument both Emacs 30 and 31 take.)
     (unless (string= text drawn)
       (replace-region-contents start end (lambda () (substring-no-properties drawn))))
     (while (< pos (length drawn))
       (let ((next (or (next-property-change pos drawn) (length drawn))))
         (add-text-properties (+ start pos) (+ start next) (text-properties-at pos drawn))
         (setq pos next)))
     (put-text-property start new-end 'line-prefix agent-native--org-indent)
     (agent-native--protect start new-end))
    (agent-native--shift-undo start (- (length drawn) (length text)))))

(defun agent-native-open-link ()
  "Open the link at point in the agent's text."
  (interactive)
  (when-let* ((link (get-text-property (point) 'agent-native-link)))
    (org-link-open-from-string (org-link-make-string link))))

(defun agent-native-open-link-at-mouse (event)
  "Open the link clicked on (EVENT)."
  (interactive "e")
  (posn-set-point (event-end event))
  (agent-native-open-link))

(defun agent-native--text-start ()
  "Begin a block of the agent's text."
  (agent-native--fresh-line)
  (agent-native--insert (propertize "⏺ " 'face 'agent-native-bullet))
  (setq agent-native--block (list :type 'text :start (copy-marker agent-native--out)
                                   :org (eq agent-native-format 'org))))

(defun agent-native--text-delta (text)
  "Add streamed TEXT to the current text block."
  (agent-native--insert
   (if (plist-get agent-native--block :org)
       ;; Org reads columns (a heading's star is in the first), so the
       ;; indent is drawn, not typed.
       (propertize text 'line-prefix agent-native--org-indent)
     (agent-native--indent-lines text))))

(defun agent-native--block-stop ()
  "Finish the streamed block: give text its Org or markdown look."
  (when (eq (plist-get agent-native--block :type) 'text)
    (let ((start (plist-get agent-native--block :start)))
      (cond
       ((string-blank-p (buffer-substring-no-properties start agent-native--out))
        ;; Nothing was written: take the bullet away again.
        (let ((from (save-excursion (goto-char start) (forward-line 0)
                                    (skip-chars-backward "\n") (point))))
          (agent-native--shift-undo from (- from agent-native--out))
          (agent-native--without-undo (delete-region from agent-native--out))))
       ((and (plist-get agent-native--block :org)
               (not (agent-native--markdown-p
                     (buffer-substring-no-properties start agent-native--out))))
          ;; Your Org setup and the modes of source blocks run in there: a
          ;; failure leaves the text as written, and the session going.
        (with-demoted-errors "agent-native: drawing Org failed: %S"
          (agent-native--fontify-org start agent-native--out)))
       (t (agent-native--fontify-markdown start agent-native--out)))))
  (when-let* ((start (plist-get agent-native--block :start)))
    (set-marker start nil))
  (setq agent-native--block nil))

(defun agent-native--text-whole (text)
  "Draw a whole block of the agent's TEXT (output that was not streamed)."
  (agent-native--text-start)
  (agent-native--text-delta (string-trim-right text))
  (agent-native--block-stop))

;;;; Your prompts and notes

(defun agent-native--user (text)
  "Draw a prompt you sent."
  (agent-native--fresh-line)
  (let ((start (marker-position agent-native--out)))
    (agent-native--insert (propertize "❯ " 'face 'agent-native-user-marker)
                           (agent-native--indent-lines text))
    (agent-native--without-undo
     (add-face-text-property start agent-native--out 'agent-native-user))))

(defun agent-native--note (text &optional face)
  "Draw a quiet line, e.g. the welcome line or a status change."
  (agent-native--fresh-line)
  (agent-native--insert (propertize text 'face (or face 'agent-native-dim))))

;;;; Tool calls

(defun agent-native--relative (path)
  "PATH relative to the session directory when it lies inside it."
  (if (and (stringp path) (string-prefix-p (expand-file-name default-directory) (expand-file-name path)))
      (file-relative-name path default-directory)
    path))

(defun agent-native--first-line (text)
  "The first line of TEXT, shortened."
  (let ((line (car (split-string (or text "") "\n"))))
    (truncate-string-to-width line 100 nil nil "…")))

(defun agent-native--tool-summary (name input)
  "What goes in the parentheses after tool NAME, from its INPUT."
  (let ((get (lambda (key) (alist-get key input))))
    (pcase name
      ((or "Bash" "Shell") (agent-native--first-line (funcall get 'command)))
      ((or "Read" "Write" "Edit" "MultiEdit" "NotebookEdit" "Delete")
       (if-let* ((files (funcall get 'files)))
           (mapconcat #'agent-native--relative files ", ")
         (or (agent-native--relative (or (funcall get 'file_path) (funcall get 'notebook_path))) "")))
      ((or "Grep" "Glob" "Search")
       (agent-native--first-line (format "%s" (or (funcall get 'pattern) (funcall get 'command) ""))))
      ("List" (or (agent-native--relative (funcall get 'path)) (agent-native--first-line (funcall get 'command))))
      ("WebFetch" (agent-native--first-line (funcall get 'url)))
      ("WebSearch" (agent-native--first-line (funcall get 'query)))
      ((or "Task" "Agent") (agent-native--first-line (funcall get 'description)))
      ("TodoWrite" "")
      ;; The (first) question asked.
      ("AskUserQuestion"
       (let ((questions (funcall get 'questions)))
         (agent-native--first-line
          (or (and (seqp questions) (> (length questions) 0)
                   (alist-get 'question (elt questions 0)))
              ""))))
      (_ (let ((first (car-safe input)))
           (if (consp first) (agent-native--first-line (format "%s" (cdr first))) ""))))))

(defun agent-native--tool-use (id name input)
  "Draw tool call ID: \"⏺ NAME(summary)\", waiting for its result."
  (agent-native--fresh-line)
  (let ((bullet (copy-marker agent-native--out)))
    (agent-native--insert (propertize "⏺" 'face 'agent-native-tool-pending) " "
                           (propertize (if (equal name "TodoWrite") "Update Todos" name) 'face 'agent-native-tool-name)
                           (let ((summary (agent-native--tool-summary name input)))
                             (if (string-empty-p summary) ""
                               (propertize (format "(%s)" summary) 'face 'agent-native-tool-summary))))
    (let ((end (copy-marker agent-native--out)))
      (puthash id (list :name name :input input :bullet bullet :end end) (agent-native--tool-table))
      (when (equal name "TodoWrite")
        (agent-native--under end (agent-native--todo-lines (alist-get 'todos input)))))))

(defun agent-native--fill-face (text face)
  "Give the parts of TEXT that have no face of their own FACE."
  (let ((pos 0))
    (while pos
      (let ((next (next-single-property-change pos 'face text)))
        (unless (get-text-property pos 'face text)
          (put-text-property pos (or next (length text)) 'face face text))
        (setq pos next))))
  text)

(defun agent-native--under (end lines &optional face keep)
  "Draw LINES (strings) under a tool line that ends at marker END.
FACE is for the text with no face of its own: the ⎿ and plain lines.
With KEEP, return the drawn region as a cons of markers, to remove it
later (markers cost every later insert a little, so only when asked)."
  (when lines
    (let* ((text (concat "\n" agent-native--indent "⎿  " (car lines)
                         (mapconcat (lambda (line) (concat "\n" agent-native--indent "   " line)) (cdr lines) "")))
           ;; Wrapped lines line up under the result, past the ⎿.
           (text (propertize text 'wrap-prefix (concat agent-native--indent "   ")))
           (start (marker-position end)))
      (agent-native--insert-at end (if face (agent-native--fill-face text face) text))
      ;; Later lines under the same tool go after these.
      (set-marker end (+ end (length text)))
      (when keep (cons (copy-marker start) (copy-marker end))))))

(defun agent-native--remove (region)
  "Remove REGION, a cons of markers from `agent-native--under', and free them."
  (when (and region (marker-position (car region)))
    (let ((start (marker-position (car region)))
          (size (- (cdr region) (car region))))
      (agent-native--without-undo (delete-region (car region) (cdr region)))
      (agent-native--shift-undo start (- size))
      (set-marker (car region) nil)
      (set-marker (cdr region) nil))))

(defun agent-native--todo-lines (todos)
  "Checkbox lines for TODOS, as the terminal shows them."
  (mapcar (lambda (todo)
            (let ((status (alist-get 'status todo))
                  (text (alist-get 'content todo)))
              (pcase status
                ("completed" (propertize (concat "☒ " text) 'face '(:inherit agent-native-dim :strike-through t)))
                ("in_progress" (propertize (concat "◐ " text) 'face 'bold))
                (_ (concat "☐ " text)))))
          todos))

(defun agent-native--unified-diff-lines (diff)
  "The lines of unified DIFF as an edit shows them: removed and added
lines coloured, context plain, headers left out."
  (delq nil
        (mapcar (lambda (line)
                  (cond ((string-match-p "\\`\\(?:@@\\|diff \\|index \\|--- \\|\\+\\+\\+ \\|\\\\\\)" line) nil)
                        ((string-prefix-p "+" line)
                         (propertize (concat "+ " (substring line 1)) 'face 'agent-native-diff-added))
                        ((string-prefix-p "-" line)
                         (propertize (concat "- " (substring line 1)) 'face 'agent-native-diff-removed))
                        (t (concat "  " (string-remove-prefix " " line)))))
                (split-string (string-trim-right diff "\n") "\n"))))

(defun agent-native--diff-lines (old new)
  "Lines showing OLD replaced by NEW, as a small diff."
  (append (mapcar (lambda (line) (propertize (concat "- " line) 'face 'agent-native-diff-removed))
                  (split-string old "\n"))
          (mapcar (lambda (line) (propertize (concat "+ " line) 'face 'agent-native-diff-added))
                  (split-string new "\n"))))

(defun agent-native--result-text (content)
  "Tool result CONTENT (a string or a vector of blocks) as one string."
  (string-replace
   "\r" ""
   (cond ((stringp content) content)
         ((vectorp content)
          (mapconcat (lambda (block)
                       (pcase (alist-get 'type block)
                         ("image" "[Image]")
                         ("document" "[Document]")
                         (_ (or (alist-get 'text block) ""))))
                     (seq-remove (lambda (block) (equal (alist-get 'text block) "")) content)
                     "\n"))
         (t ""))))

(defconst agent-native--result-max-width 400
  "Characters of one result line shown; minified code is one huge line,
and a line that long makes Emacs slow to draw.")

(defun agent-native--clip-lines (lines)
  "LINES, at most `agent-native--result-max-lines' of them, each cut to
`agent-native--result-max-width'; a count of the rest; RET on it shows them."
  (let* ((lines (mapcar (lambda (line)
                          (if (> (length line) agent-native--result-max-width)
                              (concat (substring line 0 agent-native--result-max-width)
                                      (propertize (format "… (%d more characters)" (- (length line) agent-native--result-max-width))
                                                  'face 'agent-native-dim))
                            line))
                        lines))
         (shown (seq-take lines agent-native--result-max-lines))
         (rest (nthcdr agent-native--result-max-lines lines)))
    (if rest
        (append shown (list (propertize (format "… +%d lines (RET to expand)" (length rest))
                                        'face 'agent-native-dim
                                        'agent-native-more rest)))
      shown)))

(defun agent-native--clip (text &optional face)
  "TEXT's first lines, with a count of the rest; RET on the count shows them.
The lines, shown or not, are in FACE (default `agent-native-tool-result')."
  (agent-native--clip-lines
   (mapcar (lambda (line) (propertize line 'face (or face 'agent-native-tool-result)))
           (split-string (string-trim-right text) "\n"))))

(defun agent-native-expand ()
  "Show all the lines a \"… +N lines\" stands for, in its place."
  (interactive)
  (when-let* ((rest (get-text-property (point) 'agent-native-more)))
    (let* ((start (or (previous-single-property-change (1+ (point)) 'agent-native-more) (point)))
           (end (or (next-single-property-change (point) 'agent-native-more) (line-end-position)))
           (text (mapconcat #'identity rest (concat "\n" agent-native--indent "   "))))
      (agent-native--without-undo
       (save-excursion
         (goto-char start)
         (delete-region start end)
         (insert text)
         (agent-native--protect start (point))))
      (agent-native--shift-undo start (- (length text) (- end start))))))

(defun agent-native--draw-tool-result (id content is-error)
  "Draw the result of tool call ID under it, and colour its bullet."
  (let* ((tool (gethash id (agent-native--tool-table)))
         (name (plist-get tool :name))
         (input (plist-get tool :input))
         (text (agent-native--result-text content)))
    ;; An agent's progress line goes when it ends, however it ends.
    (when (member name '("Agent" "Task"))
      (unless (string-prefix-p "Async agent launched" text)
        (agent-native--remove (car (alist-get id agent-native--subagents nil nil #'equal)))))
    (when (and tool (not (member id agent-native--denied)))
      (let ((bullet (plist-get tool :bullet)))
        (agent-native--without-undo
         (put-text-property bullet (1+ bullet) 'face
                           (if is-error 'agent-native-tool-error 'agent-native-tool-bullet))))
      (agent-native--under
       (plist-get tool :end)
       (cond
        (is-error (agent-native--clip text 'agent-native-tool-error))
        ((and (equal name "Edit") (alist-get 'diff input))
         (agent-native--clip-lines (agent-native--unified-diff-lines (alist-get 'diff input))))
        ((equal name "Edit")
         (agent-native--clip-lines
          (agent-native--diff-lines (or (alist-get 'old_string input) "") (or (alist-get 'new_string input) ""))))
        ((equal name "Delete") (list "Deleted"))
        ((equal name "MultiEdit")
         (agent-native--clip-lines
          (apply #'append (mapcar (lambda (edit)
                                    (agent-native--diff-lines (or (alist-get 'old_string edit) "")
                                                               (or (alist-get 'new_string edit) "")))
                                  (alist-get 'edits input)))))
        ((equal name "Write")
         (let ((lines (split-string (string-remove-suffix "\n" (or (alist-get 'content input) "")) "\n")))
           (list (format "Wrote %d line%s to %s" (length lines) (if (= (length lines) 1) "" "s")
                         (agent-native--relative (alist-get 'file_path input))))))
        ((and (equal name "Read") (string-match-p "\\`\\[Image\\]" text))
         (list "Read image"))
        ((equal name "Read")
         ;; Claude's Read numbers the file's lines ("   12→text") and may
         ;; add notes after; a shell read (Codex) is the file as it is.
         (let ((n (if (alist-get 'command input)
                      (length (split-string (string-trim-right text "\n") "\n"))
                    (seq-count (lambda (line) (string-match-p "\\`[ \t]*[0-9]+[→\t]" line))
                               (split-string text "\n")))))
           (list (format "Read %d line%s" n (if (= n 1) "" "s")))))
        ((equal name "TodoWrite") nil)
        ;; "· question → answer", as the terminal shows answers.
        ((equal name "AskUserQuestion")
         (let (pairs (start 0))
           ;; "question"="answer", either with \" inside.
           (while (string-match "\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)+\\)\"=\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"" text start)
             (push (format "· %s → %s" (string-replace "\\\"" "\"" (match-string 1 text))
                           (string-replace "\\\"" "\"" (match-string 2 text)))
                   pairs)
             (setq start (match-end 0)))
           (or (nreverse pairs) (agent-native--clip text))))
        ;; An agent's result is wrapped for the model; the reply that
        ;; follows says what it found. Here: how it went, as the terminal.
        ((member name '("Agent" "Task"))
         (let ((entry (alist-get id agent-native--subagents nil nil #'equal)))
           (if (string-prefix-p "Async agent launched" text)
               (list (propertize "Running in the background" 'face 'agent-native-dim))
             ;; Redrawn from history, the steps were never counted.
             (list (propertize (if (cdr entry)
                                   (format "Done (%d tool use%s)" (cdr entry) (if (eql (cdr entry) 1) "" "s"))
                                 "Done")
                               'face 'agent-native-dim)))))
        ((string-empty-p (string-trim text)) (list (propertize "(no output)" 'face 'agent-native-dim)))
        (t (agent-native--clip text)))
       'agent-native-tool-result))))

(defun agent-native--tool-result (id content is-error)
  "Draw the result of tool call ID, then let go of the call's markers
\(each costs every later insert a little). A background agent's call
stays: its steps keep arriving after it."
  (agent-native--draw-tool-result id content is-error)
  (when-let* ((tool (gethash id (agent-native--tool-table))))
    (unless (string-prefix-p "Async agent launched" (agent-native--result-text content))
      (set-marker (plist-get tool :bullet) nil)
      (set-marker (plist-get tool :end) nil)
      (remhash id (agent-native--tool-table)))))

(defun agent-native--tool-note (id text &optional face)
  "Draw TEXT under tool call ID; return its region (see `agent-native--remove')."
  (when-let* ((tool (gethash id (agent-native--tool-table))))
    (agent-native--under (plist-get tool :end) (list text) (or face 'agent-native-dim) t)))

(provide 'agent-native-render)
;;; agent-native-render.el ends here
