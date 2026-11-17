;;; claude-native-history.el --- past sessions: list and redraw them -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Claude Code keeps each session as JSON lines under
;; ~/.claude/projects/<the directory, with every non-alphanumeric char a
;; dash>/<session id>.jsonl. This file lists a directory's sessions for
;; /resume, by name, and draws a session's conversation back into the
;; buffer, through the same drawing as live output.
;;
;; A session's name, as the terminal's /resume shows it: the title set with
;; /rename (custom-title lines), else the one Claude Code generated
;; (ai-title lines), else the first prompt. The lines repeat; the last wins.

;;; Code:

(require 'claude-native-render)
(require 'claude-native-attach)

(defvar claude-native--history)
(declare-function claude-native--user-note "claude-native")
(require 'json)
(require 'seq)
(require 'cl-lib)

(defun claude-native--sessions-dir (directory)
  "Where Claude Code keeps the sessions of DIRECTORY.
As Claude Code names it: every character but a letter or digit becomes
\"-\" (one per UTF-16 unit, so two for an emoji), and a name over 200
characters is cut there and given a hash, found here by its prefix."
  (let* ((name (replace-regexp-in-string
                "[^a-zA-Z0-9]" (lambda (char) (if (> (string-to-char char) #xFFFF) "--" "-"))
                (directory-file-name (file-truename directory))))
         (root (expand-file-name "~/.claude/projects/")))
    (if (<= (length name) 200)
        (expand-file-name name root)
      (let ((prefix (concat (substring name 0 200) "-")))
        (or (and (file-directory-p root)
                 (seq-find (lambda (dir) (string-prefix-p prefix (file-name-nondirectory dir)))
                           (directory-files root t nil t)))
            (expand-file-name name root))))))

(defun claude-native--read-lines (file)
  "FILE's JSON lines as alists (arrays as vectors), skipping bad lines."
  (with-temp-buffer
    (insert-file-contents file)
    (let (records)
      (dolist (line (split-string (buffer-string) "\n" t))
        (when-let* ((record (ignore-errors
                              (json-parse-string line :object-type 'alist :array-type 'array
                                                 :null-object nil :false-object :false))))
          (push record records)))
      (nreverse records))))

(defun claude-native--prompt-p (record)
  "Whether RECORD is a prompt you typed (not a tool result, command echo
or a subagent's)."
  (and (equal (alist-get 'type record) "user")
       (not (eq (alist-get 'isMeta record) t))
       (not (eq (alist-get 'isSidechain record) t))
       ;; "This session is being continued…": written by /compact, not you.
       (not (eq (alist-get 'isCompactSummary record) t))
       (let ((text (claude-native--prompt-text (alist-get 'content (alist-get 'message record)))))
         (and text
              ;; Slash commands and their output come back wrapped in tags.
              (not (string-prefix-p "<" text))))))

(defconst claude-native--chunk 1000000
  "Bytes of a session file read at a time when looking for its first prompt.")

(defun claude-native--first-prompt (file)
  "The first prompt in session FILE, or nil if it has none.
Reads the file a piece at a time, up to that prompt: big request records
may come first, but most of a big file is never read."
  (let ((size (file-attribute-size (file-attributes file)))
        (offset 0))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (catch 'found
        (while (< offset size)
          ;; Add the next piece after what is left of the last one.
          (goto-char (point-max))
          (insert-file-contents-literally file nil offset (min size (+ offset claude-native--chunk)))
          (setq offset (+ offset claude-native--chunk))
          (goto-char (point-min))
          ;; Whole lines only: the last may go on in the next piece.
          (while (search-forward "\n" nil t)
            (let ((line-start (line-beginning-position 0)) (line-end (1- (point))))
              (when (save-excursion (goto-char line-start) (search-forward "\"type\":\"user\"" line-end t))
                (let ((record (ignore-errors
                                (json-parse-string (decode-coding-string (buffer-substring line-start line-end) 'utf-8)
                                                   :object-type 'alist :array-type 'array
                                                   :null-object nil :false-object :false))))
                  (when (and record (claude-native--prompt-p record))
                    (throw 'found (claude-native--prompt-text (alist-get 'content (alist-get 'message record)))))))))
          ;; Keep only the unfinished last line.
          (delete-region (point-min) (save-excursion (goto-char (point-max)) (line-beginning-position)))
          ;; The last line of the file has no newline after it.
          (when (>= offset size)
            (let ((record (ignore-errors
                            (json-parse-string (decode-coding-string (buffer-string) 'utf-8)
                                               :object-type 'alist :array-type 'array
                                               :null-object nil :false-object :false))))
              (when (and record (claude-native--prompt-p record))
                (throw 'found (claude-native--prompt-text (alist-get 'content (alist-get 'message record))))))))
        nil))))

(defconst claude-native--title-tail 400000
  "How much of a session file's end is read for its name.")

(defun claude-native--session-title (file)
  "FILE's latest title: set with /rename, else generated. Nil if none.
Reads the file's end, where the latest title lines are."
  (let ((size (file-attribute-size (file-attributes file))))
    (with-temp-buffer
      (insert-file-contents file nil (max 0 (- size claude-native--title-tail)) size)
      (cl-flet ((last-title (type key)
                  (goto-char (point-max))
                  (when (search-backward (format "\"type\":\"%s\"" type) nil t)
                    (let ((record (ignore-errors
                                    (json-parse-string (buffer-substring (line-beginning-position) (line-end-position))
                                                       :object-type 'alist))))
                      (alist-get key record)))))
        (or (last-title "custom-title" 'customTitle)
            (last-title "ai-title" 'aiTitle))))))

(defun claude-native--sessions (directory)
  "DIRECTORY's past sessions, newest first: plists (:id :time :title :prompt).
:title is nil for a session never named."
  (let ((dir (claude-native--sessions-dir directory)))
    (when (file-directory-p dir)
      (let (sessions)
        (dolist (file (directory-files dir t "\\.jsonl\\'"))
          (when-let* ((prompt (claude-native--first-prompt file)))
            (push (list :id (file-name-base file)
                        :time (file-attribute-modification-time (file-attributes file))
                        ;; Nil when untitled: the first prompt then names it.
                        :title (claude-native--session-title file)
                        :prompt prompt)
                  sessions)))
        (sort sessions (lambda (a b) (time-less-p (plist-get b :time) (plist-get a :time))))))))

(defun claude-native--session-file (directory id)
  "The file of session ID in DIRECTORY."
  (expand-file-name (concat id ".jsonl") (claude-native--sessions-dir directory)))

(defun claude-native--draw-history (file)
  "Draw the conversation stored in session FILE, as it looked live."
  (let ((claude-native--drawing-history t))
    (claude-native--draw-records (claude-native--read-lines file)))
  (claude-native--follow))

(defun claude-native--draw-records (records)
  "Draw session RECORDS (see `claude-native--draw-history')."
  (dolist (record records)
    (unless (eq (alist-get 'isSidechain record) t)
      (let ((message (alist-get 'message record)))
        (pcase (alist-get 'type record)
          ("user"
           (cond ((claude-native--prompt-p record)
                  (let ((text (claude-native--prompt-text (alist-get 'content message))))
                    (claude-native--user text)
                    ;; M-p brings back a resumed session's prompts too.
                    (push text claude-native--history)
                    ;; New attachments are numbered past this session's old ones.
                    (let ((start 0))
                      (while (string-match claude-native--attach-regexp text start)
                        (setq start (match-end 0)
                              claude-native--attach-count (max claude-native--attach-count
                                                               (string-to-number (match-string 2 text))))))))
                 ((vectorp (alist-get 'content message))
                  (seq-doseq (block (alist-get 'content message))
                    (pcase (alist-get 'type block)
                      ("tool_result"
                       (claude-native--tool-result (alist-get 'tool_use_id block) (alist-get 'content block)
                                                   (eq (alist-get 'is_error block) t)))
                      ;; "[Request interrupted by user]", as drawn live.
                      ("text" (claude-native--user-note (alist-get 'text block))))))))
          ("assistant"
           (seq-doseq (block (alist-get 'content message))
             (pcase (alist-get 'type block)
               ("text" (claude-native--text-whole (alist-get 'text block)))
               ("tool_use" (claude-native--tool-use (alist-get 'id block) (alist-get 'name block)
                                                    (alist-get 'input block)))))))))))

(provide 'claude-native-history)
;;; claude-native-history.el ends here
