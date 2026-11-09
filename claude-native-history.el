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
(require 'json)
(require 'seq)
(require 'cl-lib)

(defun claude-native--sessions-dir (directory)
  "Where Claude Code keeps the sessions of DIRECTORY."
  (let ((name (replace-regexp-in-string
               "[^a-zA-Z0-9]" "-" (directory-file-name (file-truename directory)))))
    (expand-file-name name "~/.claude/projects/")))

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
       (let ((text (claude-native--prompt-text (alist-get 'content (alist-get 'message record)))))
         (and text
              ;; Slash commands and their output come back wrapped in tags.
              (not (string-prefix-p "<" text))))))

(defun claude-native--first-prompt (file)
  "The first prompt in session FILE, or nil if it has none.
Reads only the start of big files."
  (with-temp-buffer
    (insert-file-contents file nil 0 200000)
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (let ((record (ignore-errors
                        (json-parse-string (buffer-substring (point) (line-end-position))
                                           :object-type 'alist :array-type 'array
                                           :null-object nil :false-object :false))))
          (when (and record (claude-native--prompt-p record))
            (throw 'found (claude-native--prompt-text (alist-get 'content (alist-get 'message record))))))
        (forward-line 1))
      nil)))

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
                    (push text claude-native--history)))
                 ((vectorp (alist-get 'content message))
                  (seq-doseq (block (alist-get 'content message))
                    (when (equal (alist-get 'type block) "tool_result")
                      (claude-native--tool-result (alist-get 'tool_use_id block) (alist-get 'content block)
                                                  (eq (alist-get 'is_error block) t)))))))
          ("assistant"
           (seq-doseq (block (alist-get 'content message))
             (pcase (alist-get 'type block)
               ("text" (claude-native--text-whole (alist-get 'text block)))
               ("tool_use" (claude-native--tool-use (alist-get 'id block) (alist-get 'name block)
                                                    (alist-get 'input block)))))))))))

(provide 'claude-native-history)
;;; claude-native-history.el ends here
