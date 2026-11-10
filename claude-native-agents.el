;;; claude-native-agents.el --- list the sub-agents at work -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Claude reports each sub-agent as a task: task_started when it begins,
;; task_progress at each step, task_updated and task_notification when it
;; ends. This file keeps the list of those still at work in a session, and
;; shows it in a popup buffer that redraws every second.

;;; Code:

(require 'claude-native-render)
(require 'claude-native-usage)

(defvar-local claude-native--agents nil
  "Sub-agents at work in this session, oldest first: plists
\(:id :tool-id :name :type :started :step :tools :tokens).")

(defun claude-native--agent-event (msg)
  "Keep `claude-native--agents' in step with MSG, a task message."
  (let* ((id (alist-get 'task_id msg))
         (agent (seq-find (lambda (agent) (equal (plist-get agent :id) id)) claude-native--agents))
         (ended (lambda () (setq claude-native--agents (delq agent claude-native--agents)))))
    (pcase (alist-get 'subtype msg)
      ("task_started"
       ;; Tasks are also shells and workflows; an agent's own shells too.
       (when (and (not agent) (string-suffix-p "agent" (or (alist-get 'task_type msg) "")))
         (setq claude-native--agents
               (append claude-native--agents
                       (list (list :id id
                                   :tool-id (alist-get 'tool_use_id msg)
                                   :name (or (alist-get 'description msg) "Agent")
                                   :type (alist-get 'subagent_type msg)
                                   :started (float-time)
                                   :step nil :tools 0 :tokens 0))))))
      ("task_progress"
       (when agent
         (let ((usage (alist-get 'usage msg)))
           (plist-put agent :step (alist-get 'description msg))
           (plist-put agent :tools (or (alist-get 'tool_uses usage) (plist-get agent :tools)))
           (plist-put agent :tokens (or (alist-get 'total_tokens usage) (plist-get agent :tokens))))))
      ("task_updated"
       (let ((status (alist-get 'status (alist-get 'patch msg))))
         (when (and agent status (not (member status '("running" "pending"))))
           (funcall ended))))
      ("task_notification" (when agent (funcall ended))))))

(defun claude-native--agents-text (agents)
  "The list of AGENTS: each on two lines, as a tool call in the transcript."
  (if (null agents)
      (propertize "No sub-agents are running" 'face 'claude-native-dim)
    (mapconcat
     (lambda (agent)
       (let ((tools (plist-get agent :tools))
             (tokens (plist-get agent :tokens)))
         (propertize
          (concat
           (propertize "⏺" 'face 'claude-native-tool-pending) " "
           (propertize (plist-get agent :name) 'face 'claude-native-tool-name)
           (propertize
            (concat "  "
                    (string-join
                     (delq nil (list (plist-get agent :type)
                                     (format "%d tool use%s" tools (if (= tools 1) "" "s"))
                                     (when (> tokens 0)
                                       (concat (claude-native--usage-tokens tokens) " tokens"))
                                     (format-seconds "%hh %mm %z%ss"
                                                     (- (float-time) (plist-get agent :started)))))
                     " · ")
                    "\n" claude-native--indent "⎿  " (or (plist-get agent :step) "Starting…"))
            'face 'claude-native-dim))
          'claude-native-agent agent)))
     agents "\n")))

;;;; The popup

(defvar-local claude-native--agents-session nil
  "In the agents popup: the session buffer whose agents it lists.")
(defvar-local claude-native--agents-timer nil
  "In the agents popup: the timer redrawing it.")

(defvar-keymap claude-native-agents-mode-map
  "RET" #'claude-native-agents-visit)

(define-derived-mode claude-native-agents-mode special-mode "Claude agents"
  "The sub-agents at work in a session. RET goes to an agent's call in
the session; q closes the list."
  (setq-local truncate-lines t
              mode-line-format nil
              left-margin-width 3
              right-margin-width 3
              revert-buffer-function (lambda (&rest _) (claude-native--agents-redraw)))
  (add-hook 'kill-buffer-hook
            (lambda () (when claude-native--agents-timer (cancel-timer claude-native--agents-timer)))
            nil t))

;; With evil: motion state, where q still closes the window.
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")
(with-eval-after-load 'evil
  (evil-set-initial-state 'claude-native-agents-mode 'motion)
  (evil-define-key* 'motion claude-native-agents-mode-map (kbd "RET") #'claude-native-agents-visit))

(defun claude-native--agents-redraw ()
  "Draw the popup again, the cursor staying on its line."
  (let* ((session claude-native--agents-session)
         (text (claude-native--agents-text
                (and (buffer-live-p session) (buffer-local-value 'claude-native--agents session))))
         (inhibit-read-only t))
    ;; Each window showing the list has its own cursor.
    (let ((places (mapcar (lambda (window)
                            (cons window (with-selected-window window
                                           (cons (line-number-at-pos) (current-column)))))
                          (get-buffer-window-list nil nil t)))
          (line (line-number-at-pos))
          (column (current-column)))
      (erase-buffer)
      (insert "\n" text "\n\n")
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column)
      (pcase-dolist (`(,window . (,line . ,column)) places)
        (with-selected-window window
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column column))
        (fit-window-to-buffer window)))))

(defun claude-native--agents-tick (buffer)
  "Redraw popup BUFFER while it shows; stop its timer once it does not."
  (if (and (buffer-live-p buffer) (get-buffer-window buffer t))
      (with-current-buffer buffer (claude-native--agents-redraw))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when claude-native--agents-timer (cancel-timer claude-native--agents-timer))
        (setq claude-native--agents-timer nil)))))

(defun claude-native--show-agents (session)
  "Show the sub-agents at work in SESSION, a session buffer, in a window below."
  (let ((buffer (get-buffer-create "*claude agents*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'claude-native-agents-mode)
        (claude-native-agents-mode))
      (setq claude-native--agents-session session)
      (claude-native--agents-redraw)
      (goto-char (point-min))
      (forward-line 1))
    (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-at-bottom)
                            (window-height . fit-window-to-buffer)))
    ;; Started once the popup shows: the timer ends itself when it does not.
    (with-current-buffer buffer
      (unless claude-native--agents-timer
        (setq claude-native--agents-timer
              (run-at-time 1 1 #'claude-native--agents-tick buffer))))))

(defun claude-native-agents ()
  "List the sub-agents at work in this session, in a window below.
Each shows its latest step, tool uses, tokens and time so far, updated
every second. RET goes to the agent's call in the session; q closes."
  (interactive)
  (unless (derived-mode-p 'claude-native-mode)
    (user-error "Not in a Claude session"))
  (claude-native--show-agents (current-buffer)))

(defun claude-native-agents-visit ()
  "Go to the call that started the agent at point, in its session."
  (interactive)
  (let ((agent (or (get-text-property (point) 'claude-native-agent)
                   (user-error "No agent here")))
        (session claude-native--agents-session))
    (unless (buffer-live-p session)
      (user-error "The session is gone"))
    (let ((bullet (with-current-buffer session
                    (plist-get (gethash (plist-get agent :tool-id) (claude-native--tool-table))
                               :bullet))))
      (pop-to-buffer session)
      (if (and bullet (marker-position bullet))
          (goto-char bullet)
        (message "That agent's call is no longer in the transcript")))))

(provide 'claude-native-agents)
;;; claude-native-agents.el ends here
