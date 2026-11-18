;;; agent-native-agents.el --- list the sub-agents at work -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Keeps the list of sub-agents at work in a session, from the agent-*
;; events backends send (see agent-native-backend.el), and shows it in a
;; popup buffer that redraws every second.

;;; Code:

(require 'agent-native-render)
(require 'agent-native-backend)

(defvar-local agent-native--agents nil
  "Sub-agents at work in this session, oldest first: plists
\(:id :tool-id :name :type :model :effort :started :step :tools :tokens).")

(defvar agent-native--model)
(defvar agent-native--effort)
(defvar agent-native--backend)

(defun agent-native--short-model (model)
  "MODEL without its \"claude-\" prefix."
  (and (stringp model) (string-remove-prefix "claude-" model)))

(defun agent-native--find-agent (key value)
  "The agent at work whose KEY (:id or :tool-id) is VALUE."
  (seq-find (lambda (agent) (equal (plist-get agent key) value)) agent-native--agents))

(defun agent-native--agent-start (props)
  "An agent started: PROPS of an `agent-start' event. Without a model or
effort of its own, it runs as the session does."
  (unless (agent-native--find-agent :id (plist-get props :id))
    (setq agent-native--agents
          (append agent-native--agents
                  (list (list :id (plist-get props :id)
                              :tool-id (plist-get props :tool-id)
                              :name (or (plist-get props :name) "Agent")
                              :type (plist-get props :type)
                              :model (agent-native--short-model (or (plist-get props :model) agent-native--model))
                              :effort (or (plist-get props :effort) agent-native--effort)
                              :started (float-time)
                              :step nil :tools 0 :tokens 0))))))

(defun agent-native--agent-progress (props)
  "An agent moved on: PROPS of an `agent-progress' event."
  (when-let* ((agent (agent-native--find-agent :id (plist-get props :id))))
    (dolist (key '(:step :tools :tokens))
      (when (plist-get props key)
        (plist-put agent key (plist-get props key))))))

(defun agent-native--agent-model (tool-id model)
  "The agent started by call TOOL-ID answers with MODEL (from its messages)."
  (when-let* ((agent (agent-native--find-agent :tool-id tool-id)))
    (plist-put agent :model (agent-native--short-model model))))

(defun agent-native--agent-end (id)
  "Agent ID is done."
  (setq agent-native--agents (seq-remove (lambda (agent) (equal (plist-get agent :id) id))
                                         agent-native--agents)))

(defun agent-native--agents-text (agents)
  "The list of AGENTS, as tool calls in the transcript: name and type, then
what it does now, then model, effort, tool uses, tokens and time so far."
  (if (null agents)
      (propertize "No sub-agents are running" 'face 'agent-native-dim)
    (mapconcat
     (lambda (agent)
       (let ((tools (plist-get agent :tools))
             (tokens (plist-get agent :tokens))
             (pad (concat agent-native--indent "   ")))
         (propertize
          (concat
           (propertize "⏺" 'face 'agent-native-tool-pending) " "
           (propertize (plist-get agent :name) 'face 'agent-native-tool-name)
           (when-let* ((type (plist-get agent :type)))
             (propertize (format "  %s" type) 'face 'agent-native-dim))
           "\n" agent-native--indent "⎿  " (or (plist-get agent :step) "Starting…")
           "\n" pad
           (propertize
            (string-join
             (delq nil (list (plist-get agent :model)
                             (when-let* ((effort (plist-get agent :effort))) (concat "◐ " effort))
                             (format "%d tool use%s" tools (if (= tools 1) "" "s"))
                             (format "%s tokens" (agent-native--short-count tokens))
                             (format-seconds "%hh %mm %z%ss" (- (float-time) (plist-get agent :started)))))
             " · ")
            'face 'agent-native-dim))
          'agent-native-agent agent)))
     agents "\n\n")))

;;;; The popup

(defvar-local agent-native--agents-session nil
  "In the agents popup: the session buffer whose agents it lists.")
(defvar-local agent-native--agents-timer nil
  "In the agents popup: the timer redrawing it.")

(defvar-keymap agent-native-agents-mode-map
  "RET" #'agent-native-agents-visit)

(define-derived-mode agent-native-agents-mode special-mode "Agents"
  "The sub-agents at work in a session. RET goes to an agent's call in
the session; q closes the list."
  (setq-local truncate-lines t
              mode-line-format nil
              left-margin-width 3
              right-margin-width 3
              revert-buffer-function (lambda (&rest _) (agent-native--agents-redraw)))
  (add-hook 'kill-buffer-hook
            (lambda () (when agent-native--agents-timer (cancel-timer agent-native--agents-timer)))
            nil t))

;; With evil: motion state, where q still closes the window.
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")
(with-eval-after-load 'evil
  (evil-set-initial-state 'agent-native-agents-mode 'motion)
  (evil-define-key* 'motion agent-native-agents-mode-map (kbd "RET") #'agent-native-agents-visit))

(defun agent-native--agents-redraw ()
  "Draw the popup again, the cursor staying on its line."
  (let* ((session agent-native--agents-session)
         (text (agent-native--agents-text
                (and (buffer-live-p session) (buffer-local-value 'agent-native--agents session))))
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
        ;; Fitted when the list grows or shrinks; a size set by hand stays.
        (unless (eql (window-parameter window 'agent-native-lines) (count-lines (point-min) (point-max)))
          (set-window-parameter window 'agent-native-lines (count-lines (point-min) (point-max)))
          (fit-window-to-buffer window))))))

(defun agent-native--agents-tick (buffer)
  "Redraw popup BUFFER when it shows. The timer ends with the buffer."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer (agent-native--agents-redraw))))

(defun agent-native--show-agents (session)
  "Show the sub-agents at work in SESSION, a session buffer, in a window below."
  (let ((buffer (get-buffer-create (format "*%s agents*" (downcase (agent-native-backend-name
                                                                      (buffer-local-value 'agent-native--backend session)))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-native-agents-mode)
        (agent-native-agents-mode))
      (setq agent-native--agents-session session)
      (agent-native--agents-redraw)
      (goto-char (point-min))
      (forward-line 1))
    (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-at-bottom)
                            (window-height . fit-window-to-buffer)))
    (with-current-buffer buffer
      (unless agent-native--agents-timer
        (setq agent-native--agents-timer
              (run-at-time 1 1 #'agent-native--agents-tick buffer))))))

(defun agent-native-agents ()
  "List the sub-agents at work in this session, in a window below.
Each shows its latest step, tool uses, tokens and time so far, updated
every second. RET goes to the agent's call in the session; q closes."
  (interactive)
  (unless (derived-mode-p 'agent-native-mode)
    (user-error "Not in an agent session"))
  (agent-native--show-agents (current-buffer)))

(defun agent-native-agents-visit ()
  "Go to the call that started the agent at point, in its session."
  (interactive)
  (let ((agent (or (get-text-property (point) 'agent-native-agent)
                   (user-error "No agent here")))
        (session agent-native--agents-session))
    (unless (buffer-live-p session)
      (user-error "The session is gone"))
    (let ((bullet (with-current-buffer session
                    (plist-get (gethash (plist-get agent :tool-id) (agent-native--tool-table))
                               :bullet))))
      (pop-to-buffer session)
      (if (and bullet (marker-position bullet))
          (goto-char bullet)
        (message "That agent's call is no longer in the transcript")))))

(provide 'agent-native-agents)
;;; agent-native-agents.el ends here
