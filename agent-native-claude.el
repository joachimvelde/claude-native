;;; agent-native-claude.el --- the Claude Code backend -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Claude Code runs headless (`claude -p' with stream-json in and out): one
;; JSON object per line each way. This file turns its messages into the
;; events of agent-native-backend.el, and the operations there into user
;; messages and control requests.
;;
;; Past sessions are JSON lines under ~/.claude/projects/<the directory,
;; every non-alphanumeric char a dash>/<session id>.jsonl. A resumed
;; session is drawn from that file, as events, before claude starts.

;;; Code:

(require 'agent-native-backend)
(require 'iso8601)
(require 'seq)

(defcustom agent-native-claude-program
  (or (executable-find "claude")
      ;; The installer's place; an Emacs started from the Dock may not
      ;; have it on PATH.
      (let ((local (expand-file-name "~/.local/bin/claude")))
        (and (file-executable-p local) local))
      "claude")
  "The claude executable."
  :type 'string
  :group 'agent-native)

(defcustom agent-native-claude-extra-args nil
  "More arguments for claude, e.g. (\"--fallback-model\" \"sonnet\")."
  :type '(repeat string)
  :group 'agent-native)

(defcustom agent-native-claude-model nil
  "Model to start Claude sessions with (e.g. \"opus\"), or nil for claude's default."
  :type '(choice (const nil) string)
  :group 'agent-native)

(defcustom agent-native-remote-control 'auto
  "Whether a Claude session starts under Remote Control.
Under it the session is listed on claude.ai and in the Claude app, to
follow and steer from there. `auto' does what Claude Code is set to do
\(\"Enable Remote Control for all sessions\" in its /config, which is
remoteControlAtStartup in settings.json); t is always on, nil never.
\\[agent-native-toggle-remote-control] switches it for one session."
  :type '(choice (const :tag "As Claude Code is set up" auto)
                 (const :tag "On" t)
                 (const :tag "Off" nil))
  :group 'agent-native)

;; Set in agent-native-render.el.
(defvar agent-native-format)
(defvar agent-native-org-prompt)

(cl-defstruct (agent-native-claude (:include agent-native-conn)
                                   (:constructor agent-native-claude--make))
  "A running claude."
  ;; Ids of messages whose text arrived as stream events, so the whole
  ;; message that follows is not drawn twice.
  (streamed nil)
  ;; Agent tool_use id -> its input, for the model and effort it asked for.
  (agent-calls nil)
  ;; Remote Control's state as last reported.
  (remote nil))

(defconst agent-native-claude--modes
  '((default . "default") (accept-edits . "acceptEdits") (plan . "plan")
    (bypass . "bypassPermissions"))
  "Permission modes, and what claude calls them.")

(cl-defmethod agent-native-backend-name ((_ (eql 'claude))) "Claude")
(cl-defmethod agent-native-backend-title ((_ (eql 'claude))) "Claude Code")
(cl-defmethod agent-native-backend-provider ((_ (eql 'claude))) "Claude")

(cl-defmethod agent-native-backend-available-p ((_ (eql 'claude)))
  (and (executable-find agent-native-claude-program) t))

(cl-defmethod agent-native-backend-modes ((_ (eql 'claude)))
  '((default . "") (accept-edits . "⏵⏵ accept edits on") (plan . "⏸ plan mode on")
    (bypass . "⏵⏵ bypass permissions on")))

(defun agent-native-claude--mode (name)
  "The mode claude calls NAME."
  (car (rassoc name agent-native-claude--modes)))

;;;; Starting

(defun agent-native-claude--format-args ()
  "The arguments that tell claude which format to write."
  (when (eq agent-native-format 'org)
    (list "--append-system-prompt" agent-native-org-prompt)))

(cl-defmethod agent-native-backend-start ((_ (eql 'claude)) directory &key resume on-event on-exit mode)
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (conn (agent-native-claude--make :backend 'claude :on-event on-event :on-exit on-exit)))
    (when resume
      (agent-native--emit conn 'history-start)
      (agent-native-claude--replay conn (agent-native--read-lines (agent-native-claude--session-file directory resume)))
      (agent-native--emit conn 'history-end))
    (agent-native--spawn
     conn "agent-native-claude"
     `(,agent-native-claude-program
       "-p" "--input-format" "stream-json" "--output-format" "stream-json"
       "--verbose" "--include-partial-messages"
       ;; Echo each prompt when claude takes it up, so a prompt sent while
       ;; it works is drawn when its turn starts.
       "--replay-user-messages"
       ;; A predicted next prompt after each turn, shown dim in the input.
       "--prompt-suggestions" "true"
       "--permission-prompt-tool" "stdio"
       ;; Lets S-TAB reach bypass from any start mode.
       "--allow-dangerously-skip-permissions"
       "--permission-mode" ,(alist-get (or mode 'default) agent-native-claude--modes)
       ,@(when agent-native-claude-model (list "--model" agent-native-claude-model))
       ,@(agent-native-claude--format-args)
       ,@(when resume (list "--resume" resume))
       ,@agent-native-claude-extra-args))
    (agent-native-claude--control conn "initialize" nil
                                  (lambda (response)
                                    (agent-native-claude--initialized conn (alist-get 'response response))))
    conn))

(defun agent-native-claude--control (conn subtype &optional fields callback)
  "Send control request SUBTYPE with FIELDS (an alist); CALLBACK gets the response."
  (let ((id (format "emacs_%d" (agent-native--next-id conn))))
    (agent-native--expect conn id callback)
    (agent-native--write conn `((type . "control_request")
                                (request_id . ,id)
                                (request . ((subtype . ,subtype) ,@fields))))))

(defconst agent-native-claude--extra-commands
  '(("cost" . "Show usage and what this session cost")
    ("remote-control" . "Follow and steer this session from claude.ai or the Claude app"))
  "Commands that work but the handshake does not list.")

(defun agent-native-claude--commands (commands)
  "The `session' event properties for COMMANDS, as claude lists them."
  (list :commands (append (mapcar (lambda (c) (cons (alist-get 'name c) (alist-get 'description c)))
                                  commands)
                          agent-native-claude--extra-commands)
        :hints (delq nil (mapcar (lambda (c)
                                   (let ((hint (alist-get 'argumentHint c)))
                                     (and (stringp hint) (not (string-empty-p hint))
                                          (cons (alist-get 'name c) hint))))
                                 commands))))

(defun agent-native-claude--model-plist (model)
  "MODEL, as claude's handshake lists it, in the form the picker reads."
  (list :value (alist-get 'value model)
        :aliases (delq nil (list (alist-get 'value model) (alist-get 'resolvedModel model)))
        :name (alist-get 'displayName model)
        :description (alist-get 'description model)
        :efforts (and (eq (alist-get 'supportsEffort model) t)
                      (append (alist-get 'supportedEffortLevels model) nil))))

(defun agent-native-claude--initialized (conn info)
  "Pass on what the handshake INFO says: commands, models, mode; start
Remote Control if wanted."
  (apply #'agent-native--emit conn 'session
         :models (mapcar #'agent-native-claude--model-plist (alist-get 'models info))
         (append (when-let* ((mode (alist-get 'current_permission_mode info)))
                   (list :mode (agent-native-claude--mode mode)))
                 (agent-native-claude--commands (alist-get 'commands info))))
  (when (agent-native-claude--remote-wanted-p info)
    (agent-native-backend-remote-control conn t))
  (agent-native-claude--refresh-settings conn))

(defun agent-native-claude--refresh-settings (conn)
  "Ask claude which model and effort it uses now, and pass them on."
  (agent-native-claude--control
   conn "get_settings" nil
   (lambda (response)
     (let ((applied (alist-get 'applied (alist-get 'response response))))
       (apply #'agent-native--emit conn 'session
              (append (when (alist-get 'model applied) (list :model (alist-get 'model applied)))
                      ;; A model without effort levels reports none: show none.
                      (when (assq 'effort applied) (list :effort (alist-get 'effort applied)))))))))

;;;; Messages from claude

(cl-defmethod agent-native-backend-receive ((conn agent-native-claude) msg)
  (if (alist-get 'parent_tool_use_id msg)
      (agent-native-claude--subagent conn msg)
    (pcase (alist-get 'type msg)
      ("control_response"
       (let ((response (alist-get 'response msg)))
         (agent-native--answered conn (alist-get 'request_id response) response)))
      ("stream_event" (agent-native-claude--stream conn (alist-get 'event msg)))
      ("assistant" (agent-native-claude--assistant conn (alist-get 'message msg)))
      ("user" (agent-native-claude--user conn (alist-get 'message msg) (eq (alist-get 'isReplay msg) t)))
      ("control_cancel_request" (agent-native--emit conn 'request-done :id (alist-get 'request_id msg)))
      ("system" (agent-native-claude--system conn msg))
      ("result" (agent-native-claude--result conn msg))
      ("prompt_suggestion" (agent-native--emit conn 'suggestion :text (alist-get 'suggestion msg)))
      ("control_request" (agent-native-claude--request conn msg)))))

(defun agent-native-claude--subagent (conn msg)
  "A sub-agent's message MSG: its model, and its tool calls, as steps."
  (when (equal (alist-get 'type msg) "assistant")
    (let ((parent (alist-get 'parent_tool_use_id msg)))
      ;; The model the agent really runs on.
      (when-let* ((model (alist-get 'model (alist-get 'message msg))))
        (agent-native--emit conn 'agent-model :tool-id parent :model model))
      (seq-doseq (block (alist-get 'content (alist-get 'message msg)))
        (when (equal (alist-get 'type block) "tool_use")
          (agent-native--emit conn 'agent-step :tool-id parent
                              :name (alist-get 'name block) :input (alist-get 'input block)))))))

(defun agent-native-claude--stream (conn event)
  "Pass on stream EVENT: text as it is written, and what claude is at."
  (pcase (alist-get 'type event)
    ("message_start"
     ;; Only the latest messages can still arrive whole; keep a few ids.
     (setf (agent-native-claude-streamed conn)
           (seq-take (cons (alist-get 'id (alist-get 'message event)) (agent-native-claude-streamed conn)) 20)))
    ("content_block_start"
     (let ((block (alist-get 'content_block event)))
       (pcase (alist-get 'type block)
         ("text" (agent-native--emit conn 'text-start))
         ("thinking" (agent-native--emit conn 'working :status "Thinking…"))
         ("tool_use" (agent-native--emit conn 'working :status (format "%s…" (alist-get 'name block)))))))
    ("content_block_delta"
     (let ((delta (alist-get 'delta event)))
       (when (equal (alist-get 'type delta) "text_delta")
         (agent-native--emit conn 'text-delta :text (alist-get 'text delta)))))
    ("content_block_stop" (agent-native--emit conn 'text-end))))

(defun agent-native-claude--tool-use (conn block)
  "Pass on tool_use BLOCK; keep an Agent call's input for its task."
  (when (member (alist-get 'name block) '("Agent" "Task"))
    (push (cons (alist-get 'id block) (alist-get 'input block)) (agent-native-claude-agent-calls conn)))
  (agent-native--emit conn 'tool :id (alist-get 'id block) :name (alist-get 'name block)
                      :input (alist-get 'input block)))

(defun agent-native-claude--assistant (conn message)
  "A whole assistant MESSAGE: tool calls, and text not streamed."
  (let ((streamed (member (alist-get 'id message) (agent-native-claude-streamed conn))))
    (seq-doseq (block (alist-get 'content message))
      (pcase (alist-get 'type block)
        ("tool_use" (agent-native-claude--tool-use conn block))
        ("text" (unless streamed (agent-native--emit conn 'text :text (alist-get 'text block))))))))

(defun agent-native-claude--prompt-text (content)
  "The text of prompt CONTENT: a string, or a vector of blocks (a prompt
with files attached). Nil if CONTENT is not a prompt."
  (cond ((stringp content) content)
        ;; Blocks are a prompt only with a file in them: interruptions
        ;; and tool results come as blocks too.
        ((and (vectorp content)
              (seq-some (lambda (b) (member (alist-get 'type b) '("image" "document"))) content))
         (let ((texts (delq nil (mapcar (lambda (b) (and (equal (alist-get 'type b) "text")
                                                          (alist-get 'text b)))
                                        content))))
           (and texts (string-join texts "\n"))))))

(defun agent-native-claude--user-text (conn text)
  "TEXT claude sent as the user's: an interruption is one."
  (when (string-prefix-p "[Request interrupted" text)
    (agent-native--emit conn 'interrupted)))

(defun agent-native-claude--user (conn message &optional replay)
  "A user MESSAGE claude echoes: your prompt as it takes it up (REPLAY),
tool results, interruptions."
  (let ((content (alist-get 'content message)))
    (cond
     ((and replay (agent-native-claude--prompt-text content))
      (agent-native--emit conn 'prompt :text (agent-native-claude--prompt-text content)))
     ((stringp content) (agent-native-claude--user-text conn content))
     (t
      (seq-doseq (block content)
        (pcase (alist-get 'type block)
          ("tool_result"
           (let ((content (alist-get 'content block)))
             (agent-native--emit conn 'tool-result :id (alist-get 'tool_use_id block)
                                 ;; Your no, in a resumed session: said as it was then.
                                 :content (if (and (stringp content)
                                                   (string-prefix-p "The user doesn't want to proceed" content))
                                              "Denied"
                                            content)
                                 :error (eq (alist-get 'is_error block) t))))
          ("text" (agent-native-claude--user-text conn (alist-get 'text block)))))))))

(defun agent-native-claude--system (conn msg)
  "Pass on what claude reports about itself."
  (pcase (alist-get 'subtype msg)
    ("init"
     (agent-native--emit conn 'session :id (alist-get 'session_id msg) :model (alist-get 'model msg)
                         :mode (agent-native-claude--mode (alist-get 'permissionMode msg))))
    ("status"
     (when (equal (alist-get 'status msg) "compacting")
       (agent-native--emit conn 'working :status "Compacting…"))
     (when-let* ((mode (agent-native-claude--mode (alist-get 'permissionMode msg))))
       (agent-native--emit conn 'session :mode mode)))
    ("compact_boundary" (agent-native--emit conn 'note :text "✻ Conversation compacted"))
    ("thinking_tokens"
     (agent-native--emit conn 'thinking-tokens :count (or (alist-get 'estimated_tokens msg) 0)))
    ("commands_changed"
     (apply #'agent-native--emit conn 'session (agent-native-claude--commands (alist-get 'commands msg))))
    ("bridge_state" (agent-native-claude--bridge-state conn (alist-get 'state msg) (alist-get 'detail msg)))
    ((or "task_started" "task_progress" "task_updated" "task_notification")
     (agent-native-claude--task conn msg))))

(defun agent-native-claude--result (conn msg)
  "A turn ended. Model and effort are asked for again: a /model or
/effort may have changed them."
  (agent-native-claude--refresh-settings conn)
  (let ((error (eq (alist-get 'is_error msg) t)))
    (agent-native--emit conn 'turn-end
                        :duration-ms (and (not error) (alist-get 'duration_ms msg))
                        :error (and error (or (alist-get 'result msg) (alist-get 'subtype msg)))
                        :text (alist-get 'result msg)
                        :cost (alist-get 'total_cost_usd msg))))

;;;; Sub-agents, which claude reports as tasks

(defun agent-native-claude--task (conn msg)
  "Pass on task message MSG: a sub-agent started, stepped, or ended.
Tasks are also shells and workflows; only agents are passed on."
  (let ((id (alist-get 'task_id msg)))
    (pcase (alist-get 'subtype msg)
      ("task_started"
       (when (string-suffix-p "agent" (or (alist-get 'task_type msg) ""))
         ;; Model and effort as the call asked for them; else the agent
         ;; runs as the session does. Its messages say the model for sure.
         (let ((input (alist-get (alist-get 'tool_use_id msg) (agent-native-claude-agent-calls conn)
                                 nil nil #'equal)))
           (agent-native--emit conn 'agent-start :id id :tool-id (alist-get 'tool_use_id msg)
                               :name (or (alist-get 'description msg) "Agent")
                               :type (alist-get 'subagent_type msg)
                               :model (alist-get 'model input) :effort (alist-get 'effort input)))))
      ("task_progress"
       (let ((usage (alist-get 'usage msg)))
         (agent-native--emit conn 'agent-progress :id id :step (alist-get 'description msg)
                             :tools (alist-get 'tool_uses usage) :tokens (alist-get 'total_tokens usage))))
      ("task_updated"
       (let ((status (alist-get 'status (alist-get 'patch msg))))
         (when (and status (not (member status '("running" "pending"))))
           (agent-native--emit conn 'agent-end :id id))))
      ("task_notification" (agent-native--emit conn 'agent-end :id id)))))

;;;; Permission requests

(defun agent-native-claude--request (conn msg)
  "Pass on a control request from claude: permission to use a tool, or
AskUserQuestion's questions."
  (let ((request (alist-get 'request msg)))
    (when (equal (alist-get 'subtype request) "can_use_tool")
      (let ((input (alist-get 'input request)))
        (agent-native--emit
         conn 'request
         :id (alist-get 'request_id msg) :tool-id (alist-get 'tool_use_id request)
         :name (alist-get 'tool_name request) :input input
         :suggestions (alist-get 'permission_suggestions request)
         :always (agent-native-claude--always-text (alist-get 'permission_suggestions request))
         :questions (when (equal (alist-get 'tool_name request) "AskUserQuestion")
                      (mapcar (lambda (q)
                                (list :id (alist-get 'question q) :question (alist-get 'question q)
                                      :options (mapcar (lambda (o) (alist-get 'label o)) (alist-get 'options q))))
                              (alist-get 'questions input))))))))

(defun agent-native-claude--always-text (suggestions)
  "What answering \"always\" keeps: claude's permission SUGGESTIONS, which
it applies then, in words. Rules may be kept beyond the session."
  (let ((parts
         (delq nil
               (mapcar
                (lambda (suggestion)
                  (let ((where (pcase (alist-get 'destination suggestion)
                                 ("session" " in this session")
                                 ("localSettings" " in this project, from now on")
                                 ("projectSettings" " in this project, for everyone")
                                 ("userSettings" " in every project, from now on")
                                 (_ ""))))
                    (pcase (alist-get 'type suggestion)
                      ("addRules"
                       (format "allow %s%s"
                               (mapconcat (lambda (rule)
                                            (if-let* ((content (alist-get 'ruleContent rule)))
                                                (format "%s(%s)" (alist-get 'toolName rule) content)
                                              (alist-get 'toolName rule)))
                                          (alist-get 'rules suggestion) ", ")
                               where))
                      ("setMode"
                       (format "switch to %s mode%s"
                               (pcase (alist-get 'mode suggestion)
                                 ("acceptEdits" "accept-edits")
                                 ("bypassPermissions" "bypass-permissions")
                                 (mode mode))
                               where))
                      ("addDirectories"
                       (format "allow %s%s" (mapconcat #'identity (alist-get 'directories suggestion) ", ")
                               where)))))
                suggestions))))
    (if parts
        (concat "Yes, and " (string-join parts "; "))
      "Yes, this time (Claude offers nothing to keep)")))

(cl-defmethod agent-native-backend-answer ((conn agent-native-claude) request answer)
  (let ((input (plist-get request :input)))
    (agent-native--write
     conn `((type . "control_response")
            (response . ((subtype . "success")
                         (request_id . ,(plist-get request :id))
                         (response
                          . ,(pcase answer
                               ('allow `((behavior . "allow") (updatedInput . ,input)))
                               ('always `((behavior . "allow") (updatedInput . ,input)
                                          (updatedPermissions . ,(or (plist-get request :suggestions) []))))
                               ('deny `((behavior . "deny") (message . "The user said no.") (interrupt . t)))
                               ;; AskUserQuestion: the input with the answers added.
                               (`(answers . ,answers)
                                `((behavior . "allow")
                                  (updatedInput . ,(append input `((answers . ,(mapcar (lambda (a) (cons (intern (car a)) (cdr a)))
                                                                                       answers)))))))))))))))

;;;; Operations

(defun agent-native-claude--content (text attachments)
  "What to send for prompt TEXT with ATTACHMENTS: TEXT itself, or a vector
of the files' blocks and then the text."
  (if (null attachments)
      text
    (vconcat
     (mapcar (pcase-lambda (`(,kind ,file ,media))
               `((type . ,(if (eq kind 'image) "image" "document"))
                 (source . ((type . "base64") (media_type . ,media)
                            (data . ,(with-temp-buffer
                                       (set-buffer-multibyte nil)
                                       (insert-file-contents-literally file)
                                       (base64-encode-region (point-min) (point-max) t)
                                       (buffer-string)))))))
             attachments)
     (list `((type . "text") (text . ,text))))))

(defun agent-native-claude--send-user (conn content)
  "Send a user message with CONTENT: a string, or a vector of blocks."
  (agent-native--write conn `((type . "user") (message . ((role . "user") (content . ,content))))))

(cl-defmethod agent-native-backend-send ((conn agent-native-claude) text attachments)
  (agent-native-claude--send-user conn (agent-native-claude--content text attachments)))

(cl-defmethod agent-native-backend-interrupt ((conn agent-native-claude))
  (agent-native-claude--control conn "interrupt"))

(cl-defmethod agent-native-backend-set-mode ((conn agent-native-claude) mode)
  (agent-native-claude--control conn "set_permission_mode"
                                `((mode . ,(alist-get mode agent-native-claude--modes)))))

(defun agent-native-claude--then-refresh (conn)
  "A callback for a settings change: say a refusal, then show what claude
uses now (the label went ahead of it)."
  (lambda (response)
    (when (equal (alist-get 'subtype response) "error")
      (agent-native--emit conn 'note :text (format "Error: %s" (alist-get 'error response))
                          :face 'agent-native-tool-error))
    (agent-native-claude--refresh-settings conn)))

(cl-defmethod agent-native-backend-set-model ((conn agent-native-claude) model effort)
  ;; Both at once, before any prompt sent after: claude applies them in
  ;; order, and the effort stays with the new model.
  (agent-native-claude--control conn "set_model" `((model . ,model)) (agent-native-claude--then-refresh conn))
  (when effort (agent-native-backend-set-effort conn effort)))

(cl-defmethod agent-native-backend-set-effort ((conn agent-native-claude) effort)
  ;; The session's level, set at once (not /effort, which waits its turn
  ;; behind a prompt sent before it).
  (agent-native-claude--control conn "apply_flag_settings" `((settings . ((effortLevel . ,effort))))
                                (agent-native-claude--then-refresh conn)))

;;;; Remote Control

(defun agent-native-claude--remote-wanted-p (info)
  "Whether the session should start under Remote Control.
Headless claude leaves that to its host; the handshake INFO says what
claude's settings ask for, and whether it can work here at all."
  (and (not (eq (alist-get 'remote_control_available info) :false))
       (if (eq agent-native-remote-control 'auto)
           (eq (alist-get 'remote_control_auto_enable info) t)
         agent-native-remote-control)))

(defun agent-native-claude--remote-is (conn state &rest props)
  "Pass on Remote Control's STATE (nil: off), with PROPS (:url :message)."
  (setf (agent-native-claude-remote conn) state)
  (apply #'agent-native--emit conn 'remote :state state props))

(cl-defmethod agent-native-backend-remote-control ((conn agent-native-claude) on)
  (agent-native-claude--control
   conn "remote_control" `((enabled . ,(if on t :false)))
   (lambda (response)
     (cond
      ((equal (alist-get 'subtype response) "error")
       (agent-native-claude--remote-is conn nil :message (format "Remote Control: %s" (alist-get 'error response))))
      ;; On: "connected" follows by itself, as a bridge_state message.
      (on (agent-native-claude--remote-is conn (or (agent-native-claude-remote conn) "ready")
                                          :url (alist-get 'session_url (alist-get 'response response))))
      (t (agent-native-claude--remote-is conn nil))))))

(defun agent-native-claude--bridge-state (conn state detail)
  "Remote Control's new STATE; DETAIL says why it ended."
  (if (member state '("failed" "policy_disabled" "ended"))
      (agent-native-claude--remote-is
       conn nil :message (format "Remote Control %s%s" (string-replace "_" " " state)
                                 (if (stringp detail) (concat ": " detail) "")))
    (agent-native-claude--remote-is conn state)))

;;;; Usage

(defun agent-native-claude--usage-resets (iso)
  "\"resets Nov 13, 18:00\" for ISO, a timestamp; nil if it is not one."
  (when (stringp iso)
    (condition-case nil
        ;; To the nearest minute: 17:59:59.9 is 18:00.
        (format-time-string "resets %b %-e, %H:%M" (time-add (encode-time (iso8601-parse iso)) 30))
      (error nil))))

(defun agent-native-claude--usage-limits (rate-limits)
  "The plan limits in RATE-LIMITS, as rows (TITLE PERCENT DETAIL)."
  ;; A limit without a figure (null) is not drawn: there is nothing to show.
  (or (delq nil
            (mapcar (lambda (limit)
                      (let* ((scope (alist-get 'scope limit))
                             (within (or (alist-get 'display_name (alist-get 'model scope))
                                         (alist-get 'surface scope))))
                        (when (numberp (alist-get 'percent limit))
                          (list (pcase (alist-get 'kind limit)
                                  ("session" "Current session")
                                  ("weekly_all" "Current week (all models)")
                                  (kind (if within (format "Current week (%s)" within)
                                          (capitalize (string-replace "_" " " (format "%s" kind))))))
                                (alist-get 'percent limit)
                                (agent-native-claude--usage-resets (alist-get 'resets_at limit))))))
                    (alist-get 'limits rate-limits)))
      ;; A claude that lists no limits still names the two windows.
      (delq nil (mapcar (lambda (window)
                          (when-let* ((limit (alist-get (car window) rate-limits))
                                      ((numberp (alist-get 'utilization limit))))
                            (list (cdr window) (alist-get 'utilization limit)
                                  (agent-native-claude--usage-resets (alist-get 'resets_at limit)))))
                        '((five_hour . "Current session")
                          (seven_day . "Current week (all models)"))))))

(defun agent-native-claude--usage-session (session)
  "What SESSION (cost, time, lines changed) comes to, on one line."
  (let ((seconds (/ (or (alist-get 'total_api_duration_ms session) 0) 1000))
        (added (or (alist-get 'total_lines_added session) 0))
        (removed (or (alist-get 'total_lines_removed session) 0)))
    (string-join
     (delq nil (list (format "$%.2f" (or (alist-get 'total_cost_usd session) 0))
                     (when (> seconds 0)
                       (concat (format-seconds "%hh %mm %z%ss" seconds) " API time"))
                     (when (> (+ added removed) 0)
                       (format "+%d −%d lines" added removed))))
     " · ")))

(defun agent-native-claude--usage-behaviors (title period)
  "A section on what used the plan most in PERIOD, headed by TITLE."
  (when period
    (cons
     (format "%s · %d requests · %d sessions" title
             (or (alist-get 'request_count period) 0) (or (alist-get 'session_count period) 0))
     (append
      (mapcar (lambda (advice)
                (format "  %3d%%  %s%s" (alist-get 'pct advice) (alist-get 'title advice)
                        (if-let* ((detail (alist-get 'detail advice))) (concat " · " detail) "")))
              (alist-get 'recommendations period))
      (delq nil
            (mapcar (lambda (top)
                      (when-let* ((items (append (alist-get (car top) period) nil)))
                        (format "  Top %s: %s" (cadr top)
                                (mapconcat (lambda (item)
                                             (format "%s%s %d%%" (caddr top) (alist-get 'name item) (alist-get 'pct item)))
                                           items ", "))))
                    '((skills "skills" "/") (agents "subagents" "") (plugins "plugins" "")
                      (mcp_servers "MCP servers" ""))))))))

(defun agent-native-claude--usage-report (usage context)
  "The usage report: USAGE answers get_usage, CONTEXT get_context_usage
\(nil when claude gave none)."
  (let ((rate-limits (and (eq (alist-get 'rate_limits_available usage) t)
                          (alist-get 'rate_limits usage)))
        (behaviors (alist-get 'behaviors usage)))
    (list :rows (append (agent-native-claude--usage-limits rate-limits)
                        (when-let* ((percent (alist-get 'percentage context)))
                          (list (list "Context" percent
                                      (format "%s of %s tokens"
                                              (agent-native--short-count (or (alist-get 'totalTokens context) 0))
                                              (agent-native--short-count (or (alist-get 'maxTokens context) 0)))))))
          :session (agent-native-claude--usage-session (alist-get 'session usage))
          :sections (delq nil (list (agent-native-claude--usage-behaviors "Last 24h" (alist-get 'day behaviors))
                                    (agent-native-claude--usage-behaviors "Last 7d" (alist-get 'week behaviors)))))))

(cl-defmethod agent-native-backend-usage ((conn agent-native-claude) callback)
  (agent-native-claude--control
   conn "get_usage" nil
   (lambda (usage)
     (agent-native-claude--control
      conn "get_context_usage" nil
      (lambda (context)
        (funcall callback
                 (if (equal (alist-get 'subtype usage) "success")
                     (agent-native-claude--usage-report
                      (alist-get 'response usage)
                      (and (equal (alist-get 'subtype context) "success") (alist-get 'response context)))
                   (format "Claude gave no usage: %s" (or (alist-get 'error usage) "update Claude Code")))))))))

;;;; Past sessions

(defun agent-native-claude--sessions-dir (directory)
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

(defun agent-native-claude--prompt-p (record)
  "Whether RECORD is a prompt you typed (not a tool result, command echo
or a subagent's)."
  (and (equal (alist-get 'type record) "user")
       (not (eq (alist-get 'isMeta record) t))
       (not (eq (alist-get 'isSidechain record) t))
       ;; "This session is being continued…": written by /compact, not you.
       (not (eq (alist-get 'isCompactSummary record) t))
       (let ((text (agent-native-claude--prompt-text (alist-get 'content (alist-get 'message record)))))
         (and text
              ;; Slash commands and their output come back wrapped in tags.
              (not (string-prefix-p "<" text))))))

(defconst agent-native-claude--chunk 1000000
  "Bytes of a session file read at a time when looking for its first prompt.")

(defun agent-native-claude--parse-prompt (string)
  "The prompt in record STRING (undecoded bytes), or nil if it is none."
  (let ((record (agent-native--parse (decode-coding-string string 'utf-8))))
    (and record (agent-native-claude--prompt-p record)
         (agent-native-claude--prompt-text (alist-get 'content (alist-get 'message record))))))

(defun agent-native-claude--first-prompt (file)
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
          (insert-file-contents-literally file nil offset (min size (+ offset agent-native-claude--chunk)))
          (setq offset (+ offset agent-native-claude--chunk))
          (goto-char (point-min))
          ;; Whole lines only: the last may go on in the next piece.
          (while (search-forward "\n" nil t)
            (let ((line-start (line-beginning-position 0)) (line-end (1- (point))))
              (when (save-excursion (goto-char line-start) (search-forward "\"type\":\"user\"" line-end t))
                (when-let* ((prompt (agent-native-claude--parse-prompt (buffer-substring line-start line-end))))
                  (throw 'found prompt)))))
          ;; Keep only the unfinished last line.
          (delete-region (point-min) (save-excursion (goto-char (point-max)) (line-beginning-position)))
          ;; The last line of the file has no newline after it.
          (when (>= offset size)
            (when-let* ((prompt (agent-native-claude--parse-prompt (buffer-string))))
              (throw 'found prompt))))
        nil))))

(defconst agent-native-claude--title-tail 400000
  "How much of a session file's end is read for its name.")

(defun agent-native-claude--session-title (file)
  "FILE's latest title: set with /rename, else generated. Nil if none.
Reads the file's end, where the latest title lines are."
  (let ((size (file-attribute-size (file-attributes file))))
    (with-temp-buffer
      (insert-file-contents file nil (max 0 (- size agent-native-claude--title-tail)) size)
      (cl-flet ((last-title (type key)
                  (goto-char (point-max))
                  (when (search-backward (format "\"type\":\"%s\"" type) nil t)
                    (alist-get key (agent-native--parse
                                    (buffer-substring (line-beginning-position) (line-end-position)))))))
        (or (last-title "custom-title" 'customTitle)
            (last-title "ai-title" 'aiTitle))))))

(cl-defmethod agent-native-backend-sessions ((_ (eql 'claude)) directory)
  (let ((dir (agent-native-claude--sessions-dir directory)))
    (when (file-directory-p dir)
      (let (sessions)
        (dolist (file (directory-files dir t "\\.jsonl\\'"))
          (when-let* ((prompt (agent-native-claude--first-prompt file)))
            (push (list :id (file-name-base file)
                        :time (file-attribute-modification-time (file-attributes file))
                        ;; Nil when untitled: the first prompt then names it.
                        :title (agent-native-claude--session-title file)
                        :prompt prompt)
                  sessions)))
        (sort sessions (lambda (a b) (time-less-p (plist-get b :time) (plist-get a :time))))))))

(defun agent-native-claude--session-file (directory id)
  "The file of session ID in DIRECTORY."
  (expand-file-name (concat id ".jsonl") (agent-native-claude--sessions-dir directory)))

(defun agent-native-claude--replay (conn records)
  "Pass on session RECORDS, read from a session file, as events."
  (dolist (record records)
    (unless (eq (alist-get 'isSidechain record) t)
      (let ((message (alist-get 'message record)))
        (pcase (alist-get 'type record)
          ("user"
           (cond ((agent-native-claude--prompt-p record)
                  (agent-native--emit conn 'prompt :text (agent-native-claude--prompt-text (alist-get 'content message))))
                 ((vectorp (alist-get 'content message))
                  (agent-native-claude--user conn message))))
          ("assistant"
           (seq-doseq (block (alist-get 'content message))
             (pcase (alist-get 'type block)
               ("text" (agent-native--emit conn 'text :text (alist-get 'text block)))
               ("tool_use" (agent-native-claude--tool-use conn block)))))
          ;; A prompt sent while claude worked, taken into that turn.
          ("attachment"
           (let ((attachment (alist-get 'attachment record)))
             (when (and (equal (alist-get 'type attachment) "queued_command")
                        (member (alist-get 'commandMode attachment) '(nil "prompt"))
                        (stringp (alist-get 'prompt attachment)))
               (agent-native--emit conn 'prompt :text (alist-get 'prompt attachment)))))
          ("system"
           (when (equal (alist-get 'subtype record) "compact_boundary")
             (agent-native--emit conn 'note :text "✻ Conversation compacted"))))))))

(provide 'agent-native-claude)
;;; agent-native-claude.el ends here
