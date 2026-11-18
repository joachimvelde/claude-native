;;; agent-native-codex.el --- the Codex backend -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Codex runs as `codex app-server': JSON-RPC over stdio, one object per
;; line. A session is a thread; a prompt starts a turn, or joins the one
;; running. Codex reports a turn as items (messages, commands, file
;; changes, tool calls), each started and then completed; this file turns
;; them into the events of agent-native-backend.el.
;;
;; Sub-agents are threads of their own whose events come on the same
;; connection: they show as steps under the call that spawned them, never
;; in the transcript.
;;
;; What a turn should run with (model, effort, permission mode) goes along
;; with the next turn/start: Codex has no request to change it between turns.

;;; Code:

(require 'agent-native-backend)
(require 'seq)

(defcustom agent-native-codex-program (or (executable-find "codex") "codex")
  "The codex executable."
  :type 'string
  :group 'agent-native)

(defcustom agent-native-codex-extra-args nil
  "More arguments for `codex app-server', e.g. (\"-c\" \"model_verbosity=\\\"low\\\"\")."
  :type '(repeat string)
  :group 'agent-native)

(defcustom agent-native-codex-model nil
  "Model to start Codex sessions with (e.g. \"gpt-6-luna\").
Nil for Codex's own default."
  :type '(choice (const nil) string)
  :group 'agent-native)

;; Set in agent-native-render.el.
(defvar agent-native-format)
(defvar agent-native-org-prompt)

(cl-defstruct (agent-native-codex (:include agent-native-conn)
                                  (:constructor agent-native-codex--make))
  "A running `codex app-server' and its thread."
  directory
  ;; The thread's id, and whether it is ready for prompts (a resumed one
  ;; is not until its history is drawn).
  thread ready
  ;; Prompts sent before it was ready: (TEXT ATTACHMENTS) lists.
  (waiting nil)
  ;; The running turn's id; `stop' when an interrupt waits for it.
  turn stop
  model effort mode
  ;; The collaboration mode the thread has: "default" or "plan".
  (collaboration "default")
  ;; turn/start fields for the next turn: an alist.
  (overrides nil)
  (models nil)
  ;; Skills: name -> path.
  (skills nil)
  ;; agentMessage item id -> characters streamed so far.
  (streamed nil)
  ;; The turn's latest text, for the notification when it ends.
  last-text
  ;; Item id -> (NAME . INPUT) of tool calls under way.
  (tools (make-hash-table :test #'equal))
  ;; Sub-agent thread id -> (SPAWN-ITEM-ID TOOL-USES).
  (agents nil)
  ;; The thread's last token usage, and its workspace-write sandbox.
  tokens workspace
  ;; Whether to ask for a guess at the next prompt after each turn.
  (predict t))

(defconst agent-native-codex--policies
  '((default "untrusted" "workspace-write" ((type . "workspaceWrite")))
    (accept-edits "on-request" "workspace-write" ((type . "workspaceWrite")))
    (plan "on-request" "read-only" ((type . "readOnly")))
    (bypass "never" "danger-full-access" ((type . "dangerFullAccess"))))
  "Each permission mode's approval policy, sandbox, and sandbox policy.")

(cl-defmethod agent-native-backend-name ((_ (eql 'codex))) "Codex")
(cl-defmethod agent-native-backend-provider ((_ (eql 'codex))) "OpenAI")

(cl-defmethod agent-native-backend-available-p ((_ (eql 'codex)))
  (and (executable-find agent-native-codex-program) t))

(cl-defmethod agent-native-backend-modes ((_ (eql 'codex)))
  '((default . "") (accept-edits . "⏵⏵ auto") (plan . "⏸ plan mode on")
    (bypass . "⏵⏵ full access")))

;;;; JSON-RPC

(defun agent-native-codex--call (conn method &optional params callback)
  "Send request METHOD with PARAMS (an alist); CALLBACK gets the result
and the error message (one of them nil)."
  (let ((id (agent-native--next-id conn)))
    (agent-native--expect conn id (when callback
                                    (lambda (msg)
                                      (funcall callback (alist-get 'result msg)
                                               (when-let* ((error (alist-get 'error msg)))
                                                 (or (alist-get 'message error) "failed"))))))
    (agent-native--write conn `((id . ,id) (method . ,method) ,@(when params `((params . ,params)))))))

(defun agent-native-codex--respond (conn id result)
  "Answer Codex's request ID with RESULT (an alist)."
  (agent-native--write conn `((id . ,id) (result . ,(or result (make-hash-table))))))

(defun agent-native-codex--refuse (conn id)
  "Answer Codex's request ID: not something done here."
  (agent-native--write conn `((id . ,id) (error . ((code . -32601) (message . "Not supported by agent-native"))))))

(defun agent-native-codex--initialize (conn then)
  "Do CONN's handshake, then call THEN."
  (agent-native-codex--call
   conn "initialize"
   `((clientInfo . ((name . "agent-native") (title . "agent-native") (version . "0.2.0")))
     ;; Plan mode and next-prompt guesses are experimental.
     (capabilities . ((experimentalApi . t))))
   (lambda (_ error)
     (if error
         (agent-native--emit conn 'note :text (format "Codex: %s" error) :face 'agent-native-tool-error)
       (agent-native--write conn '((method . "initialized")))
       (funcall then)))))

;;;; Starting

(cl-defmethod agent-native-backend-start ((_ (eql 'codex)) directory &key resume on-event on-exit mode)
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (conn (agent-native-codex--make :backend 'codex :on-event on-event :on-exit on-exit
                                         :directory (directory-file-name default-directory)
                                         :mode (or mode 'default))))
    (agent-native--spawn conn "agent-native-codex"
                         `(,agent-native-codex-program "app-server" ,@agent-native-codex-extra-args))
    (agent-native-codex--initialize conn (lambda ()
                                           ;; Models first: the model picker waits for them.
                                           (agent-native-codex--load-models conn)
                                           (agent-native-codex--open-thread conn resume)))
    conn))

(defun agent-native-codex--open-thread (conn resume)
  "Start CONN's thread, or RESUME that one."
  (pcase-let ((`(,approval ,sandbox . ,_) (alist-get (agent-native-codex-mode conn) agent-native-codex--policies)))
    (agent-native-codex--call
     conn (if resume "thread/resume" "thread/start")
     `(,@(when resume `((threadId . ,resume) (excludeTurns . t)))
       (cwd . ,(agent-native-codex-directory conn))
       (approvalPolicy . ,approval)
       (sandbox . ,sandbox)
       ,@(when agent-native-codex-model `((model . ,agent-native-codex-model)))
       ,@(when (eq agent-native-format 'org) `((developerInstructions . ,agent-native-org-prompt))))
     (lambda (result error)
       (if error
           (agent-native--emit conn 'note :text (format "Codex could not %s the session: %s"
                                                        (if resume "resume" "start") error)
                               :face 'agent-native-tool-error)
         (agent-native-codex--opened conn result resume))))))

(defun agent-native-codex--opened (conn result resume)
  "The thread is there (RESULT of thread/start or thread/resume)."
  (let* ((thread (alist-get 'thread result))
         ;; A model or effort picked while the thread started goes with
         ;; its first turn: keep showing it.
         (overrides (agent-native-codex-overrides conn))
         (model (if (assq 'model overrides) (alist-get 'model overrides) (alist-get 'model result)))
         (effort (if (assq 'effort overrides) (alist-get 'effort overrides) (alist-get 'reasoningEffort result))))
    (setf (agent-native-codex-thread conn) (alist-get 'id thread)
          (agent-native-codex-model conn) model
          (agent-native-codex-effort conn) effort)
    (agent-native-codex--keep-sandbox conn (alist-get 'sandbox result))
    (agent-native--emit conn 'session :id (alist-get 'id thread) :model model :effort effort
                        :mode (agent-native-codex-mode conn))
    (agent-native-codex--load-skills conn)
    (if resume
        (progn (agent-native--emit conn 'history-start)
               (agent-native-codex--load-history conn nil))
      (agent-native-codex--ready conn))))

(defun agent-native-codex--ready (conn)
  "CONN takes prompts now: send those that waited."
  (setf (agent-native-codex-ready conn) t)
  (dolist (prompt (agent-native-codex-waiting conn))
    (apply #'agent-native-codex--send conn prompt))
  (setf (agent-native-codex-waiting conn) nil))

(defun agent-native-codex--load-models (conn)
  "Ask which models there are, and pass them on."
  (agent-native-codex--call
   conn "model/list" '((limit . 100))
   (lambda (result _)
     (let ((models (mapcar (lambda (model)
                             (list :value (alist-get 'id model)
                                   :aliases (delete-dups (list (alist-get 'id model) (alist-get 'model model)))
                                   :name (alist-get 'displayName model)
                                   :description (alist-get 'description model)
                                   :default-effort (alist-get 'defaultReasoningEffort model)
                                   :efforts (mapcar (lambda (e) (alist-get 'reasoningEffort e))
                                                    (alist-get 'supportedReasoningEfforts model))))
                           (seq-remove (lambda (model) (eq (alist-get 'hidden model) t))
                                       (alist-get 'data result)))))
       (setf (agent-native-codex-models conn) models)
       (agent-native--emit conn 'session :models models)))))

(defconst agent-native-codex--commands
  '(("compact" . "Summarize the conversation so far, to free context"))
  "Slash commands done by this backend.")

(defun agent-native-codex--load-skills (conn)
  "Ask which skills there are: each is a slash command."
  (agent-native-codex--call
   conn "skills/list" `((cwds . [,(agent-native-codex-directory conn)]))
   (lambda (result _)
     (let ((skills (seq-filter (lambda (skill) (not (eq (alist-get 'enabled skill) :false)))
                               (seq-mapcat (lambda (entry) (append (alist-get 'skills entry) nil))
                                           (alist-get 'data result)))))
       (setf (agent-native-codex-skills conn)
             (mapcar (lambda (skill) (cons (alist-get 'name skill) (alist-get 'path skill))) skills))
       (agent-native--emit conn 'session
                           :commands (append agent-native-codex--commands
                                             (mapcar (lambda (skill)
                                                       (cons (alist-get 'name skill)
                                                             (car (split-string (or (alist-get 'description skill) "") "\n"))))
                                                     skills)))))))

(defun agent-native-codex--load-history (conn cursor)
  "Pass on the resumed thread's turns, a page at a time from CURSOR."
  (agent-native-codex--call
   conn "thread/turns/list"
   `((threadId . ,(agent-native-codex-thread conn)) (itemsView . "full") (sortDirection . "asc")
     (limit . 50) ,@(when cursor `((cursor . ,cursor))))
   (lambda (result error)
     (when error
       (agent-native--emit conn 'note :text (format "Codex could not read the conversation: %s" error)
                           :face 'agent-native-tool-error))
     (seq-doseq (turn (alist-get 'data result))
       (seq-doseq (item (alist-get 'items turn))
         (agent-native-codex--item conn item 'history))
       (when (equal (alist-get 'status turn) "interrupted")
         (agent-native--emit conn 'interrupted)))
     (if-let* ((next (and (not error) (alist-get 'nextCursor result))))
         (agent-native-codex--load-history conn next)
       (setf (agent-native-codex-agents conn) nil)
       (agent-native--emit conn 'history-end)
       (agent-native-codex--ready conn)))))

;;;; Messages from Codex

(cl-defmethod agent-native-backend-receive ((conn agent-native-codex) msg)
  (let ((method (alist-get 'method msg))
        (id (alist-get 'id msg))
        (params (alist-get 'params msg)))
    (cond
     ((and method id) (agent-native-codex--request conn id method params))
     (method (agent-native-codex--notification conn method params))
     (id (agent-native--answered conn id msg)))))

(defun agent-native-codex--notification (conn method params)
  "Pass on notification METHOD with PARAMS: the thread's, or a sub-agent's."
  (let ((thread (or (alist-get 'threadId params) (alist-get 'id (alist-get 'thread params)))))
    (cond
     ((null thread) nil)
     ((equal thread (agent-native-codex-thread conn)) (agent-native-codex--thread-event conn method params))
     ((assoc thread (agent-native-codex-agents conn)) (agent-native-codex--agent-event conn thread method params)))))

(defun agent-native-codex--thread-event (conn method params)
  "Pass on METHOD with PARAMS, about the session's own thread."
  (pcase method
    ("thread/settings/updated" (agent-native-codex--take-settings conn (alist-get 'threadSettings params)))
    ("turn/started"
     (setf (agent-native-codex-turn conn) (alist-get 'id (alist-get 'turn params))
           (agent-native-codex-last-text conn) nil)
     (agent-native--emit conn 'working :status "Working…")
     ;; Interrupted before Codex said which turn runs.
     (when (agent-native-codex-stop conn)
       (agent-native-backend-interrupt conn)))
    ("item/started" (agent-native-codex--item conn (alist-get 'item params) 'started))
    ("item/completed" (agent-native-codex--item conn (alist-get 'item params) 'completed))
    ("item/agentMessage/delta"
     (when-let* ((entry (assoc (alist-get 'itemId params) (agent-native-codex-streamed conn))))
       (cl-incf (cdr entry) (length (alist-get 'delta params)))
       (agent-native--emit conn 'text-delta :text (alist-get 'delta params))))
    ("turn/plan/updated" (agent-native-codex--plan conn (alist-get 'plan params)))
    ("thread/tokenUsage/updated" (setf (agent-native-codex-tokens conn) (alist-get 'tokenUsage params)))
    ("turn/completed" (agent-native-codex--turn-completed conn (alist-get 'turn params)))
    ("thread/prediction/updated"
     (let ((result (alist-get 'result params)))
       (when (and (equal (alist-get 'type result) "completed") (stringp (alist-get 'text result)))
         (agent-native--emit conn 'suggestion :text (string-trim (alist-get 'text result))))))
    ("serverRequest/resolved" (agent-native--emit conn 'request-done :id (alist-get 'requestId params)))
    ("error" (when (eq (alist-get 'willRetry params) t)
               (agent-native--emit conn 'working :status "Retrying…")))))

(defun agent-native-codex--take-settings (conn settings)
  "Pass on the thread's SETTINGS: model and effort."
  (setf (agent-native-codex-model conn) (alist-get 'model settings)
        (agent-native-codex-effort conn) (alist-get 'effort settings))
  (agent-native-codex--keep-sandbox conn (alist-get 'sandboxPolicy settings))
  (agent-native--emit conn 'session :model (alist-get 'model settings) :effort (alist-get 'effort settings)))

(defun agent-native-codex--keep-sandbox (conn policy)
  "Keep POLICY if it is a workspace-write one: going back to that mode
later keeps its writable roots and network access."
  (when (equal (alist-get 'type policy) "workspaceWrite")
    (setf (agent-native-codex-workspace conn) policy)))

(defun agent-native-codex--turn-completed (conn turn)
  "TURN ended: completed, interrupted, or failed."
  (let ((status (alist-get 'status turn)))
    (setf (agent-native-codex-turn conn) nil
          (agent-native-codex-stop conn) nil
          (agent-native-codex-streamed conn) nil)
    (when (equal status "interrupted")
      (agent-native--emit conn 'interrupted))
    (agent-native--emit conn 'turn-end
                        :duration-ms (and (equal status "completed") (alist-get 'durationMs turn))
                        :error (and (equal status "failed")
                                    (or (alist-get 'message (alist-get 'error turn)) "The turn failed"))
                        :text (agent-native-codex-last-text conn))
    ;; A guess at your next prompt. Experimental: a Codex without it says
    ;; so, and is not asked again.
    (when (and (equal status "completed") (agent-native-codex-predict conn))
      (agent-native-codex--call conn "thread/prediction/request"
                                `((threadId . ,(agent-native-codex-thread conn))
                                  (sourceTurnId . ,(alist-get 'id turn)))
                                (lambda (_ error)
                                  (when error (setf (agent-native-codex-predict conn) nil)))))))

;;;; Items

(defun agent-native-codex--unwrap (command)
  "COMMAND without the shell Codex runs it in (\"/bin/zsh -lc '…'\")."
  (if (and (stringp command)
           (string-match "\\`[^ ]*/\\(?:ba\\|z\\)?sh -l?c \\(\\(?:.\\|\n\\)+\\)\\'" command))
      (let ((quoted (match-string 1 command)))
        (cond ((string-match "\\`'\\(\\(?:.\\|\n\\)*\\)'\\'" quoted)
               (string-replace "'\\''" "'" (match-string 1 quoted)))
              ((string-match "\\`\"\\(\\(?:.\\|\n\\)*\\)\"\\'" quoted)
               (replace-regexp-in-string "\\\\\\([\"\\\\$`]\\)" "\\1" (match-string 1 quoted)))
              (t quoted)))
    command))

(defun agent-native-codex--command (item)
  "Command ITEM as a tool call (NAME . INPUT): a read, search or listing
when that is all it does, else a shell command."
  (let* ((actions (append (alist-get 'commandActions item) nil))
         (action (and (= (length actions) 1) (car actions)))
         (command (if action (alist-get 'command action) (agent-native-codex--unwrap (alist-get 'command item)))))
    (pcase (alist-get 'type action)
      ("read" (cons "Read" `((file_path . ,(alist-get 'path action)) (command . ,command))))
      ("search" (cons "Search" `((pattern . ,(or (alist-get 'query action) command))
                                 (path . ,(alist-get 'path action)) (command . ,command))))
      ("listFiles" (cons "List" `((path . ,(alist-get 'path action)) (command . ,command))))
      (_ (cons "Shell" `((command . ,command)))))))

(defun agent-native-codex--file-change (item)
  "File change ITEM as a tool call (NAME . INPUT)."
  (let* ((changes (append (alist-get 'changes item) nil))
         (kind (lambda (change) (alist-get 'type (alist-get 'kind change)))))
    (cond
     ((and (= (length changes) 1) (equal (funcall kind (car changes)) "add"))
      (cons "Write" `((file_path . ,(alist-get 'path (car changes))) (content . ,(alist-get 'diff (car changes))))))
     ((and (= (length changes) 1) (equal (funcall kind (car changes)) "delete"))
      (cons "Delete" `((file_path . ,(alist-get 'path (car changes))))))
     (t
      (cons "Edit" `((files . ,(mapcar (lambda (change) (alist-get 'path change)) changes))
                     ;; A new file's "diff" is its text: make it added lines.
                     (diff . ,(mapconcat (lambda (change)
                                           (if (equal (funcall kind change) "add")
                                               (mapconcat (lambda (line) (concat "+" line))
                                                          (split-string (string-trim-right (alist-get 'diff change) "\n") "\n")
                                                          "\n")
                                             (alist-get 'diff change)))
                                         changes "\n"))))))))

(defun agent-native-codex--tool (item)
  "ITEM as a tool call: (NAME . INPUT), or nil if it is none."
  (pcase (alist-get 'type item)
    ("commandExecution" (agent-native-codex--command item))
    ("fileChange" (agent-native-codex--file-change item))
    ("mcpToolCall"
     (let ((arguments (alist-get 'arguments item)))
       (cons (format "%s.%s" (alist-get 'server item) (alist-get 'tool item))
             (if (and (consp arguments) (consp (car arguments))) arguments `((arguments . ,arguments))))))
    ("dynamicToolCall"
     (let ((arguments (alist-get 'arguments item)))
       (cons (alist-get 'tool item)
             (if (and (consp arguments) (consp (car arguments))) arguments `((arguments . ,arguments))))))
    ("webSearch" (cons "WebSearch" `((query . ,(alist-get 'query item)))))
    ("imageView" (cons "Read" `((file_path . ,(alist-get 'path item)))))
    ("imageGeneration" (cons "ImageGen" `((prompt . ,(or (alist-get 'revisedPrompt item) "")))))))

(defun agent-native-codex--result (item)
  "Tool call ITEM's result: (CONTENT . ERROR)."
  (let ((status (alist-get 'status item)))
    (pcase (alist-get 'type item)
      ("commandExecution"
       (if (equal status "declined")
           (cons "Declined" t)
         (let ((exit (alist-get 'exitCode item)))
           (cons (or (alist-get 'aggregatedOutput item) "")
                 (or (equal status "failed") (and (numberp exit) (/= exit 0)))))))
      ("fileChange"
       (if (member status '("failed" "declined"))
           (cons (capitalize status) t)
         (cons "" nil)))
      ("mcpToolCall"
       (if-let* ((error (alist-get 'error item)))
           (cons (or (alist-get 'message error) "Failed") t)
         (cons (or (alist-get 'content (alist-get 'result item)) "") nil)))
      ("dynamicToolCall"
       (cons (or (alist-get 'contentItems item) "") (eq (alist-get 'success item) :false)))
      ("webSearch" (cons (format "Searched: %s" (or (alist-get 'query item) "")) nil))
      ("imageView" (cons "[Image]" nil))
      ("imageGeneration"
       (if (alist-get 'failure item)
           (cons "Image generation failed" t)
         (cons (format "Saved %s" (or (alist-get 'savedPath item) "")) nil))))))

(defun agent-native-codex--prompt-text (item)
  "The prompt in userMessage ITEM: its first text (more texts after it
name attached files)."
  (seq-some (lambda (input) (and (equal (alist-get 'type input) "text") (alist-get 'text input)))
            (alist-get 'content item)))

(defun agent-native-codex--item (conn item phase)
  "Pass on thread ITEM as it starts or completes (PHASE `started',
`completed'), or whole, from a resumed session (`history')."
  (let ((id (alist-get 'id item))
        (start (memq phase '(started history)))
        (done (memq phase '(completed history))))
    (pcase (alist-get 'type item)
      ("userMessage"
       (when-let* ((text (and done (agent-native-codex--prompt-text item))))
         (agent-native--emit conn 'prompt :text text)))
      ("agentMessage"
       (let ((streamed (assoc id (agent-native-codex-streamed conn))))
         (cond
          ((eq phase 'started)
           (push (cons id 0) (agent-native-codex-streamed conn))
           (agent-native--emit conn 'text-start))
          (streamed
           (agent-native--emit conn 'text-end)
           ;; Nothing came as it was written: draw it whole.
           (when (zerop (cdr streamed))
             (agent-native--emit conn 'text :text (alist-get 'text item))))
          (t (agent-native--emit conn 'text :text (alist-get 'text item)))))
       (when done (setf (agent-native-codex-last-text conn) (alist-get 'text item))))
      ;; Plan mode's proposed plan.
      ("plan" (when done (agent-native--emit conn 'text :text (alist-get 'text item))))
      ("reasoning" (when (eq phase 'started) (agent-native--emit conn 'working :status "Thinking…")))
      ("contextCompaction"
       (if (eq phase 'started)
           (agent-native--emit conn 'working :status "Compacting…")
         (agent-native--emit conn 'note :text "✻ Conversation compacted")))
      ("enteredReviewMode" (when done (agent-native--emit conn 'note :text "✻ Reviewing")))
      ("subAgentActivity" (agent-native-codex--sub-agent conn item phase))
      ("collabAgentToolCall" (agent-native-codex--collab conn item phase))
      (_
       (when-let* ((tool (agent-native-codex--tool item)))
         ;; Also a call whose start was missed (its result is all there is).
         (when (or start (not (gethash id (agent-native-codex-tools conn))))
           (puthash id tool (agent-native-codex-tools conn))
           (agent-native--emit conn 'tool :id id :name (car tool) :input (cdr tool)))
         (when done
           (remhash id (agent-native-codex-tools conn))
           (pcase-let ((`(,content . ,error) (agent-native-codex--result item)))
             (agent-native--emit conn 'tool-result :id id :content content :error error))))))))

(defun agent-native-codex--plan (conn plan)
  "Codex updated its PLAN: drawn as a todo list, as Claude's are."
  (let ((id (format "plan-%d" (agent-native--next-id conn))))
    (agent-native--emit conn 'tool :id id :name "TodoWrite"
                        :input `((todos . ,(mapcar (lambda (step)
                                                     `((content . ,(alist-get 'step step))
                                                       (status . ,(pcase (alist-get 'status step)
                                                                    ("inProgress" "in_progress")
                                                                    (status status)))))
                                                   plan))))
    (agent-native--emit conn 'tool-result :id id :content "")))

;;;; Sub-agents

(defun agent-native-codex--agent-spawned (conn child call name model effort phase)
  "Sub-agent thread CHILD was spawned by item CALL, as NAME."
  (push (list child call 0) (agent-native-codex-agents conn))
  (unless (eq phase 'history)
    (agent-native--emit conn 'agent-start :id child :tool-id call :name name :model model :effort effort)))

(defun agent-native-codex--agent-done (conn child failed)
  "Sub-agent thread CHILD ended; FAILED if not well."
  (when-let* ((entry (assoc child (agent-native-codex-agents conn))))
    (agent-native--emit conn 'tool-result :id (nth 1 entry) :content "" :error failed)
    (agent-native--emit conn 'agent-end :id child)
    (setf (agent-native-codex-agents conn) (assoc-delete-all child (agent-native-codex-agents conn)))))

(defun agent-native-codex--sub-agent (conn item phase)
  "A sub-agent's ITEM in the session's thread: it started, or ended."
  (let ((child (alist-get 'agentThreadId item)))
    (pcase (alist-get 'kind item)
      ("started"
       (when (memq phase '(started history))
         (let ((name (file-name-nondirectory (or (alist-get 'agentPath item) "agent"))))
           (agent-native--emit conn 'tool :id (alist-get 'id item) :name "Agent" :input `((description . ,name)))
           (agent-native-codex--agent-spawned conn child (alist-get 'id item) name (alist-get 'model item)
                                              (alist-get 'reasoningEffort item) phase))))
      ((or "completed" "interrupted")
       (when (memq phase '(completed history))
         (agent-native-codex--agent-done conn child (equal (alist-get 'kind item) "interrupted")))))))

(defun agent-native-codex--collab (conn item phase)
  "An agent tool call ITEM (older Codex): spawn, wait, and the rest."
  (let ((id (alist-get 'id item))
        (prompt (or (alist-get 'prompt item) "")))
    (pcase (alist-get 'tool item)
      ("wait"
       (if (eq phase 'started)
           (agent-native--emit conn 'working :status "Waiting for agents…")
         (dolist (state (alist-get 'agentsStates item))
           (when (member (alist-get 'status (cdr state)) '("completed" "errored" "shutdown" "interrupted" "notFound"))
             (agent-native-codex--agent-done conn (symbol-name (car state))
                                             (not (equal (alist-get 'status (cdr state)) "completed")))))))
      ("spawnAgent"
       (when (memq phase '(started history))
         (agent-native--emit conn 'tool :id id :name "Agent"
                             :input `((description . ,(car (split-string prompt "\n"))))))
       (when (memq phase '(completed history))
         (seq-doseq (child (alist-get 'receiverThreadIds item))
           (agent-native-codex--agent-spawned conn child id (car (split-string prompt "\n"))
                                              (alist-get 'model item) (alist-get 'reasoningEffort item) phase))))
      (tool
       (when (memq phase '(started history))
         (agent-native--emit conn 'tool :id id :name (capitalize tool) :input `((prompt . ,prompt))))
       (when (memq phase '(completed history))
         (agent-native--emit conn 'tool-result :id id
                             :content (mapconcat (lambda (state) (or (alist-get 'message (cdr state)) ""))
                                                 (alist-get 'agentsStates item) "\n")
                             :error (equal (alist-get 'status item) "failed")))))))

(defun agent-native-codex--step (name input)
  "A short line for a sub-agent's tool call NAME with INPUT."
  (let ((what (seq-some (lambda (key) (alist-get key input)) '(command file_path pattern query description))))
    (if (stringp what)
        (format "%s(%s)" name (truncate-string-to-width (car (split-string what "\n")) 60 nil nil "…"))
      name)))

(defun agent-native-codex--agent-event (conn child method params)
  "Pass on METHOD with PARAMS from sub-agent thread CHILD: its tool calls
as steps, its token use."
  (let ((entry (assoc child (agent-native-codex-agents conn))))
    (pcase method
      ("item/started"
       (when-let* ((tool (agent-native-codex--tool (alist-get 'item params))))
         (cl-incf (nth 2 entry))
         (agent-native--emit conn 'agent-step :tool-id (nth 1 entry) :name (car tool) :input (cdr tool))
         (agent-native--emit conn 'agent-progress :id child :step (agent-native-codex--step (car tool) (cdr tool))
                             :tools (nth 2 entry))))
      ("thread/tokenUsage/updated"
       (agent-native--emit conn 'agent-progress :id child
                           :tokens (alist-get 'totalTokens (alist-get 'total (alist-get 'tokenUsage params))))))))

;;;; Requests from Codex

(defun agent-native-codex--request (conn id method params)
  "Codex asks request ID, METHOD with PARAMS: permission, or questions."
  (let* ((item (alist-get 'itemId params))
         (tool (gethash item (agent-native-codex-tools conn))))
    (pcase method
      ("item/commandExecution/requestApproval"
       (let ((decisions (append (alist-get 'availableDecisions params) nil)))
         (agent-native--emit conn 'request :id id :tool-id item :kind 'command
                             :name (or (car tool) "Shell")
                             :input (or (cdr tool) `((command . ,(agent-native-codex--unwrap (alist-get 'command params)))))
                             :decisions decisions
                             :always (agent-native-codex--always-text decisions))))
      ("item/fileChange/requestApproval"
       (agent-native--emit conn 'request :id id :tool-id item :kind 'file
                           :name (or (car tool) "Edit") :input (cdr tool)
                           :always "Yes, and don't ask again for these files in this session"))
      ("item/permissions/requestApproval"
       (agent-native--emit conn 'request :id id :tool-id item :kind 'permissions :name "Permissions"
                           :input `((reason . ,(or (alist-get 'reason params) "more access")))
                           :permissions (alist-get 'permissions params)
                           :always "Yes, for the rest of this session"))
      ("item/tool/requestUserInput"
       (agent-native--emit conn 'request :id id :tool-id item :kind 'questions :name "Question"
                           :questions (mapcar (lambda (q)
                                                (list :id (alist-get 'id q) :question (alist-get 'question q)
                                                      :options (mapcar (lambda (o) (alist-get 'label o))
                                                                       (alist-get 'options q))))
                                              (alist-get 'questions params))))
      ;; An MCP server's form: there is no way to fill one in here.
      ("mcpServer/elicitation/request" (agent-native-codex--respond conn id '((action . "decline"))))
      (_ (agent-native-codex--refuse conn id)))))

(defun agent-native-codex--always (decisions)
  "The decision for \"yes, and don't ask again\" among DECISIONS."
  (cond ((or (null decisions) (member "acceptForSession" decisions)) "acceptForSession")
        ;; A rule allowing commands like this one.
        ((seq-find (lambda (d) (and (consp d) (assq 'acceptWithExecpolicyAmendment d))) decisions))
        (t "accept")))

(defun agent-native-codex--always-text (decisions)
  "What answering \"always\" keeps, given DECISIONS. A rule outlives the
session: Codex saves it with its own (in ~/.codex/rules)."
  (let ((always (agent-native-codex--always decisions)))
    (cond ((equal always "acceptForSession") "Yes, and don't ask again for this in this session")
          ((consp always)
           (format "Yes, and from now on, in every project, allow commands that start with `%s`"
                   (mapconcat #'identity (alist-get 'execpolicy_amendment (alist-get 'acceptWithExecpolicyAmendment always))
                              " ")))
          (t "Yes, this time (Codex offers nothing to keep)"))))

(cl-defmethod agent-native-backend-answer ((conn agent-native-codex) request answer)
  (let ((decisions (plist-get request :decisions)))
    (agent-native-codex--respond
     conn (plist-get request :id)
     (pcase (plist-get request :kind)
       ('questions
        `((answers . ,(mapcar (lambda (a) (cons (intern (car a)) `((answers . [,(cdr a)]))))
                              (cdr-safe answer)))))
       ('permissions
        (if (eq answer 'deny)
            `((permissions . ,(make-hash-table)))
          `((permissions . ,(plist-get request :permissions))
            (scope . ,(if (eq answer 'always) "session" "turn")))))
       (_
        `((decision . ,(pcase answer
                         ('allow "accept")
                         ('always (agent-native-codex--always decisions))
                         ;; No: and stop the turn, as with Claude.
                         (_ (if (or (null decisions) (member "cancel" decisions)) "cancel" "decline"))))))))))

;;;; Operations

(defun agent-native-codex--want-mode (conn mode)
  "Run the next turn in permission MODE."
  (pcase-let ((`(,_ ,approval ,_ ,policy) (assq mode agent-native-codex--policies)))
    (setf (agent-native-codex-mode conn) mode)
    (setf (alist-get 'approvalPolicy (agent-native-codex-overrides conn)) approval
          (alist-get 'sandboxPolicy (agent-native-codex-overrides conn))
          (if (equal (alist-get 'type policy) "workspaceWrite")
              (or (agent-native-codex-workspace conn) policy)
            policy))))

(defun agent-native-codex--take-overrides (conn)
  "The settings for the turn about to start, and forget them. None when
a turn runs: the prompt joins it, and they wait for the next."
  (unless (agent-native-codex-turn conn)
    (let ((collaboration (if (eq (agent-native-codex-mode conn) 'plan) "plan" "default")))
      (prog1 (append (agent-native-codex-overrides conn)
                     (unless (equal collaboration (agent-native-codex-collaboration conn))
                       (setf (agent-native-codex-collaboration conn) collaboration)
                       ;; Its model and effort take precedence: the ones wanted.
                       `((collaborationMode
                          . ((mode . ,collaboration)
                             (settings . ((model . ,(agent-native-codex-model conn))
                                          (reasoning_effort . ,(agent-native-codex-effort conn))
                                          (developer_instructions))))))))
        (setf (agent-native-codex-overrides conn) nil)))))

(defun agent-native-codex--input (text skill attachments)
  "The input items for prompt TEXT: the text first, then SKILL (NAME .
PATH) when it calls one, then ATTACHMENTS."
  (vconcat
   (list `((type . "text") (text . ,text)))
   (when skill
     (list `((type . "skill") (name . ,(car skill)) (path . ,(cdr skill)))))
   (mapcar (pcase-lambda (`(,kind ,file ,_))
             (if (eq kind 'image)
                 `((type . "localImage") (path . ,file))
               `((type . "text") (text . ,(format "Attached file: %s" file)))))
           attachments)))

(defun agent-native-codex--send (conn text attachments)
  "Send prompt TEXT with ATTACHMENTS: a turn, or a part of the one running."
  (if (string-match-p "\\`/compact\\s-*\\'" text)
      (agent-native-codex--call conn "thread/compact/start" `((threadId . ,(agent-native-codex-thread conn)))
                                (lambda (_ error)
                                  (when error
                                    (setf (agent-native-codex-stop conn) nil)
                                    (agent-native--emit conn 'turn-end :error error))))
    (let ((skill (and (string-match "\\`/\\([^ \n]+\\)" text)
                      (assoc (match-string 1 text) (agent-native-codex-skills conn))))
          (running (agent-native-codex-turn conn)))
      (agent-native-codex--call
       conn "turn/start"
       `((threadId . ,(agent-native-codex-thread conn))
         (input . ,(agent-native-codex--input text skill attachments))
         ,@(agent-native-codex--take-overrides conn))
       (lambda (result error)
         (cond
          ((and error running)
           (agent-native--emit conn 'note :text (format "Error: %s" error) :face 'agent-native-tool-error))
          ;; No turn, so an interrupt waiting for one is moot: it would
          ;; stop the next.
          (error (setf (agent-native-codex-stop conn) nil)
                 (agent-native--emit conn 'turn-end :error error))
          ((not (agent-native-codex-turn conn))
           (setf (agent-native-codex-turn conn) (alist-get 'id (alist-get 'turn result))))))))))

(cl-defmethod agent-native-backend-send ((conn agent-native-codex) text attachments)
  (if (agent-native-codex-ready conn)
      (agent-native-codex--send conn text attachments)
    (setf (agent-native-codex-waiting conn)
          (append (agent-native-codex-waiting conn) (list (list text attachments))))))

(cl-defmethod agent-native-backend-interrupt ((conn agent-native-codex))
  (if-let* ((turn (agent-native-codex-turn conn)))
      (agent-native-codex--call conn "turn/interrupt"
                                `((threadId . ,(agent-native-codex-thread conn)) (turnId . ,turn)))
    (setf (agent-native-codex-stop conn) t)))

(cl-defmethod agent-native-backend-set-mode ((conn agent-native-codex) mode)
  (agent-native-codex--want-mode conn mode))

(defun agent-native-codex--want (conn model effort)
  "Run the next turn with MODEL at EFFORT, and say so now."
  (setf (agent-native-codex-model conn) model
        (agent-native-codex-effort conn) effort
        (alist-get 'model (agent-native-codex-overrides conn)) model
        (alist-get 'effort (agent-native-codex-overrides conn)) effort)
  ;; A collaboration mode carries its own model: set it again.
  (when (eq (agent-native-codex-mode conn) 'plan)
    (setf (agent-native-codex-collaboration conn) nil))
  (agent-native--emit conn 'session :model model :effort effort))

(cl-defmethod agent-native-backend-set-model ((conn agent-native-codex) model effort)
  (let ((info (seq-find (lambda (m) (member model (plist-get m :aliases))) (agent-native-codex-models conn)))
        (current (agent-native-codex-effort conn)))
    (agent-native-codex--want
     conn model
     (or effort
         ;; As with Claude, the effort stays, if the new model has it.
         (and (member current (plist-get info :efforts)) current)
         (plist-get info :default-effort)
         current))))

(cl-defmethod agent-native-backend-set-effort ((conn agent-native-codex) effort)
  (agent-native-codex--want conn (agent-native-codex-model conn) effort))

;;;; Usage

(defun agent-native-codex--window-title (minutes)
  "The name of a limit window MINUTES long."
  (pcase minutes
    (300 "Current 5h")
    (10080 "Current week")
    ((pred numberp) (if (zerop (% minutes 1440))
                        (format "Current %dd" (/ minutes 1440))
                      (format "Current %dh" (/ minutes 60))))
    (_ "Current window")))

(defun agent-native-codex--limit-rows (rate-limits)
  "Bars for RATE-LIMITS (an account/rateLimits/read result)."
  (let* ((snapshots (or (mapcar #'cdr (alist-get 'rateLimitsByLimitId rate-limits))
                        (list (alist-get 'rateLimits rate-limits))))
         (several (cdr snapshots)))
    (seq-mapcat
     (lambda (snapshot)
       (delq nil
             (mapcar (lambda (key)
                       (when-let* ((window (alist-get key snapshot))
                                   ((numberp (alist-get 'usedPercent window))))
                         (list (concat (agent-native-codex--window-title (alist-get 'windowDurationMins window))
                                       (when several
                                         (format " (%s)" (or (alist-get 'limitName snapshot)
                                                             (alist-get 'limitId snapshot)))))
                               (alist-get 'usedPercent window)
                               (when (numberp (alist-get 'resetsAt window))
                                 (format-time-string "resets %b %-e, %H:%M"
                                                     (seconds-to-time (alist-get 'resetsAt window)))))))
                     '(primary secondary))))
     snapshots)))

(defun agent-native-codex--usage-report (rate-limits tokens usage)
  "The usage report from RATE-LIMITS, the thread's TOKENS and the
account's USAGE (results of account/rateLimits/read and account/usage/read)."
  (let* ((total (alist-get 'total tokens))
         (last (alist-get 'last tokens))
         (window (alist-get 'modelContextWindow tokens))
         (credits (alist-get 'credits (alist-get 'rateLimits rate-limits)))
         (summary (alist-get 'summary usage))
         (days (append (alist-get 'dailyUsageBuckets usage) nil))
         ;; Days without use have no bucket: count by date (ISO dates sort as text).
         (today (format-time-string "%F" nil t))
         (week-start (format-time-string "%F" (time-subtract nil (days-to-time 6)) t))
         (tokens-since (lambda (date)
                         (seq-reduce #'+ (mapcar (lambda (day)
                                                   (if (string< (or (alist-get 'startDate day) "") date) 0
                                                     (or (alist-get 'tokens day) 0)))
                                                 days)
                                     0)))
         (week (funcall tokens-since week-start)))
    (list :rows (append (agent-native-codex--limit-rows rate-limits)
                        (when (and (numberp window) (> window 0) last)
                          (let ((used (or (alist-get 'totalTokens last) 0)))
                            (list (list "Context" (/ (* 100.0 used) window)
                                        (format "%s of %s tokens" (agent-native--short-count used)
                                                (agent-native--short-count window)))))))
          :session (string-join
                    (delq nil (list (format "%s tokens" (agent-native--short-count (or (alist-get 'totalTokens total) 0)))
                                    (when (alist-get 'cachedInputTokens total)
                                      (format "%s cached" (agent-native--short-count (alist-get 'cachedInputTokens total))))
                                    (when (and credits (eq (alist-get 'hasCredits credits) t)
                                               (not (eq (alist-get 'unlimited credits) t))
                                               (alist-get 'balance credits))
                                      (format "%s credits left" (round (string-to-number (alist-get 'balance credits)))))))
                    " · ")
          :sections (when days
                      (list (cons (string-join
                                   (delq nil (list (format "Last 7d · %s tokens" (agent-native--short-count week))
                                                   (when (numberp (alist-get 'currentStreakDays summary))
                                                     (format "%d-day streak" (alist-get 'currentStreakDays summary)))))
                                   " · ")
                                  (delq nil (list (when (seq-find (lambda (day) (equal (alist-get 'startDate day) today)) days)
                                                    (format "  Today %s tokens" (agent-native--short-count
                                                                                 (funcall tokens-since today))))
                                                  (when (numberp (alist-get 'peakDailyTokens summary))
                                                    (format "  Peak day %s tokens"
                                                            (agent-native--short-count (alist-get 'peakDailyTokens summary))))))))))))

(cl-defmethod agent-native-backend-usage ((conn agent-native-codex) callback)
  (agent-native-codex--call
   conn "account/rateLimits/read" nil
   (lambda (rate-limits _)
     (agent-native-codex--call
      conn "account/usage/read" nil
      (lambda (usage _)
        (funcall callback (agent-native-codex--usage-report rate-limits (agent-native-codex-tokens conn) usage)))))))

;;;; Past sessions

(defun agent-native-codex--ask (directory method params)
  "The result of METHOD with PARAMS, asked of a Codex started in DIRECTORY
for it, and stopped after."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (conn (agent-native-codex--make :backend 'codex))
         done result failure)
    (agent-native--spawn conn "agent-native-codex-ask" (list agent-native-codex-program "app-server"))
    (unwind-protect
        (progn
          (agent-native-codex--call
           conn "initialize" '((clientInfo . ((name . "agent-native") (version . "0.2.0"))))
           (lambda (_ error)
             (if error
                 (setq failure error done t)
               (agent-native--write conn '((method . "initialized")))
               (agent-native-codex--call conn method params
                                         (lambda (value error) (setq result value failure error done t))))))
          (let ((end (+ (float-time) 15)))
            (while (and (not done) (< (float-time) end) (agent-native-live-p conn))
              (accept-process-output (agent-native-conn-process conn) 0.1)))
          (cond (failure (error "Codex: %s" failure))
                ((not done) (error "Codex did not answer"))
                (t result)))
      (when (agent-native-live-p conn)
        (delete-process (agent-native-conn-process conn))))))

(cl-defmethod agent-native-backend-sessions ((_ (eql 'codex)) directory)
  (mapcar (lambda (thread)
            (list :id (alist-get 'id thread)
                  :time (seconds-to-time (or (alist-get 'updatedAt thread) (alist-get 'createdAt thread) 0))
                  :title (alist-get 'name thread)
                  :prompt (or (alist-get 'preview thread) "")))
          (alist-get 'data (agent-native-codex--ask
                            directory "thread/list"
                            `((cwd . ,(directory-file-name (expand-file-name directory)))
                              (limit . 100) (sortKey . "updated_at"))))))

(provide 'agent-native-codex)
;;; agent-native-codex.el ends here
