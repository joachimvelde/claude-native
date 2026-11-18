;;; agent-native-backend.el --- what a backend is: the pipe, the operations, the events -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; A backend runs one agent harness (Claude Code, Codex) as a process that
;; speaks JSON lines, and is the only code that knows that harness's
;; protocol. The session buffer (agent-native.el) knows only this file.
;;
;; Two directions:
;;
;; - Operations, buffer -> backend: the generic functions below. A backend
;;   is a symbol (`claude', `codex') for what needs no process (starting,
;;   past sessions, modes), else a connection, a struct that includes
;;   `agent-native-conn'.
;;
;; - Events, backend -> buffer: `agent-native--emit' with a type and a
;;   plist. The buffer draws them the same whatever the backend:
;;
;;   session        :id :model :effort :mode :models :commands :hints
;;                  (any of them; a key present with nil means "none")
;;   prompt         :text          the agent took up a prompt of yours
;;   working        :status        busy, saying STATUS ("Thinking…")
;;   thinking-tokens :count
;;   text-start, text-delta :text, text-end, text :text   the agent's text
;;   tool           :id :name :input       a tool call
;;   tool-result    :id :content :error    its result
;;   agent-step     :tool-id :name :input  a sub-agent's call, under TOOL-ID
;;   agent-start    :id :tool-id :name :type :model :effort
;;   agent-progress :id :step :tools :tokens
;;   agent-model    :tool-id :model
;;   agent-end      :id
;;   request        :id :tool-id :name :input :questions  asks you something;
;;                  :always says what answering `always' would keep
;;   request-done   :id            it no longer waits for an answer
;;   interrupted                   a turn was interrupted
;;   note           :text :face    a line in the transcript
;;   turn-end       :duration-ms :error :text :cost
;;   suggestion     :text          a guess at your next prompt
;;   remote         :state :url :message   Remote Control
;;   history-start, history-end    events between are a resumed session
;;
;; A new feature is an event or an operation here, drawn or called once in
;; agent-native.el, and implemented in each backend file.

;;; Code:

(require 'json)
(require 'cl-lib)

(defgroup agent-native nil
  "Coding agents (Claude Code, Codex), drawn natively in Emacs."
  :group 'tools
  :prefix "agent-native-")

;;;; Connections and their pipe

(cl-defstruct (agent-native-conn (:constructor nil) (:copier nil))
  "A running agent process. Backends include this in their own struct."
  backend
  process
  ;; Output not yet ended by a newline: chunks, newest first.
  (pending nil)
  ;; Called with (TYPE PLIST) for each event.
  on-event
  ;; Called with the exit description when the process ends.
  on-exit
  ;; Request id -> callback for requests we sent.
  (callbacks (make-hash-table :test #'equal))
  (next-id 0))

(defun agent-native--emit (conn type &rest props)
  "Tell CONN's session about event TYPE with PROPS (see the Commentary)."
  (when-let* ((on-event (agent-native-conn-on-event conn)))
    (funcall on-event type props)))

(defun agent-native--parse (line)
  "Parse one JSON LINE into an alist, or nil if it is not JSON.
Arrays become vectors, so a message can be sent back as it came:
`json-serialize' reads a list as an object."
  (condition-case nil
      (json-parse-string line :object-type 'alist :array-type 'array
                         :null-object nil :false-object :false)
    (json-error nil)))

(defun agent-native--filter (conn output)
  "Split OUTPUT into lines and hand each message to CONN's backend."
  (if (not (string-search "\n" output))
      ;; Part of a long line (a big tool result): joined once it ends, so
      ;; a line of N chunks costs N, not N².
      (push output (agent-native-conn-pending conn))
    (let* ((text (apply #'concat (nreverse (cons output (agent-native-conn-pending conn)))))
           (lines (split-string text "\n")))
      ;; The last piece has no newline yet: keep it for the next chunk.
      (setf (agent-native-conn-pending conn) (list (car (last lines))))
      (dolist (line (butlast lines))
        (when-let* ((msg (and (not (string-empty-p line)) (agent-native--parse line))))
          ;; One message that fails to draw must not lose those after it
          ;; (a lost turn end would leave the spinner running).
          (condition-case-unless-debug err
              (agent-native-backend-receive conn msg)
            (error (message "agent-native: %s on a %s message" (error-message-string err)
                            (or (alist-get 'type msg) (alist-get 'method msg))))))))))

(defun agent-native--spawn (conn name command)
  "Run COMMAND for CONN in `default-directory', as process NAME."
  (setf (agent-native-conn-process conn)
        (make-process
         :name name
         :command command
         :connection-type 'pipe
         :coding 'utf-8-unix
         :noquery t
         :stderr (get-buffer-create (format " *%s stderr*" name))
         :filter (lambda (_ output) (agent-native--filter conn output))
         :sentinel (lambda (process event)
                     (unless (process-live-p process)
                       (when (agent-native-conn-on-exit conn)
                         (funcall (agent-native-conn-on-exit conn) (string-trim event)))))))
  conn)

(defun agent-native-live-p (conn)
  "Whether CONN's process still runs."
  (and conn (process-live-p (agent-native-conn-process conn))))

(defun agent-native--write (conn object)
  "Write OBJECT (an alist) to CONN as one JSON line."
  (when (agent-native-live-p conn)
    (process-send-string (agent-native-conn-process conn)
                         (concat (json-serialize object :null-object nil :false-object :false) "\n"))))

(defun agent-native--next-id (conn)
  "A fresh request number for CONN."
  (cl-incf (agent-native-conn-next-id conn)))

(defun agent-native--expect (conn id callback)
  "Call CALLBACK with the answer to CONN's request ID, when it comes."
  (when callback
    (puthash id callback (agent-native-conn-callbacks conn))))

(defun agent-native--answered (conn id answer)
  "Hand ANSWER to the callback waiting for CONN's request ID, if any."
  (when-let* ((callback (gethash id (agent-native-conn-callbacks conn))))
    (remhash id (agent-native-conn-callbacks conn))
    (funcall callback answer)))

(defun agent-native-stop (conn)
  "End CONN: it is over at once, and ON-EXIT hears \"stopped\".
The process gets SIGTERM and a few seconds to leave: killed outright, an
agent cannot sign off (a Claude session under Remote Control would stay
listed on claude.ai)."
  (when (agent-native-live-p conn)
    (let ((process (agent-native-conn-process conn))
          (on-exit (agent-native-conn-on-exit conn)))
      ;; Detach first: nothing the process says while leaving is passed on.
      (setf (agent-native-conn-process conn) nil
            (agent-native-conn-on-event conn) nil
            (agent-native-conn-on-exit conn) nil)
      (clrhash (agent-native-conn-callbacks conn))
      (signal-process process 'SIGTERM)
      (run-at-time 5 nil (lambda () (when (process-live-p process) (delete-process process))))
      (when on-exit (funcall on-exit "stopped")))))

;;;; Backends, without a process

(defcustom agent-native-backends '(claude codex)
  "The backends there are, in the order pickers list them."
  :type '(repeat symbol))

(cl-defgeneric agent-native-backend-name (backend)
  "BACKEND's short name, as in \"Codex finished\" (a string).")

(cl-defgeneric agent-native-backend-title (backend)
  "BACKEND's full name, for the welcome line."
  (agent-native-backend-name backend))

(cl-defgeneric agent-native-backend-provider (backend)
  "Who makes BACKEND's models, as the model picker names it."
  (agent-native-backend-name backend))

(cl-defgeneric agent-native-backend-available-p (backend)
  "Whether BACKEND's program is installed.")

(cl-defgeneric agent-native-backend-start (backend directory &rest keys)
  "Start BACKEND in DIRECTORY and return its connection.
KEYS: :resume, a past session's id; :mode, the permission mode to start
in; :on-event, called with (TYPE PLIST) for each event; :on-exit, called
with the exit description.")

(cl-defgeneric agent-native-backend-sessions (backend directory)
  "BACKEND's past sessions in DIRECTORY, newest first.
Each a plist (:id :time :title :prompt); :title is nil if never named.")

(cl-defgeneric agent-native-backend-modes (backend)
  "BACKEND's permission modes in the order S-TAB goes through them: an
alist of (MODE . LABEL), MODE one of `default', `accept-edits', `plan',
`bypass', LABEL what the status line says (\"\" for none).")

;;;; Operations on a running session

(cl-defgeneric agent-native-backend-receive (conn msg)
  "Turn MSG, a message from CONN's process, into events.")

(cl-defgeneric agent-native-backend-send (conn text attachments)
  "Send prompt TEXT, with ATTACHMENTS: (KIND FILE MEDIA-TYPE) lists, KIND
`image' or `pdf', in the order their marks appear in TEXT.")

(cl-defgeneric agent-native-backend-interrupt (conn)
  "Stop the turn CONN's agent is working on.")

(cl-defgeneric agent-native-backend-set-mode (conn mode)
  "Use permission MODE (see `agent-native-backend-modes') from now on.")

(cl-defgeneric agent-native-backend-set-model (conn model effort)
  "Use MODEL from now on, at EFFORT (nil: the model's own default).")

(cl-defgeneric agent-native-backend-set-effort (conn effort)
  "Use EFFORT from now on.")

(cl-defgeneric agent-native-backend-answer (conn request answer)
  "Answer REQUEST (a `request' event's plist) with ANSWER: `allow',
`always', `deny', or (answers . ((ID . LABEL) ...)) for :questions.")

(cl-defgeneric agent-native-backend-usage (conn callback)
  "Call CALLBACK with a usage report for CONN's session, or with a string
saying why there is none. A report is a plist:
 :rows      ((TITLE PERCENT DETAIL) ...), drawn as bars
 :session   a line on what the session used
 :sections  ((HEADING LINE ...) ...), drawn after, dim")

(cl-defgeneric agent-native-backend-remote-control (conn on)
  "Turn Remote Control ON or off for CONN's session."
  (ignore on)
  (user-error "Remote Control is not available with %s"
              (agent-native-backend-name (agent-native-conn-backend conn))))

;;;; Helpers for backends

(defun agent-native--short-count (count)
  "COUNT (of tokens) in short: 950, 23k, 1.2M, 4.8B."
  (cond ((>= count 1000000000) (format "%.3gB" (/ count 1e9)))
        ((>= count 1000000) (format "%.3gM" (/ count 1e6)))
        ((>= count 1000) (format "%dk" (round count 1000)))
        (t (number-to-string count))))

(defun agent-native--read-lines (file)
  "FILE's JSON lines as alists (arrays as vectors), skipping bad lines."
  (with-temp-buffer
    (insert-file-contents file)
    (let (records)
      (dolist (line (split-string (buffer-string) "\n" t))
        (when-let* ((record (agent-native--parse line)))
          (push record records)))
      (nreverse records))))

(provide 'agent-native-backend)
;;; agent-native-backend.el ends here
