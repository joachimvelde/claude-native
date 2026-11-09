;;; claude-native-process.el --- talk to claude's stream-json mode -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Claude Code runs headless (`claude -p' with stream-json in and out): one
;; JSON object per line each way. This file owns that pipe: it starts the
;; process, splits its output into messages, and sends user messages and
;; control requests (interrupt, permission mode, model) with callbacks for
;; their responses. It knows nothing about buffers or drawing.

;;; Code:

(require 'json)
(require 'cl-lib)

(defgroup claude-native nil
  "Claude Code, drawn natively in Emacs."
  :group 'tools
  :prefix "claude-native-")

(defcustom claude-native-program
  (or (executable-find "claude")
      ;; The installer's place; an Emacs started from the Dock may not
      ;; have it on PATH.
      (let ((local (expand-file-name "~/.local/bin/claude")))
        (and (file-executable-p local) local))
      "claude")
  "The claude executable."
  :type 'string)

(defcustom claude-native-extra-args nil
  "More arguments for claude, e.g. (\"--model\" \"opus\")."
  :type '(repeat string))

(cl-defstruct (claude-native-conn (:constructor claude-native-conn--make))
  "A running claude process."
  process
  ;; Output not yet ended by a newline.
  (pending "")
  ;; Called with each message (an alist) claude writes.
  on-message
  ;; Called with the exit description when the process ends.
  on-exit
  ;; request_id -> callback for control requests we sent.
  (callbacks (make-hash-table :test #'equal))
  (next-id 0))

(defun claude-native--parse (line)
  "Parse one JSON LINE into an alist, or nil if it is not JSON.
Arrays become vectors, so a message can be sent back as it came:
`json-serialize' reads a list as an object."
  (condition-case nil
      (json-parse-string line :object-type 'alist :array-type 'array
                         :null-object nil :false-object :false)
    (json-error nil)))

(defun claude-native--filter (conn output)
  "Split OUTPUT into lines and hand each message of CONN on."
  (let* ((text (concat (claude-native-conn-pending conn) output))
         (lines (split-string text "\n")))
    ;; The last piece has no newline yet: keep it for the next chunk.
    (setf (claude-native-conn-pending conn) (car (last lines)))
    (dolist (line (butlast lines))
      (when-let* ((msg (and (not (string-empty-p line)) (claude-native--parse line))))
        (claude-native--dispatch conn msg)))))

(defun claude-native--dispatch (conn msg)
  "Answer callbacks for control responses; pass every message of CONN on."
  (when (equal (alist-get 'type msg) "control_response")
    (let* ((response (alist-get 'response msg))
           (id (alist-get 'request_id response))
           (callback (gethash id (claude-native-conn-callbacks conn))))
      (when callback
        (remhash id (claude-native-conn-callbacks conn))
        (funcall callback response))))
  (when (claude-native-conn-on-message conn)
    (funcall (claude-native-conn-on-message conn) msg)))

(cl-defun claude-native-start (&key directory args on-message on-exit)
  "Start claude in DIRECTORY with extra ARGS; return the connection.
ON-MESSAGE gets each message; ON-EXIT the exit description."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (conn (claude-native-conn--make :on-message on-message :on-exit on-exit))
         (command `(,claude-native-program
                    "-p" "--input-format" "stream-json" "--output-format" "stream-json"
                    "--verbose" "--include-partial-messages"
                    ;; Echo each prompt when claude takes it up, so a prompt
                    ;; sent while it works is drawn when its turn starts.
                    "--replay-user-messages"
                    ;; A predicted next prompt after each turn, shown dim
                    ;; in the empty input.
                    "--prompt-suggestions" "true"
                    "--permission-prompt-tool" "stdio"
                    ,@claude-native-extra-args ,@args))
         (process (make-process
                   :name "claude-native"
                   :command command
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :noquery t
                   :stderr (get-buffer-create " *claude-native stderr*")
                   :filter (lambda (_ output) (claude-native--filter conn output))
                   :sentinel (lambda (process event)
                               (unless (process-live-p process)
                                 (when (claude-native-conn-on-exit conn)
                                   (funcall (claude-native-conn-on-exit conn) (string-trim event))))))))
    (setf (claude-native-conn-process conn) process)
    conn))

(defun claude-native-live-p (conn)
  "Whether CONN's process still runs."
  (and conn (process-live-p (claude-native-conn-process conn))))

(defun claude-native--send (conn object)
  "Write OBJECT (an alist) to CONN as one JSON line."
  (when (claude-native-live-p conn)
    (process-send-string (claude-native-conn-process conn)
                         (concat (json-serialize object :null-object nil :false-object :false) "\n"))))

(defun claude-native-send-user (conn content)
  "Send a user message with CONTENT: a string, or a vector of blocks."
  (claude-native--send conn `((type . "user")
                              (message . ((role . "user") (content . ,content))))))

(defun claude-native-control (conn subtype &optional fields callback)
  "Send control request SUBTYPE with FIELDS (an alist); CALLBACK gets the response."
  (let ((id (format "emacs_%d" (cl-incf (claude-native-conn-next-id conn)))))
    (when callback
      (puthash id callback (claude-native-conn-callbacks conn)))
    (claude-native--send conn `((type . "control_request")
                                (request_id . ,id)
                                (request . ((subtype . ,subtype) ,@fields))))))

(defun claude-native-respond (conn request-id response)
  "Answer claude's control request REQUEST-ID with RESPONSE (an alist)."
  (claude-native--send conn `((type . "control_response")
                              (response . ((subtype . "success")
                                           (request_id . ,request-id)
                                           (response . ,response))))))

(defun claude-native-stop (conn)
  "End CONN's process."
  (when (claude-native-live-p conn)
    (delete-process (claude-native-conn-process conn))))

(provide 'claude-native-process)
;;; claude-native-process.el ends here
