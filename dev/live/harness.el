;;; harness.el --- live checks of agent-native with real claude and codex -*- lexical-binding: t -*-

;; Live checks against the real `claude' and `codex', signed in: they use
;; your plan (haiku, gpt-6-luna), and leave sessions behind (delete them
;; after). The Codex phase answers "always" to `touch a1.txt' and `touch
;; a2.txt': Codex keeps those rules for good, in ~/.codex/rules/default.rules.
;; Remove them after.
;;
;; A phase file (phase-*.el) sets `ck-steps', each (NAME TIMEOUT ACTION
;; DONE), and loads this: ACTION runs, then DONE is polled until it holds.
;; Run one in a GUI Emacs in the background, in an empty directory:
;;
;;   open -W -g -n -a Emacs --env CK_OUT=/tmp/out --env CK_WORK=/tmp/work \
;;        --args -Q -l dev/live/phase-codex.el
;;
;; $CK_OUT/log.txt says what ran and each CHECK; $CK_OUT/transcripts.txt
;; has the session buffers and *Messages* at the end.

(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(require 'agent-native)
(require 'cl-lib)
(require 'dired)

(defvar ck-out (file-name-as-directory (getenv "CK_OUT")))
(defvar ck-work (file-name-as-directory (getenv "CK_WORK")))
(defvar ck-buffer nil "The session buffer steps act on.")
(defvar ck-turns (make-hash-table :test #'eq) "Buffer -> turns ended.")
(defvar ck-mark 0)
(defvar ck-answers nil "Answers for the next minibuffer questions.")
(defvar ck-choices nil "Keys for the next `read-multiple-choice' questions.")
(defvar ck-permission 'allow "What the permission function answers.")
(defvar ck-asked nil "Permission requests seen: (NAME INPUT).")
(defvar ck-fails 0)
(defvar ck-steps nil)

(defun ck-say (fmt &rest args)
  (write-region (concat (format-time-string "%T ") (apply #'format fmt args) "\n") nil
                (expand-file-name "log.txt" ck-out) t 'silent))

(defun ck-check (what ok)
  "Log check WHAT as passed or failed (OK)."
  (unless ok (cl-incf ck-fails))
  (ck-say "%s %s" (if ok "  CHECK ok  " "  CHECK FAIL") what)
  ok)

(advice-add 'agent-native--turn-ended :after
            (lambda (&rest _) (cl-incf (gethash (current-buffer) ck-turns 0))))

(setq agent-native-permission-function
      (lambda (request)
        (push (list (plist-get request :name) (plist-get request :input)) ck-asked)
        (ck-say "  ASKED %s %S -> %s" (plist-get request :name)
                (let ((s (format "%S" (plist-get request :input)))) (substring s 0 (min 120 (length s))))
                ck-permission)
        ck-permission)
      agent-native-format 'org
      agent-native-codex-model "gpt-6-luna"
      agent-native-claude-model "haiku"
      agent-native-permission-mode 'default
      agent-native-notify t
      completing-read-function
      (lambda (prompt collection &rest _)
        (let ((answer (pop ck-answers)))
          (ck-say "  PICK %s-> %S (of %S)" prompt answer
                  (seq-take (all-completions "" collection) 8))
          (or answer (error "No answer for %s" prompt)))))

(fset 'read-multiple-choice
      (lambda (prompt choices &rest _)
        (let ((key (pop ck-choices)))
          (ck-say "  CHOICE %s -> %c" prompt (or key ??))
          (or (assq key choices) (error "No choice for %s" prompt)))))

(defun ck-text (&optional buffer)
  (with-current-buffer (or buffer ck-buffer)
    (buffer-substring-no-properties (point-min) (or agent-native--input (point-max)))))
(defun ck-has (regexp &optional buffer) (string-match-p regexp (ck-text buffer)))
(defun ck-count (regexp &optional buffer)
  (with-temp-buffer (insert (ck-text buffer)) (how-many regexp (point-min) (point-max))))
(defun ck-label (&optional buffer)
  (with-current-buffer (or buffer ck-buffer)
    (substring-no-properties (or (and agent-native--label-overlay
                                      (overlay-get agent-native--label-overlay 'after-string))
                                 ""))))
(defun ck-status (&optional buffer)
  (with-current-buffer (or buffer ck-buffer) (or (agent-native--status-line) "")))
(defun ck-local (var &optional buffer) (buffer-local-value var (or buffer ck-buffer)))
(defun ck-busy (&optional buffer) (ck-local 'agent-native--busy buffer))
(defun ck-live (&optional buffer) (agent-native-live-p (ck-local 'agent-native--conn buffer)))

(defun ck-type (text)
  "Put TEXT in the input, as typed."
  (with-current-buffer ck-buffer (goto-char (point-max)) (insert text)))
(defun ck-send (text)
  "Type TEXT and press RET; a user-error is logged, not raised."
  (setq ck-mark (gethash ck-buffer ck-turns 0))
  (ck-type text)
  (with-current-buffer ck-buffer
    (condition-case err (agent-native-send)
      (user-error (ck-say "  USER-ERROR %s" (error-message-string err))
                  err))))
(defun ck-turn-done (&optional buffer)
  (let ((buffer (or buffer ck-buffer)))
    (and (> (gethash buffer ck-turns 0) ck-mark) (not (ck-busy buffer)))))
(defun ck-run (command &rest answers)
  "Run COMMAND interactively in `ck-buffer', minibuffer answered by ANSWERS.
A user-error is logged and returned."
  (setq ck-answers answers)
  (with-current-buffer ck-buffer
    (condition-case err (progn (call-interactively command) nil)
      (user-error (ck-say "  USER-ERROR %s" (error-message-string err)) err))))
(defun ck-session-buffer ()
  "The buffer a command left current, as the selected window shows it."
  (window-buffer (selected-window)))

(defun ck-dump (name &optional buffer)
  (let ((buffer (or buffer ck-buffer)))
    (when (buffer-live-p buffer)
      (write-region (format "=== %s [%s]\nlabel: %s\nstatus: %s\nmode: %s model: %s effort: %s session: %s\n%s\n"
                            name (buffer-name buffer) (ck-label buffer) (ck-status buffer)
                            (ck-local 'agent-native--mode buffer) (ck-local 'agent-native--model buffer)
                            (ck-local 'agent-native--effort buffer) (ck-local 'agent-native--session-id buffer)
                            (ck-text buffer))
                    nil (expand-file-name "transcripts.txt" ck-out) t 'silent))))

(defun ck-usage-text (name)
  "The text of usage buffer NAME, which is then closed."
  (when-let* ((buffer (get-buffer name)))
    (prog1 (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer buffer))))

(defun ck-red-png (file)
  "Write FILE, a 32x32 red PNG, to attach."
  (with-temp-file file
    (set-buffer-multibyte nil)
    (insert (base64-decode-string "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAIAAAD8GO2jAAAAKElEQVR4nO3NsQ0AAAzCMP5/un0CNkuZ41wybXsHAAAAAAAAAAAAxR4yw/wuPL6QkAAAAABJRU5ErkJggg=="))))

(defvar ck-current nil)
(defvar ck-deadline nil)
(defvar ck-timer nil)

(defun ck-finish (why)
  (ck-say "FINISH %s, %d failed checks" why ck-fails)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-native-mode)
        (ck-dump "end" buffer)
        (agent-native-stop agent-native--conn))))
  (write-region (format "=== *Messages*\n%s\n" (with-current-buffer "*Messages*" (buffer-string)))
                nil (expand-file-name "transcripts.txt" ck-out) t 'silent)
  (when ck-timer (cancel-timer ck-timer))
  (write-region "done\n" nil (expand-file-name "done" ck-out) nil 'silent)
  (run-at-time 3 nil #'kill-emacs))

(defun ck-tick ()
  (condition-case err
      (cond
       ((and ck-current (funcall (nth 3 ck-current)))
        (ck-say "ok   %s (%.0fs left)" (car ck-current) (- ck-deadline (float-time)))
        (setq ck-current nil))
       ((and ck-current (> (float-time) ck-deadline))
        (ck-say "FAIL %s: timed out" (car ck-current))
        (cl-incf ck-fails)
        (ck-dump (concat "timeout " (car ck-current)))
        (ck-finish "timeout"))
       (ck-current nil)
       ((null ck-steps) (ck-finish "all steps ran"))
       (t (setq ck-current (pop ck-steps)
                ck-deadline (+ (float-time) (nth 1 ck-current)))
          (ck-say "run  %s" (car ck-current))
          (funcall (nth 2 ck-current))))
    (error (ck-say "ERROR in %s: %S" (car ck-current) err)
           (cl-incf ck-fails)
           (ck-dump (format "error in %s" (car ck-current)))
           (ck-finish "error"))))

(defun ck-start ()
  (setq default-directory ck-work)
  (setq ck-timer (run-at-time 1 0.3 #'ck-tick))
  ;; A safety net.
  (run-at-time 2400 nil (lambda () (ck-finish "overall timeout"))))
