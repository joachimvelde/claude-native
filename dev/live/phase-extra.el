;;; phase-extra.el --- commands mid-turn and in odd orders, both backends -*- lexical-binding: t -*-

(load (expand-file-name "harness.el" (file-name-directory load-file-name)))

(defvar ck-claude nil)
(defvar ck-codex nil)
(defvar ck-quit-once nil "Make the next permission question C-g'd.")
(defvar ck-thread nil)

;; C-g on the question, once.
(let ((answer agent-native-permission-function))
  (setq agent-native-permission-function
        (lambda (request)
          (if ck-quit-once
              (progn (setq ck-quit-once nil) (ck-say "  C-g on %s" (plist-get request :name)) (signal 'quit nil))
            (funcall answer request)))))

(defun ck-both (name timeout action done)
  "Steps NAME for Claude, then Codex: ACTION and DONE run with `ck-buffer' set."
  (list (list (concat name " (claude)") timeout (lambda () (setq ck-buffer ck-claude) (funcall action))
              (lambda () (let ((ck-buffer ck-claude)) (funcall done))))
        (list (concat name " (codex)") timeout (lambda () (setq ck-buffer ck-codex) (funcall action))
              (lambda () (let ((ck-buffer ck-codex)) (funcall done))))))

(setq
 ck-steps
 (append
  (list
   (list "open both" 90
         (lambda ()
           (agent-native-claude) (setq ck-claude (ck-session-buffer))
           (agent-native-codex) (setq ck-codex (ck-session-buffer)))
         (lambda () (and (ck-local 'agent-native--commands ck-claude) (ck-local 'agent-native--models ck-codex)
                         (agent-native-codex-ready (ck-local 'agent-native--conn ck-codex))))))
  ;; Two prompts, one right after the other, while idle.
  (ck-both "two prompts at once" 120
           (lambda () (ck-send "Reply with exactly: FIRST") (ck-send "Reply with exactly: SECOND"))
           (lambda () (and (not (ck-busy)) (ck-has "SECOND\n") (ck-has "^⏺ .*SECOND"))))
  (ck-both "check two prompts" 5
           (lambda ()
             (ck-check (format "%s: FIRST drawn once" (buffer-name ck-buffer)) (= 1 (ck-count "^❯ Reply with exactly: FIRST")))
             (ck-check (format "%s: SECOND drawn once" (buffer-name ck-buffer)) (= 1 (ck-count "^❯ Reply with exactly: SECOND")))
             (ck-check "nothing queued" (null (ck-local 'agent-native--queued))))
           (lambda () t))
  ;; TAB takes Claude's suggestion.
  (list
   (list "TAB takes the suggestion" 30
         (lambda () (setq ck-buffer ck-claude))
         (lambda () (or (ck-local 'agent-native--suggestion) t)))
   (list "check TAB" 5
         (lambda ()
           (let ((suggestion (ck-local 'agent-native--suggestion)))
             (ck-say "  suggestion: %S" suggestion)
             (when suggestion
               (with-current-buffer ck-claude (agent-native-tab))
               (ck-check "TAB put it in the input" (equal (with-current-buffer ck-claude (agent-native--input-text)) suggestion))
               (with-current-buffer ck-claude (agent-native--set-input "")))))
         (lambda () t)))
  ;; A permission question C-g'd, then answered with C-c C-p.
  (ck-both "C-g on the question" 90
           (lambda ()
             (setq ck-quit-once t ck-permission 'allow ck-asked nil)
             (with-current-buffer ck-buffer (setq agent-native--mode 'default))
             (with-current-buffer ck-buffer (agent-native-backend-set-mode agent-native--conn 'default))
             (ck-send "Run `mkdir qdir` in the shell, then reply with exactly: MADE"))
           (lambda () (ck-local 'agent-native--pending)))
  (ck-both "C-c C-p answers it" 90
           (lambda ()
             (ck-check "status asks for C-c C-p" (string-match-p "C-c C-p" (ck-status)))
             (ck-check "waiting note shown" (ck-has "Waiting for permission"))
             (with-current-buffer ck-buffer (agent-native-answer-pending)))
           (lambda () (ck-turn-done)))
  (ck-both "check C-c C-p" 5
           (lambda ()
             (ck-check "MADE" (ck-has "^⏺ MADE"))
             (ck-check "no waiting note left" (not (ck-has "Waiting for permission")))
             (ignore-errors (delete-directory (expand-file-name "qdir" ck-work))))
           (lambda () t))
  ;; Interrupt while a question waits.
  (ck-both "question, then interrupt" 90
           (lambda ()
             (setq ck-quit-once t)
             (ck-send "Run `mkdir idir` in the shell, then reply with exactly: MADE2"))
           (lambda () (ck-local 'agent-native--pending)))
  (ck-both "interrupt" 60
           (lambda () (with-current-buffer ck-buffer (agent-native-interrupt)))
           (lambda () (and (not (ck-busy)) (null (ck-local 'agent-native--pending)))))
  (ck-both "check interrupt with a question" 5
           (lambda ()
             (ck-check "no waiting note left" (not (ck-has "Waiting for permission")))
             (ck-check "idir not made" (not (file-exists-p (expand-file-name "idir" ck-work))))
             (ck-check "status clear" (not (string-match-p "C-c C-p" (ck-status)))))
           (lambda () t))
  ;; Model, effort and mode changed while a turn runs.
  (ck-both "change settings mid-turn" 120
           (lambda ()
             (setq ck-permission 'allow)
             (ck-send "Run `sleep 4` in the shell, then reply with exactly: SLEPT"))
           (lambda () (ck-has "(sleep 4)")))
  (ck-both "pick model and effort, cycle mode" 120
           (lambda ()
             (with-current-buffer ck-buffer
               (agent-native-set-model-and-effort (if (eq agent-native--backend 'claude) "haiku" "gpt-6-luna") "high")
               (agent-native-cycle-mode)))
           (lambda () (ck-turn-done)))
  (ck-both "next turn uses them" 120
           (lambda ()
             (ck-check "mode accept-edits" (eq (ck-local 'agent-native--mode) 'accept-edits))
             (ck-send "Reply with exactly: NEXT"))
           (lambda () (and (ck-turn-done) (string-match-p "high" (ck-label)))))
  (ck-both "check settings" 5
           (lambda () (ck-say "  %s label: %s" (buffer-name ck-buffer) (ck-label))
             (ck-check "no error" (not (ck-has "^Error"))))
           (lambda () t))
  ;; The agents list while a sub-agent works.
  (ck-both "sub-agent and the list" 240
           (lambda ()
             (ck-send (if (eq ck-buffer ck-claude)
                          "Use the Agent tool (subagent_type general-purpose) to run `sleep 5` and then `ls`, and report the files. Then reply with the list."
                        "Spawn exactly one sub-agent to run `sleep 5` and then `ls` in this directory and report the file names. Wait for it, then reply with the list.")))
           (lambda () (ck-local 'agent-native--agents)))
  (ck-both "open the list" 10
           (lambda ()
             (with-current-buffer ck-buffer (agent-native-agents))
             (let ((list (get-buffer (format "*%s agents*" (if (eq ck-buffer ck-claude) "claude" "codex")))))
               (ck-check "list shows" (and list (get-buffer-window list)))
               (when list
                 (ck-say "  list:\n%s" (with-current-buffer list (buffer-string)))
                 (with-current-buffer list
                   (goto-char (point-min))
                   (when (text-property-search-forward 'agent-native-agent)
                     (backward-char)
                     (condition-case err (agent-native-agents-visit)
                       (error (ck-say "  visit: %S" err)))
                     (ck-check "visit went to the session" (eq (ck-session-buffer) ck-buffer))))
                 (quit-windows-on list t))))
           (lambda () t))
  (ck-both "sub-agent done" 240
           (lambda () nil)
           (lambda () (ck-turn-done)))
  ;; /clear while a turn runs.
  (ck-both "/clear mid-turn" 90
           (lambda () (ck-send "Run `sleep 8` in the shell, then reply with exactly: NO"))
           (lambda () (ck-has "(sleep 8)")))
  (ck-both "clear now" 60
           (lambda () (ck-send "/clear"))
           (lambda () (and (not (ck-busy)) (not (ck-has "sleep 8"))
                           (if (eq ck-buffer ck-codex)
                               (agent-native-codex-ready (ck-local 'agent-native--conn))
                             (ck-local 'agent-native--commands)))))
  (ck-both "a turn after clear" 90
           (lambda () (ck-send "Reply with exactly: FRESH"))
           (lambda () (ck-turn-done)))
  (ck-both "check after clear" 5
           (lambda ()
             (ck-check "FRESH" (ck-has "^⏺ FRESH"))
             (ck-check "/clear kept effort and mode" (and (string-match-p "high" (ck-label))
                                                          (eq (ck-local 'agent-native--mode) 'accept-edits)))
             (ck-check "no NO" (not (ck-has "^⏺ NO")))
             (ck-check "no error" (not (ck-has "^Error"))))
           (lambda () t))
  ;; /resume while a turn runs: the earlier session.
  (ck-both "/resume mid-turn" 90
           (lambda () (ck-send "Run `sleep 8` in the shell, then reply with exactly: NO2"))
           (lambda () (ck-has "(sleep 8)")))
  (ck-both "resume now" 120
           (lambda ()
             ;; Both agents have a session that began so: the picker numbers
             ;; them, newest first. Pick this agent's.
             (let* ((name "Reply with exactly: FIRST")
                    (backend (ck-local 'agent-native--backend))
                    (same (seq-filter (lambda (s) (equal (plist-get s :prompt) name))
                                      (agent-native--past-sessions ck-work)))
                    (n (1+ (seq-position same (seq-find (lambda (s) (eq (plist-get s :backend) backend)) same))))
                    (answer (if (= n 1) name (format "%s <%d>" name n))))
               (setq ck-answers (list answer))
               (with-current-buffer ck-buffer
                 (condition-case err (call-interactively #'agent-native-resume)
                   (error (ck-say "  resume: %S" err))))))
           (lambda () (and (ck-has "↑ resumed") (not (ck-busy))
                           (if (eq ck-buffer ck-codex)
                               (agent-native-codex-ready (ck-local 'agent-native--conn))
                             (ck-local 'agent-native--commands)))))
  (ck-both "check resume" 90
           (lambda ()
             (ck-check "the FIRST session" (ck-has "^❯ Reply with exactly: FIRST"))
             (ck-check "no NO2" (not (ck-has "NO2")))
             (ck-send "Reply with exactly: BACK-AGAIN"))
           (lambda () (ck-turn-done)))
  ;; Kill the buffer while a question waits.
  (ck-both "kill the buffer mid-question" 90
           (lambda ()
             (setq ck-quit-once t)
             (ck-send "Run `mkdir kdir` in the shell, then reply with exactly: K"))
           (lambda () (ck-local 'agent-native--pending)))
  (ck-both "kill now" 15
           (lambda () (kill-buffer ck-buffer))
           (lambda () (not (buffer-live-p ck-buffer))))))

(ck-start)
