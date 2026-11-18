;;; phase-cross.el --- both backends together, commands in different orders -*- lexical-binding: t -*-

(load (expand-file-name "harness.el" (file-name-directory load-file-name)))

(defvar ck-claude nil)
(defvar ck-codex nil)
(defvar ck-codex-thread nil)
(defvar ck-plain nil "A buffer that is no session, in the work directory.")

(defun ck-from-plain (command &rest answers)
  "Run COMMAND from a plain buffer in the work directory."
  (setq ck-answers answers)
  (switch-to-buffer ck-plain)
  (condition-case err (progn (call-interactively command) nil)
    (user-error (ck-say "  USER-ERROR %s" (error-message-string err)) err)))

(setq
 ck-steps
 (list
  (list "agent-native with no session: the default backend" 60
        (lambda ()
          (setq agent-native-backend 'claude)
          (ck-from-plain #'agent-native)
          (setq ck-claude (ck-session-buffer) ck-buffer ck-claude)
          (ck-check "a Claude buffer" (eq (ck-local 'agent-native--backend) 'claude)))
        (lambda () t))
  (list "choose OpenAI before Claude is ready" 90
        (lambda ()
          (setq ck-answers (list "OpenAI" "gpt-6-luna" "low"))
          (with-current-buffer ck-claude (call-interactively #'agent-native-choose-model))
          (setq ck-codex (ck-session-buffer) ck-buffer ck-codex)
          (ck-check "now in the Codex buffer" (eq (ck-local 'agent-native--backend) 'codex))
          (ck-check "label luna low" (string-match-p "gpt-6-luna · ◐ low" (ck-label)))
          (ck-send "Reply with exactly: CODEX-ONE"))
        (lambda () (ck-turn-done)))
  (list "agent-native goes to the last used: Codex" 10
        (lambda ()
          (setq ck-codex-thread (ck-local 'agent-native--session-id))
          (ck-check "luna low after the turn" (string-match-p "gpt-6-luna · ◐ low" (ck-label)))
          (ck-from-plain #'agent-native)
          (ck-check "went to Codex" (eq (ck-session-buffer) ck-codex)))
        (lambda () t))
  (list "choose Claude from Codex" 120
        (lambda ()
          (setq ck-answers (list "Claude" "haiku" "low"))
          (with-current-buffer ck-codex (call-interactively #'agent-native-choose-model))
          (ck-check "now in the Claude buffer" (eq (ck-session-buffer) ck-claude))
          (setq ck-buffer ck-claude)
          (ck-send "Reply with exactly: CLAUDE-ONE"))
        (lambda () (and (ck-turn-done) (string-match-p "haiku.*low" (ck-label)))))
  (list "a Codex model name typed in Claude" 60
        (lambda ()
          (ck-send "/model gpt-6-luna"))
        (lambda () t))
  (list "after the wrong model" 60
        (lambda () (ck-say "  label after /model gpt-6-luna in Claude: %s" (ck-label))
          (ck-send "Reply with exactly: STILL-CLAUDE"))
        (lambda () (ck-turn-done)))
  (list "resume the Codex session from Claude's buffer" 120
        (lambda ()
          (ck-dump "claude after wrong model")
          (with-current-buffer ck-claude
            (setq ck-answers (list "Reply with exactly: CODEX-ONE"))
            (condition-case err (call-interactively #'agent-native-resume)
              (error (ck-say "  ERROR %S" err))))
          (ck-check "went to the Codex buffer" (eq (ck-session-buffer) ck-codex))
          (setq ck-buffer ck-codex))
        (lambda () (and (ck-has "↑ resumed") (agent-native-codex-ready (ck-local 'agent-native--conn)))))
  (list "check resumed Codex" 5
        (lambda ()
          (ck-check "same thread" (equal (ck-local 'agent-native--session-id) ck-codex-thread))
          (ck-check "Claude buffer untouched" (string-match-p "CLAUDE-ONE" (ck-text ck-claude)))
          (ck-check "Claude still live" (ck-live ck-claude)))
        (lambda () t))
  (list "new session, backend asked" 60
        (lambda ()
          (setq ck-answers (list "codex"))
          (with-current-buffer ck-claude (agent-native-new t))
          (ck-check "a codex <2>" (string-match-p "\\*codex: .*<2>" (buffer-name (ck-session-buffer))))
          (with-current-buffer (ck-session-buffer) (agent-native-exit)))
        (lambda () t))
  (list "Codex dies" 30
        (lambda ()
          (signal-process (process-id (agent-native-conn-process (ck-local 'agent-native--conn ck-codex))) 'SIGKILL))
        (lambda () (and (not (ck-live ck-codex)) (ck-has "exited" ck-codex))))
  (list "send to the dead Codex, then choose OpenAI from Claude" 90
        (lambda ()
          (setq ck-buffer ck-codex)
          (ck-send "Should stay")
          (setq ck-answers (list "OpenAI" "gpt-6-luna" "medium"))
          (with-current-buffer ck-claude (call-interactively #'agent-native-choose-model))
          (ck-check "in the Codex buffer" (eq (ck-session-buffer) ck-codex))
          (ck-check "draft kept" (equal (with-current-buffer ck-codex (agent-native--input-text)) "Should stay")))
        (lambda () (and (ck-has "↑ resumed") (agent-native-codex-ready (ck-local 'agent-native--conn)))))
  (list "a turn after the restart" 120
        (lambda ()
          (ck-check "same thread after restart" (equal (ck-local 'agent-native--session-id) ck-codex-thread))
          (ck-say "  label: %s" (ck-label))
          (with-current-buffer ck-codex (agent-native--set-input ""))
          (ck-send "Reply with exactly: CODEX-AGAIN"))
        (lambda () (ck-turn-done)))
  (list "check model after restart" 5
        (lambda () (ck-check "luna medium" (string-match-p "gpt-6-luna · ◑ medium\\|gpt-6-luna.*medium" (ck-label))))
        (lambda () t))
  (list "Claude dies" 30
        (lambda ()
          (signal-process (process-id (agent-native-conn-process (ck-local 'agent-native--conn ck-claude))) 'SIGKILL))
        (lambda () (and (not (ck-live ck-claude)) (ck-has "exited" ck-claude))))
  (list "agent-native-continue from a plain buffer" 60
        (lambda ()
          ;; Codex was used last: it is live, so continue just shows it.
          (ck-from-plain #'agent-native-continue)
          (ck-check "went to Codex" (eq (ck-session-buffer) ck-codex))
          (ck-from-plain #'agent-native-claude)
          (ck-check "went to Claude" (eq (ck-session-buffer) ck-claude))
          (setq ck-buffer ck-claude))
        (lambda () (and (ck-has "↑ resumed") (ck-local 'agent-native--commands))))
  (list "dired attach" 120
        (lambda ()
          (let ((dired (dired-noselect ck-work)))
            (switch-to-buffer dired)
            (dired-goto-file (expand-file-name "red.png" ck-work))
            (agent-native-dired-attach)
            (ck-say "  dired attach went to %s, input %S" (buffer-name (ck-session-buffer))
                    (with-current-buffer (ck-session-buffer) (agent-native--input-text)))
            (setq ck-buffer (ck-session-buffer))
            (ck-send "What single color fills this image? Reply with one lowercase word.")))
        (lambda () (ck-turn-done)))
  (list "the command picker" 30
        (lambda ()
          (ck-check "red" (ck-has "^⏺ red"))
          (setq ck-answers (list "usage"))
          (with-current-buffer ck-buffer (call-interactively #'agent-native-command)))
        (lambda () (or (get-buffer "*claude usage*") (get-buffer "*codex usage*"))))
  (list "effort picker in Codex" 120
        (lambda ()
          (ck-usage-text "*claude usage*") (ck-usage-text "*codex usage*")
          (setq ck-buffer ck-codex)
          (ck-run #'agent-native-set-effort "high")
          (ck-send "Reply with exactly: HIGH"))
        (lambda () (ck-turn-done)))
  (list "choose-model from a file buffer" 60
        (lambda ()
          (ck-check "label high" (string-match-p "high" (ck-label ck-codex)))
          (let ((file (progn (write-region "notes\n" nil (expand-file-name "notes.txt" ck-work))
                             (find-file-noselect (expand-file-name "notes.txt" ck-work)))))
            (switch-to-buffer file)
            (setq ck-answers (list "Claude" "haiku" "medium"))
            (call-interactively #'agent-native-choose-model)
            (ck-check "went to Claude" (eq (ck-session-buffer) ck-claude))))
        (lambda () t))))

(setq ck-plain (get-buffer-create "plain"))
(with-current-buffer ck-plain (setq default-directory ck-work))
(ck-red-png (expand-file-name "red.png" ck-work))

(ck-start)
