;;; phase-codex.el --- every command with Codex, in different orders -*- lexical-binding: t -*-

(load (expand-file-name "harness.el" (file-name-directory load-file-name)))

(defvar ck-thread-1 nil)
(defvar ck-second-model nil)
(defvar ck-buffer-2 nil)

(setq
 ck-steps
 (list
  ;; Usage, a model, and a prompt, all before the thread is open.
  (list "open, usage + model + prompt before ready" 120
        (lambda ()
          (agent-native-codex)
          (setq ck-buffer (ck-session-buffer))
          (ck-check "buffer name" (equal (buffer-name ck-buffer) (format "*codex: %s*" (abbreviate-file-name (directory-file-name ck-work)))))
          (with-current-buffer ck-buffer (agent-native-usage))
          (with-current-buffer ck-buffer (agent-native-set-model-and-effort "gpt-6-luna" "low"))
          (ck-send "Reply with exactly: ONE"))
        (lambda () (and (ck-turn-done) (get-buffer "*codex usage*"))))
  (list "check first turn" 5
        (lambda ()
          (let ((usage (ck-usage-text "*codex usage*")))
            (ck-say "  usage before ready:\n%s" usage)
            (ck-check "usage shows limits" (string-match-p "Current" (or usage ""))))
          (ck-check "prompt drawn once" (= 1 (ck-count "^❯ Reply with exactly: ONE")))
          (ck-check "reply ONE" (ck-has "^⏺ ONE"))
          (ck-check "label luna low" (string-match-p "gpt-6-luna · ◐ low" (ck-label)))
          (setq ck-thread-1 (ck-local 'agent-native--session-id)))
        (lambda () t))
  ;; A second prompt while a turn runs joins it.
  (list "steer a running turn" 120
        (lambda ()
          (setq ck-permission 'allow)
          (ck-send "Run `sleep 6` in the shell, then reply with exactly: DONE-A"))
        (lambda () (ck-has "Shell(sleep 6)")))
  (list "send while busy" 120
        (lambda () (ck-send "Also end your reply with the word DONE-B"))
        (lambda () (and (ck-turn-done) (ck-has "DONE-B"))))
  (list "check steering" 5
        (lambda ()
          (ck-check "first prompt once" (= 1 (ck-count "^❯ Run `sleep 6`")))
          (ck-check "second prompt once" (= 1 (ck-count "^❯ Also end your reply")))
          (ck-check "nothing queued" (null (ck-local 'agent-native--queued)))
          (ck-check "not busy" (not (ck-busy))))
        (lambda () t))
  ;; Interrupt at once, before Codex says the turn started.
  (list "interrupt before the turn starts" 60
        (lambda ()
          (ck-send "Run `sleep 20` in the shell, then reply with exactly: SLEPT")
          (with-current-buffer ck-buffer (agent-native-interrupt)))
        (lambda () (and (not (ck-busy)) (ck-has "Interrupted"))))
  (list "a turn after the interrupt runs" 90
        (lambda ()
          (ck-check "no SLEPT" (not (ck-has "^⏺ SLEPT")))
          (ck-send "Reply with exactly: AFTER-INT"))
        (lambda () (ck-turn-done)))
  (list "check after interrupt" 5
        (lambda ()
          (ck-check "AFTER-INT answered, not interrupted" (ck-has "^⏺ AFTER-INT"))
          (ck-check "one Interrupted" (= 1 (ck-count "Interrupted"))))
        (lambda () t))
  ;; Interrupt mid-tool.
  (list "interrupt a running command" 90
        (lambda () (ck-send "Run `sleep 30` in the shell, then reply with exactly: SLEPT2"))
        (lambda () (ck-has "Shell(sleep 30)")))
  (list "interrupt now" 30
        (lambda () (with-current-buffer ck-buffer (agent-native-interrupt)))
        (lambda () (and (not (ck-busy)) (= 2 (ck-count "Interrupted")))))
  ;; Permissions: no, then always.
  (list "deny a command" 120
        (lambda ()
          (setq ck-permission 'deny ck-asked nil)
          (ck-send "Run `touch denied.txt` in the shell. If it is refused, reply with exactly: REFUSED"))
        (lambda () (and (not (ck-busy)) ck-asked (ck-has "Denied"))))
  (list "check deny" 5
        (lambda ()
          (ck-check "denied.txt not made" (not (file-exists-p (expand-file-name "denied.txt" ck-work))))
          (ck-check "no permission left pending" (null (ck-local 'agent-native--pending))))
        (lambda () t))
  (list "always allow" 150
        (lambda ()
          (setq ck-permission 'always ck-asked nil)
          (ck-send "Run `touch a1.txt` in the shell. Then, as a separate shell command, run `touch a2.txt`. Then reply with exactly: TOUCHED"))
        (lambda () (ck-turn-done)))
  (list "check always" 5
        (lambda ()
          (ck-say "  asked %d times: %S" (length ck-asked) (mapcar #'car ck-asked))
          (ck-check "a1 and a2 made" (and (file-exists-p (expand-file-name "a1.txt" ck-work))
                                          (file-exists-p (expand-file-name "a2.txt" ck-work))))
          (setq ck-permission 'allow))
        (lambda () t))
  ;; Modes.
  (list "cycle to auto, write without asking" 150
        (lambda ()
          (with-current-buffer ck-buffer (agent-native-cycle-mode))
          (ck-check "mode accept-edits" (eq (ck-local 'agent-native--mode) 'accept-edits))
          (ck-check "status says auto" (string-match-p "auto" (ck-status)))
          (setq ck-asked nil)
          (ck-send "Use apply_patch to create b.txt containing the line hi. Then reply with exactly: WROTE"))
        (lambda () (ck-turn-done)))
  (list "check auto" 5
        (lambda ()
          (ck-check "b.txt made" (file-exists-p (expand-file-name "b.txt" ck-work)))
          (ck-check "not asked in auto" (null ck-asked))
          (ck-check "Write drawn" (ck-has "Write(b.txt)\\|Edit(b.txt)")))
        (lambda () t))
  (list "plan mode" 240
        (lambda ()
          (with-current-buffer ck-buffer (agent-native-cycle-mode))
          (ck-check "mode plan" (eq (ck-local 'agent-native--mode) 'plan))
          (ck-send "Plan how to add a README to this directory, in two short steps. Do not change any files."))
        (lambda () (ck-turn-done)))
  (list "full access, then default" 150
        (lambda ()
          (ck-check "no README written in plan mode" (not (file-exists-p (expand-file-name "README.md" ck-work))))
          (with-current-buffer ck-buffer (agent-native-cycle-mode))
          (ck-check "mode bypass" (eq (ck-local 'agent-native--mode) 'bypass))
          (with-current-buffer ck-buffer (agent-native-cycle-mode))
          (ck-check "mode default again" (eq (ck-local 'agent-native--mode) 'default))
          (setq ck-asked nil)
          (ck-send "Run `echo back-to-default` in the shell, then reply with exactly: BACK"))
        (lambda () (ck-turn-done)))
  (list "check back in default" 5
        (lambda ()
          (ck-check "asked again in default" ck-asked)
          (ck-check "BACK" (ck-has "^⏺ BACK")))
        (lambda () t))
  ;; Slash commands typed.
  (list "/effort high typed" 120
        (lambda ()
          (ck-send "/effort high")
          (ck-check "label shows high at once" (string-match-p "high" (ck-label)))
          (ck-send "Reply with exactly: EFFORT"))
        (lambda () (ck-turn-done)))
  (list "/model NAME typed" 120
        (lambda ()
          (ck-check "still high after the turn" (string-match-p "◕ high\\|high" (ck-label)))
          (setq ck-second-model (seq-find (lambda (m) (not (equal m "gpt-6-luna")))
                                          (mapcar (lambda (m) (plist-get m :value)) (ck-local 'agent-native--models))))
          (ck-say "  second model: %s" ck-second-model)
          (ck-send (format "/model %s" ck-second-model))
          (ck-check "label shows it" (string-match-p (regexp-quote ck-second-model) (ck-label)))
          (ck-send "/model gpt-6-luna")
          (ck-send "Reply with exactly: MODEL"))
        (lambda () (ck-turn-done)))
  (list "/usage, /tasks, /rc, unknown command" 30
        (lambda ()
          (ck-check "label luna" (string-match-p "gpt-6-luna" (ck-label)))
          (ck-say "  label now: %s" (ck-label))
          (ck-send "/usage")
          (ck-send "/tasks")
          (ck-send "/rc"))
        (lambda () (get-buffer "*codex usage*")))
  (list "check usage after turns" 5
        (lambda ()
          (let ((usage (ck-usage-text "*codex usage*")))
            (ck-say "  usage:\n%s" usage)
            (ck-check "context row" (string-match-p "Context" usage))
            (ck-check "session tokens" (string-match-p "Session" usage))))
        (lambda () t))
  (list "/compact" 180
        (lambda () (ck-send "/compact"))
        (lambda () (and (ck-turn-done) (ck-has "compacted"))))
  (list "a turn after compact" 120
        (lambda () (ck-send "Reply with exactly: POST-COMPACT"))
        (lambda () (ck-turn-done)))
  ;; History.
  (list "M-p and M-n" 5
        (lambda ()
          (with-current-buffer ck-buffer
            (agent-native--set-input "draft")
            (agent-native-previous-prompt 1)
            (ck-check "M-p brings the last prompt" (equal (agent-native--input-text) "Reply with exactly: POST-COMPACT"))
            (agent-native-previous-prompt 1)
            (ck-say "  M-p twice: %S" (agent-native--input-text))
            (agent-native-next-prompt 1)
            (agent-native-next-prompt 1)
            (ck-check "M-n back to the draft" (equal (agent-native--input-text) "draft"))
            (agent-native--set-input "")))
        (lambda () t))
  ;; Attach an image.
  (list "attach an image" 150
        (lambda ()
          (with-current-buffer ck-buffer (agent-native-attach (expand-file-name "red.png" ck-work)))
          (ck-say "  input: %S" (with-current-buffer ck-buffer (agent-native--input-text)))
          (ck-send "What single color fills this image? Reply with one lowercase word."))
        (lambda () (ck-turn-done)))
  (list "check image" 5
        (lambda () (ck-check "says red" (ck-has "^⏺ red"))
          (ck-check "prompt keeps the mark" (ck-has "\\[Image #1\\]")))
        (lambda () t))
  ;; A sub-agent.
  (list "sub-agent" 300
        (lambda () (ck-send "Spawn exactly one sub-agent to run `ls` in this directory and report the file names back. Wait for it, then reply with the list."))
        (lambda () (ck-turn-done)))
  (list "check sub-agent" 5
        (lambda ()
          (ck-check "Agent drawn" (ck-has "Agent("))
          (ck-check "no agents left running" (null (ck-local 'agent-native--agents))))
        (lambda () t))
  ;; Kill, then a prompt (refused), then start again: it resumes.
  (list "kill" 15
        (lambda () (with-current-buffer ck-buffer (agent-native-kill)))
        (lambda () (not (ck-live))))
  (list "send after kill, then reopen" 120
        (lambda ()
          (ck-check "status says ended" (string-match-p "ended" (ck-status)))
          (ck-send "Should not send")
          (ck-check "input kept" (equal (with-current-buffer ck-buffer (agent-native--input-text)) "Should not send"))
          (agent-native-codex)
          (ck-check "same buffer" (eq (ck-session-buffer) ck-buffer)))
        (lambda () (and (ck-has "↑ resumed") (agent-native-codex-ready (ck-local 'agent-native--conn)))))
  (list "a turn in the resumed session" 120
        (lambda ()
          (ck-check "same thread" (equal (ck-local 'agent-native--session-id) ck-thread-1))
          (ck-check "draft kept" (equal (with-current-buffer ck-buffer (agent-native--input-text)) "Should not send"))
          (with-current-buffer ck-buffer (agent-native--set-input ""))
          (ck-check "history drawn" (ck-has "^❯ Reply with exactly: ONE"))
          (ck-send "What was the first word I asked you to reply with? Reply with that word only."))
        (lambda () (ck-turn-done)))
  (list "check resumed memory" 5
        (lambda () (ck-check "remembers ONE" (ck-has "^⏺ ONE\n\n✻\\|^⏺ ONE$"))
          (ck-dump "after resume"))
        (lambda () t))
  ;; /clear: a new thread here.
  (list "/clear" 60
        (lambda () (ck-send "/clear"))
        (lambda () (and (ck-local 'agent-native--session-id)
                        (not (equal (ck-local 'agent-native--session-id) ck-thread-1))
                        (agent-native-codex-ready (ck-local 'agent-native--conn)))))
  (list "a turn after clear" 120
        (lambda ()
          (ck-check "old transcript gone" (not (ck-has "ONE")))
          (ck-send "Reply with exactly: CLEARED"))
        (lambda () (ck-turn-done)))
  ;; A second session, then exit it.
  (list "agent-native-new" 120
        (lambda ()
          (with-current-buffer ck-buffer (agent-native-new))
          (setq ck-buffer-2 (ck-session-buffer))
          (ck-check "a <2> buffer" (string-match-p "<2>" (buffer-name ck-buffer-2)))
          (let ((ck-buffer ck-buffer-2)) (ck-send "Reply with exactly: SECOND-SESSION")))
        (lambda () (ck-turn-done ck-buffer-2)))
  (list "exit the second session" 10
        (lambda ()
          (ck-check "second replied" (ck-has "SECOND-SESSION" ck-buffer-2))
          (with-current-buffer ck-buffer-2 (agent-native-exit)))
        (lambda () (not (buffer-live-p ck-buffer-2))))
  ;; Resume the first thread from the list.
  (list "resume from the list" 120
        (lambda ()
          (let* ((sessions (agent-native--past-sessions ck-work)))
            (ck-say "  past sessions: %S" (mapcar (lambda (s) (list (plist-get s :backend) (plist-get s :id) (plist-get s :prompt))) sessions))
            (let* ((target (seq-find (lambda (s) (equal (plist-get s :id) ck-thread-1)) sessions)))
              (ck-check "first thread listed" target)
              (with-current-buffer ck-buffer
                (agent-native-resume ck-thread-1 'codex)))))
        (lambda () (and (equal (ck-local 'agent-native--session-id) ck-thread-1)
                        (ck-has "↑ resumed")
                        (agent-native-codex-ready (ck-local 'agent-native--conn)))))
  ;; Exit while a turn runs.
  (list "exit mid-turn" 60
        (lambda () (ck-send "Run `sleep 20` in the shell."))
        (lambda () (ck-has "Shell(sleep 20)")))
  (list "exit now" 10
        (lambda () (with-current-buffer ck-buffer (agent-native-exit)))
        (lambda () (not (buffer-live-p ck-buffer))))))

(ck-red-png (expand-file-name "red.png" ck-work))

(ck-start)
