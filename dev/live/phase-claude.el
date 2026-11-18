;;; phase-claude.el --- every command with Claude Code, in different orders -*- lexical-binding: t -*-

(load (expand-file-name "harness.el" (file-name-directory load-file-name)))

(defvar ck-session-1 nil)
(defvar ck-buffer-2 nil)

(setq
 ck-steps
 (list
  (list "open, usage + model + prompt before ready" 120
        (lambda ()
          (agent-native-claude)
          (setq ck-buffer (ck-session-buffer))
          (ck-check "buffer name" (equal (buffer-name ck-buffer) (format "*claude: %s*" (abbreviate-file-name (directory-file-name ck-work)))))
          (with-current-buffer ck-buffer (agent-native-usage))
          (with-current-buffer ck-buffer (agent-native-set-model-and-effort "haiku" "low"))
          (ck-send "Reply with exactly: ONE"))
        (lambda () (and (ck-turn-done) (get-buffer "*claude usage*"))))
  (list "check first turn" 30
        (lambda ()
          (let ((usage (ck-usage-text "*claude usage*")))
            (ck-say "  usage before ready:\n%s" usage)
            (ck-check "usage shows limits" (string-match-p "Current\\|session" (or usage ""))))
          (ck-check "prompt drawn once" (= 1 (ck-count "^❯ Reply with exactly: ONE")))
          (ck-check "reply ONE" (ck-has "^⏺ ONE"))
          (setq ck-session-1 (ck-local 'agent-native--session-id)))
        (lambda () (string-match-p "haiku.*low" (ck-label))))
  (list "steer a running turn" 120
        (lambda ()
          (setq ck-permission 'allow)
          (ck-send "Run `sleep 6` with Bash, then reply with exactly: DONE-A"))
        (lambda () (ck-has "Bash(sleep 6)")))
  (list "send while busy" 120
        (lambda () (ck-send "Also end your reply with the word DONE-B"))
        (lambda () (and (ck-turn-done) (null (ck-local 'agent-native--queued)) (ck-has "DONE-B"))))
  (list "check steering" 5
        (lambda ()
          (ck-check "first prompt once" (= 1 (ck-count "^❯ Run `sleep 6`")))
          (ck-check "second prompt once" (= 1 (ck-count "^❯ Also end your reply")))
          (ck-check "not busy" (not (ck-busy))))
        (lambda () t))
  (list "interrupt at once" 60
        (lambda ()
          (ck-send "Run `sleep 20` with Bash, then reply with exactly: SLEPT")
          (with-current-buffer ck-buffer (agent-native-interrupt)))
        (lambda () (and (not (ck-busy)) (ck-has "Interrupted"))))
  (list "a turn after the interrupt runs" 90
        (lambda ()
          (ck-check "no SLEPT" (not (ck-has "^⏺ SLEPT")))
          (ck-send "Reply with exactly: AFTER-INT"))
        (lambda () (ck-turn-done)))
  (list "check after interrupt" 5
        (lambda ()
          (ck-check "AFTER-INT answered" (ck-has "^⏺ AFTER-INT"))
          (ck-check "no error note" (not (ck-has "^Error"))))
        (lambda () t))
  (list "interrupt a running command" 90
        (lambda () (ck-send "Run `sleep 30` with Bash, then reply with exactly: SLEPT2"))
        (lambda () (ck-has "Bash(sleep 30)")))
  (list "interrupt now" 30
        (lambda () (with-current-buffer ck-buffer (agent-native-interrupt)))
        (lambda () (and (not (ck-busy)) (= 2 (ck-count "Interrupted")))))
  (list "deny a command" 120
        (lambda ()
          (setq ck-permission 'deny ck-asked nil)
          (ck-send "Run `touch denied.txt` with Bash."))
        (lambda () (and (not (ck-busy)) ck-asked (ck-has "Denied"))))
  (list "check deny" 5
        (lambda ()
          (ck-check "denied.txt not made" (not (file-exists-p (expand-file-name "denied.txt" ck-work))))
          (ck-check "no permission left pending" (null (ck-local 'agent-native--pending)))
          (ck-check "no error note" (not (ck-has "^Error"))))
        (lambda () t))
  (list "always allow" 150
        (lambda ()
          (setq ck-permission 'always ck-asked nil)
          (ck-send "Run `touch a1.txt` with Bash. Then, as a separate Bash call, run `touch a1.txt` again. Then reply with exactly: TOUCHED"))
        (lambda () (ck-turn-done)))
  (list "check always" 5
        (lambda ()
          (ck-say "  asked %d times: %S" (length ck-asked) (mapcar #'car ck-asked))
          (ck-check "asked once" (= 1 (length ck-asked)))
          (setq ck-permission 'allow))
        (lambda () t))
  (list "cycle to accept-edits, write without asking" 150
        (lambda ()
          ;; "Always" may have switched it already, as Claude Code suggests.
          (ck-say "  mode before cycling: %s" (ck-local 'agent-native--mode))
          (with-current-buffer ck-buffer
            (while (not (eq agent-native--mode 'accept-edits)) (agent-native-cycle-mode)))
          (ck-check "mode accept-edits" (eq (ck-local 'agent-native--mode) 'accept-edits))
          (setq ck-asked nil)
          (ck-send "Use the Write tool to create b.txt containing the line hi. Then reply with exactly: WROTE"))
        (lambda () (ck-turn-done)))
  (list "check accept-edits" 5
        (lambda ()
          (ck-check "b.txt made" (file-exists-p (expand-file-name "b.txt" ck-work)))
          (ck-check "not asked" (null ck-asked))
          (ck-check "mode still accept-edits" (eq (ck-local 'agent-native--mode) 'accept-edits)))
        (lambda () t))
  (list "plan mode" 240
        (lambda ()
          (with-current-buffer ck-buffer (agent-native-cycle-mode))
          (ck-check "mode plan" (eq (ck-local 'agent-native--mode) 'plan))
          (setq ck-permission 'deny ck-asked nil)
          (ck-send "Plan how to add a README.md to this directory, in two short steps, then present the plan."))
        (lambda () (ck-turn-done)))
  (list "bypass, then default" 150
        (lambda ()
          (ck-say "  asked in plan mode: %S" (mapcar #'car ck-asked))
          (ck-check "no README written in plan mode" (not (file-exists-p (expand-file-name "README.md" ck-work))))
          (ck-check "mode still plan" (eq (ck-local 'agent-native--mode) 'plan))
          (setq ck-permission 'allow)
          (with-current-buffer ck-buffer (agent-native-cycle-mode))
          (ck-check "mode bypass" (eq (ck-local 'agent-native--mode) 'bypass))
          (with-current-buffer ck-buffer (agent-native-cycle-mode))
          (ck-check "mode default again" (eq (ck-local 'agent-native--mode) 'default))
          (setq ck-asked nil)
          (ck-send "Run `mkdir newdir` with Bash, then reply with exactly: BACK"))
        (lambda () (ck-turn-done)))
  (list "check back in default" 5
        (lambda ()
          (ck-check "asked again in default" ck-asked)
          (ck-check "BACK" (ck-has "^⏺ BACK")))
        (lambda () t))
  (list "a question" 150
        (lambda ()
          (setq ck-answers (list "Blue"))
          (ck-send "Use the AskUserQuestion tool to ask me one question: which color I prefer, with the options Red and Blue. Then reply with exactly the color I chose, in capitals."))
        (lambda () (ck-turn-done)))
  (list "/effort high typed" 120
        (lambda ()
          (ck-check "BLUE" (ck-has "^⏺ BLUE"))
          (ck-send "/effort high"))
        (lambda () (string-match-p "high" (ck-label))))
  (list "/model sonnet typed, then back" 120
        (lambda ()
          (ck-check "effort echo not drawn as a prompt" (not (ck-has "^❯ /effort")))
          (ck-send "/model sonnet"))
        (lambda () (string-match-p "sonnet" (ck-label))))
  (list "/model haiku, turn" 120
        (lambda ()
          (ck-send "/model haiku")
          (ck-send "Reply with exactly: MODEL"))
        (lambda () (ck-turn-done)))
  (list "/usage, /tasks, /cost" 30
        (lambda ()
          (ck-say "  label now: %s" (ck-label))
          (ck-check "label haiku" (string-match-p "haiku" (ck-label)))
          (ck-send "/tasks")
          (ck-send "/cost"))
        (lambda () (get-buffer "*claude usage*")))
  (list "check usage after turns" 5
        (lambda ()
          (let ((usage (ck-usage-text "*claude usage*")))
            (ck-say "  usage:\n%s" usage)
            (ck-check "context row" (string-match-p "Context" usage))))
        (lambda () t))
  (list "remote control: off" 30
        (lambda ()
          (ck-say "  remote: %S %S" (ck-local 'agent-native--remote) (ck-local 'agent-native--remote-url))
          (if (ck-local 'agent-native--remote-url)
              (progn (setq ck-choices (list ?d)) (ck-send "/rc"))
            (ck-say "  remote was not on at start")))
        (lambda () (not (ck-local 'agent-native--remote))))
  (list "remote control: on" 60
        (lambda () (ck-send "/remote-control"))
        (lambda () (and (ck-local 'agent-native--remote-url) (string-match-p "remote" (ck-label)))))
  (list "/compact" 240
        (lambda () (ck-send "/compact"))
        (lambda () (ck-turn-done)))
  (list "a turn after compact" 120
        (lambda () (ck-send "Reply with exactly: POST-COMPACT"))
        (lambda () (ck-turn-done)))
  (list "M-p and M-n" 5
        (lambda ()
          (with-current-buffer ck-buffer
            (agent-native--set-input "draft")
            (agent-native-previous-prompt 1)
            (ck-check "M-p brings the last prompt" (equal (agent-native--input-text) "Reply with exactly: POST-COMPACT"))
            (agent-native-next-prompt 1)
            (ck-check "M-n back to the draft" (equal (agent-native--input-text) "draft"))
            (agent-native--set-input "")))
        (lambda () t))
  (list "attach an image" 150
        (lambda ()
          (with-current-buffer ck-buffer (agent-native-attach (expand-file-name "red.png" ck-work)))
          (ck-send "What single color fills this image? Reply with one lowercase word."))
        (lambda () (ck-turn-done)))
  (list "sub-agent" 300
        (lambda ()
          (ck-check "says red" (ck-has "^⏺ red"))
          (ck-send "Use the Agent tool with subagent_type general-purpose to run `ls` in this directory and report the file names. Then reply with the list."))
        (lambda () (ck-turn-done)))
  (list "check sub-agent" 5
        (lambda ()
          (ck-check "Agent drawn" (ck-has "Agent("))
          (ck-check "no agents left running" (null (ck-local 'agent-native--agents))))
        (lambda () t))
  (list "kill" 15
        (lambda () (with-current-buffer ck-buffer (agent-native-kill)))
        (lambda () (not (ck-live))))
  (list "send after kill, then reopen" 120
        (lambda ()
          (ck-check "status says ended" (string-match-p "ended" (ck-status)))
          (ck-send "Should not send")
          (ck-check "input kept" (equal (with-current-buffer ck-buffer (agent-native--input-text)) "Should not send"))
          (agent-native-claude)
          (ck-check "same buffer" (eq (ck-session-buffer) ck-buffer)))
        (lambda () (and (ck-has "↑ resumed") (ck-local 'agent-native--commands))))
  (list "a turn in the resumed session" 120
        (lambda ()
          (ck-check "same session" (equal (ck-local 'agent-native--session-id) ck-session-1))
          (ck-check "draft kept" (equal (with-current-buffer ck-buffer (agent-native--input-text)) "Should not send"))
          (with-current-buffer ck-buffer (agent-native--set-input ""))
          (ck-check "history drawn" (ck-has "^❯ Reply with exactly: ONE"))
          (ck-send "What was the first word I asked you to reply with? Reply with that word only."))
        (lambda () (ck-turn-done)))
  (list "/clear" 60
        (lambda ()
          (ck-check "remembers ONE" (ck-has "^⏺ ONE$"))
          (ck-dump "after resume")
          (ck-send "/clear"))
        (lambda () (and (ck-local 'agent-native--commands) (not (ck-has "ONE")))))
  (list "a turn after clear" 120
        (lambda () (ck-send "Reply with exactly: CLEARED"))
        (lambda () (ck-turn-done)))
  (list "agent-native-new" 120
        (lambda ()
          (ck-check "new session id" (not (equal (ck-local 'agent-native--session-id) ck-session-1)))
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
  (list "resume from the list" 120
        (lambda ()
          (let ((sessions (agent-native--past-sessions ck-work)))
            (ck-say "  past sessions: %S" (mapcar (lambda (s) (list (plist-get s :backend) (plist-get s :id) (plist-get s :title) (plist-get s :prompt))) sessions))
            (ck-check "first session listed" (seq-find (lambda (s) (equal (plist-get s :id) ck-session-1)) sessions))
            (with-current-buffer ck-buffer (agent-native-resume ck-session-1 'claude))))
        (lambda () (and (equal (ck-local 'agent-native--session-id) ck-session-1)
                        (ck-has "↑ resumed") (ck-local 'agent-native--commands))))
  (list "exit mid-turn" 60
        (lambda () (ck-send "Run `sleep 20` with Bash."))
        (lambda () (ck-has "Bash(sleep 20)")))
  (list "exit now" 10
        (lambda () (with-current-buffer ck-buffer (agent-native-exit)))
        (lambda () (not (buffer-live-p ck-buffer))))))

(ck-red-png (expand-file-name "red.png" ck-work))

(ck-start)
