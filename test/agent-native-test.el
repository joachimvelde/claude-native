;;; agent-native-test.el --- replay real sessions through the UI -*- lexical-binding: t -*-

;; Replay tests run a session buffer against dev/fake-claude.py or
;; dev/fake-codex.py, which replay recordings of real sessions
;; (dev/record-claude.py, dev/record-codex.py), and check what the buffer
;; shows. Other tests feed a backend messages by hand, or draw directly.
;; Run: emacs --batch -L . -l test/agent-native-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'agent-native)

(defconst agent-native-test--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defvar agent-native-test--sandbox nil
  "Where the recording being replayed ran; tool paths in it are under this.")

(defun agent-native-test--recorded-cwd (recording)
  "The directory RECORDING (from dev/record-claude.py or dev/record-codex.py) ran in."
  (seq-some (lambda (record)
              (let ((msg (alist-get 'msg record)))
                (or (and (equal (alist-get 'subtype msg) "init") (alist-get 'cwd msg))
                    (alist-get 'cwd (alist-get 'thread (alist-get 'result msg))))))
            (agent-native--read-lines recording)))

(defun agent-native-test--wait (predicate &optional seconds)
  "Process output until PREDICATE holds, or fail after SECONDS."
  (let ((end (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (should (funcall predicate))))

(defmacro agent-native-test--session (recording &rest body)
  "Run BODY in a session buffer replaying RECORDING."
  (declare (indent 1))
  ;; Recordings hold account data, so they stay out of git: make one with
  ;; dev/record.py to run these tests.
  `(let* ((_ (skip-unless (file-exists-p (expand-file-name ,recording agent-native-test--root))))
          (process-environment (cons (concat "AGENT_NATIVE_REPLAY=" (expand-file-name ,recording agent-native-test--root))
                                     process-environment))
          (agent-native-claude-program (expand-file-name "dev/fake-claude.py" agent-native-test--root))
          (agent-native-permission-function (lambda (_) 'allow))
          (agent-native-test--sandbox
           (file-name-as-directory
            (agent-native-test--recorded-cwd (expand-file-name ,recording agent-native-test--root))))
          ;; The recording's directory may be gone (a temp dir): the
          ;; process needs it to start in.
          (made (unless (file-directory-p agent-native-test--sandbox)
                  (make-directory agent-native-test--sandbox t)
                  t))
          (default-directory agent-native-test--sandbox)
          (buffer (get-buffer-create "*agent-native-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (setq default-directory agent-native-test--sandbox)
           (agent-native--open 'claude)
           ,@body)
       (with-current-buffer buffer (agent-native-stop agent-native--conn))
       (kill-buffer buffer)
       (when made (delete-directory agent-native-test--sandbox t)))))

(defun agent-native-test--type-and-send (text)
  (goto-char (point-max))
  (insert text)
  (agent-native-send))

(defun agent-native-test--transcript ()
  (buffer-substring-no-properties (point-min) agent-native--input))

(defun agent-native-test--conn (backend)
  "A BACKEND connection with no process, drawing into this buffer, which
becomes its session buffer."
  (let ((buffer (current-buffer)))
    (setq agent-native--backend backend)
    (funcall (if (eq backend 'codex) #'agent-native-codex--make #'agent-native-claude--make)
             :backend backend
             :on-event (lambda (type props) (agent-native--event buffer type props)))))

(defun agent-native-test--receive (conn &rest messages)
  "Hand MESSAGES to CONN's backend as if its process wrote them."
  (dolist (msg messages)
    (agent-native-backend-receive conn msg)))

(defun agent-native-test--replay (records)
  "Draw Claude session RECORDS into this buffer as a resumed session."
  (let ((conn (agent-native-test--conn 'claude)))
    (agent-native--emit conn 'history-start)
    (agent-native-claude--replay conn records)
    (agent-native--emit conn 'history-end)))

(ert-deftest agent-native-replays-a-session ()
  (agent-native-test--session "dev/recordings/session1.jsonl"
    ;; Handshake: commands arrive.
    (agent-native-test--wait (lambda () agent-native--commands))
    (should (assoc "compact" agent-native--commands))
    ;; Turn 1: Write, Edit (permission granted), Bash, then text.
    (agent-native-test--type-and-send "Use the Write tool to create notes.txt")
    (agent-native-test--wait (lambda () (string-match-p "printed" (agent-native-test--transcript))))
    (agent-native-test--wait (lambda () (not agent-native--busy)))
    (let ((text (agent-native-test--transcript)))
      (should (string-match-p "^❯ Use the Write tool to create notes.txt\n\n⏺ Write(notes.txt)" text))
      (should (string-match-p "⎿  Wrote 1 line to notes.txt" text))
      (should (string-match-p "⏺ Edit(notes.txt)" text))
      (should (string-match-p "- hi" text))
      (should (string-match-p "\\+ hello" text))
      (should (string-match-p "⏺ Bash(cat " text))
      (should (string-match-p "⎿  hello" text))
      (should (string-match-p "⏺ I created `notes.txt`" text))
      ;; Allowed requests leave no note behind, as in the terminal.
      (should-not (string-match-p "Waiting for permission" text))
      (should-not (string-match-p "Allowed" text)))
    (should (equal agent-native--model "claude-haiku-5-5"))
    ;; Model and effort show at the right end of the rule above the input.
    (agent-native-test--wait (lambda () (equal agent-native--effort "medium")))
    (should (string-match-p "haiku-5-5 · ◐ medium"
                            (substring-no-properties (overlay-get agent-native--label-overlay 'after-string))))
    (should (> agent-native--cost 0))
    ;; The input area is empty and still editable.
    (should (equal (agent-native--input-text) ""))
    ;; Permission mode, then an interrupted turn.
    (agent-native-cycle-mode)
    (agent-native-test--wait (lambda () (eq agent-native--mode 'accept-edits)))
    (agent-native-test--type-and-send "Think step by step")
    (agent-native-test--wait (lambda () (> agent-native--thinking-tokens 0)))
    (agent-native-interrupt)
    (agent-native-test--wait (lambda () (string-match-p "Interrupted" (agent-native-test--transcript))))
    (agent-native-test--wait (lambda () (not agent-native--busy)))
    ;; The interrupted turn ends as an error result; only "Interrupted" shows.
    (should-not (string-match-p "error_during_execution" (agent-native-test--transcript)))
    ;; A slash command's output arrives whole, not streamed.
    (agent-native-test--type-and-send "/compact")
    (agent-native-test--wait (lambda () (string-match-p "subscription" (agent-native-test--transcript))))))

(ert-deftest agent-native-draws-usage ()
  (agent-native-test--session "dev/recordings/session1.jsonl"
    (agent-native-test--wait (lambda () agent-native--commands))
    ;; /usage and its aliases ask claude for numbers and show them in a
    ;; popup; the transcript stays as it was.
    (agent-native-test--type-and-send "/cost")
    (agent-native-test--wait (lambda () (get-buffer "*claude usage*")))
    (should-not (string-match-p "usage\\|Session" (agent-native-test--transcript)))
    (let ((text (with-current-buffer "*claude usage*"
                  (should (derived-mode-p 'agent-native-usage-mode))
                  (should buffer-read-only)
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer "*claude usage*")
      (should (string-match-p "Current session +█+░  95%  resets Nov 10, " text))
      (should (string-match-p "Current week (all models) +█+░+  21%  resets Nov 13, " text))
      (should (string-match-p "Current week (Fable) +█+░+  11%" text))
      (should (string-match-p "Context +█░+   2%  23k of 1M tokens" text))
      (should (string-match-p "Session +\\$0\\.42 · 1m 15s API time · \\+12 −3 lines" text))
      (should (string-match-p "Last 24h · 803 requests · 18 sessions" text))
      (should (string-match-p " 87%  Start fresh when you switch tasks · run /clear" text))
      (should (string-match-p "Top skills: /dataviz 2%" text))
      (should-not (string-match-p "Top subagents" text)))
    (with-current-buffer buffer
      (should (equal (agent-native--input-text) "")))))

(ert-deftest agent-native-usage-without-a-plan ()
  ;; An API key has no plan limits; an old claude gives no context either.
  (let ((text (agent-native--usage-text
               (agent-native-claude--usage-report
                '((session . ((total_cost_usd . 1.5))) (rate_limits_available . :false))
                nil))))
    (should (equal (substring-no-properties text) "Session  $1.50"))))

;; Task messages as claude 2.1.322 writes them, cut down to what is read.
(defun agent-native-test--task (subtype id &rest fields)
  `((type . "system") (subtype . ,subtype) (task_id . ,id) ,@fields))

(ert-deftest agent-native-lists-agents-at-work ()
  (with-temp-buffer
    (agent-native-mode)
    (let* ((session (current-buffer))
           (conn (agent-native-test--conn 'claude))
           (receive (lambda (msg) (agent-native-test--receive conn msg))))
      ;; The session runs haiku at medium; the first call asks for high.
      (setq agent-native--model "claude-haiku-5-5" agent-native--effort "medium")
      (funcall receive '((type . "assistant")
                         (message . ((id . "m1")
                                     (content . [((type . "tool_use") (id . "toolu_1") (name . "Agent")
                                                  (input . ((description . "Count files") (effort . "high"))))])))))
      (funcall receive (agent-native-test--task
                        "task_started" "a1" '(tool_use_id . "toolu_1") '(description . "Count files")
                        '(subagent_type . "general-purpose") '(task_type . "local_agent")))
      ;; Its first message says the model it really runs on.
      (funcall receive '((type . "assistant") (parent_tool_use_id . "toolu_1")
                         (message . ((model . "claude-sonnet-5-5") (content . [])))))
      (funcall receive (agent-native-test--task
                        "task_started" "a2" '(tool_use_id . "toolu_2") '(description . "Slow echo")
                        '(subagent_type . "Explore") '(task_type . "local_agent")))
      ;; An agent's own shell is a task too, but not an agent.
      (funcall receive (agent-native-test--task
                        "task_started" "b1" '(description . "Print done") '(task_type . "local_bash")
                        '(parent_task_id . "a1")))
      (funcall receive (agent-native-test--task
                        "task_progress" "a1" '(description . "Running Print done")
                        '(usage . ((total_tokens . 42751) (tool_uses . 2) (duration_ms . 1954)))))
      (should (equal (mapcar (lambda (agent) (plist-get agent :id)) agent-native--agents) '("a1" "a2")))
      (should (string-match-p "2 agents: C-c C-t" (agent-native--status-line)))
      (save-window-excursion
        (agent-native-agents)
        (unwind-protect
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (equal (buffer-name) "*claude agents*"))
              (should (derived-mode-p 'agent-native-agents-mode))
              (should buffer-read-only)
              ;; Name and type, what it does now, then model, effort,
              ;; tool uses, tokens and time.
              (should (string-match-p
                       (concat "⏺ Count files  general-purpose\n  ⎿  Running Print done\n"
                               " +sonnet-5-5 · ◐ high · 2 tool uses · 43k tokens · [0-9]+s")
                       text))
              (should (string-match-p
                       "⏺ Slow echo  Explore\n  ⎿  Starting…\n +haiku-5-5 · ◐ medium · 0 tool uses · 0 tokens · [0-9]+s"
                       text))
              ;; RET on an agent goes to its call in the session.
              (should (equal (plist-get (get-text-property (point) 'agent-native-agent) :id) "a1"))
              (agent-native-agents-visit)
              (should (eq (current-buffer) session))
              (should (looking-at-p "⏺ Agent(Count files)"))
              ;; The shell ending leaves its agent; an agent ending goes.
              (funcall receive (agent-native-test--task "task_notification" "b1" '(status . "completed")))
              (funcall receive (agent-native-test--task "task_updated" "a1" '(patch . ((status . "completed")))))
              (funcall receive (agent-native-test--task "task_notification" "a1" '(status . "completed")))
              (should (equal (mapcar (lambda (agent) (plist-get agent :id)) agent-native--agents) '("a2")))
              (should (string-match-p "1 agent: C-c C-t" (agent-native--status-line)))
              (funcall receive (agent-native-test--task "task_notification" "a2" '(status . "failed")))
              (should-not agent-native--agents)
              (with-current-buffer "*claude agents*"
                (agent-native--agents-redraw)
                (should (string-match-p "No sub-agents are running" (buffer-string)))))
          (kill-buffer "*claude agents*"))))))

(ert-deftest agent-native-transcript-is-read-only ()
  (agent-native-test--session "dev/recordings/session1.jsonl"
    (agent-native-test--wait (lambda () agent-native--commands))
    (goto-char (point-min))
    (should-error (insert "x") :type 'text-read-only)
    (goto-char (point-max))
    (insert "typed")
    (should (equal (agent-native--input-text) "typed"))))

(provide 'agent-native-test)

(ert-deftest agent-native-completes-commands-and-files ()
  (agent-native-test--session "dev/recordings/session1.jsonl"
    (agent-native-test--wait (lambda () agent-native--commands))
    (goto-char (point-max))
    (insert "/compa")
    (let ((capf (agent-native--complete)))
      (should capf)
      (should (member "compact" (all-completions "compa" (nth 2 capf)))))
    ;; The file to complete.
    (with-temp-file (expand-file-name "notes.txt" agent-native-test--sandbox))
    (agent-native--set-input "look at @no")
    (goto-char (point-max))
    (let ((capf (agent-native--complete)))
      (should capf)
      (should (member "notes.txt" (all-completions "no" (nth 2 capf)))))
    ;; A slash later in the text is no command.
    (agent-native--set-input "see a/b")
    (goto-char (point-max))
    (should-not (agent-native--complete))))

(defconst agent-native-test--live
  (file-name-as-directory (or (getenv "CN_LIVE") (expand-file-name "cn-live" temporary-file-directory)))
  "A directory with real sessions from manual testing (set CN_LIVE).")

(ert-deftest agent-native-lists-and-draws-past-sessions ()
  (skip-unless (file-directory-p (agent-native-claude--sessions-dir agent-native-test--live)))
  (let ((sessions (agent-native-backend-sessions 'claude agent-native-test--live)))
    (should sessions)
    ;; Newest first, each with its first prompt.
    (should (cl-every (lambda (s) (stringp (plist-get s :prompt))) sessions))
    (should (time-less-p (plist-get (car (last sessions)) :time) (plist-get (car sessions) :time)))
    (let* ((with-apple (seq-find (lambda (s) (let ((case-fold-search nil))
                                                  (string-match-p "APPLE BANANA" (plist-get s :prompt))))
                                 sessions))
           (file (agent-native-claude--session-file agent-native-test--live (plist-get with-apple :id))))
      (with-temp-buffer
        (setq default-directory agent-native-test--live)
        (agent-native-mode)
        (agent-native-test--replay (agent-native--read-lines file))
        (let ((text (buffer-substring-no-properties (point-min) agent-native--input)))
          (should (string-match-p "^❯ Reply with exactly: APPLE BANANA" text))
          (should (string-match-p "^⏺ APPLE BANANA" text)))))))

(ert-deftest agent-native-knows-native-commands ()
  (should (eq (agent-native--native-command "/resume") 'agent-native-resume))
  (should (eq (agent-native--native-command "/model ") 'agent-native-choose-model))
  (should (eq (agent-native--native-command "/effort") 'agent-native-set-effort))
  (should (eq (agent-native--native-command "/tasks") 'agent-native-agents))
  (should-not (agent-native--native-command "/effort high"))
  (should-not (agent-native--native-command "/compact"))
  (should-not (agent-native--native-command "please /resume")))

(defun agent-native-test--visible (start end)
  "The text from START to END that is not invisible."
  (let ((out ""))
    (dotimes (i (- end start))
      (let ((p (+ start i)))
        (unless (get-text-property p 'invisible) (setq out (concat out (string (char-after p)))))))
    out))

(ert-deftest agent-native-renders-markdown-like-the-terminal ()
  (with-temp-buffer
    (agent-native-mode)
    (agent-native--text-whole "# Title\nSome **bold**, *it* and `code`.\n```\nraw **kept** `x`\n```")
    (let ((shown (agent-native-test--visible (point-min) agent-native--out)))
      (should (string-match-p "^⏺ Title$" shown))
      (should (string-match-p "Some bold, it and code\\." shown))
      ;; Code blocks keep their text, lose their fences.
      (should (string-match-p "raw \\*\\*kept\\*\\* `x`" shown))
      (should-not (string-match-p "```" shown)))))

(ert-deftest agent-native-tool-calls-are-quieter-than-text ()
  (with-temp-buffer
    (agent-native-mode)
    (agent-native--text-whole "The answer.")
    (agent-native--tool-use "t1" "Bash" '((command . "seq 1 10")))
    (agent-native--tool-result "t1" (mapconcat #'number-to-string (number-sequence 1 10) "\n") nil)
    (agent-native--tool-use "t2" "Edit" '((file_path . "/x/a") (old_string . "old") (new_string . "new")))
    (agent-native--tool-result "t2" "updated" nil)
    (agent-native--tool-use "t3" "Bash" '((command . "false")))
    (agent-native--tool-result "t3" "boom" t)
    (cl-flet ((face-of (text)
                (goto-char (point-min))
                (search-forward text)
                (get-text-property (match-beginning 0) 'face)))
      ;; Claude's text is plain; a tool's arguments and output are dim.
      (should-not (face-of "The answer."))
      (should (eq (face-of "(seq 1 10)") 'agent-native-tool-summary))
      (should (eq (face-of "⎿") 'agent-native-tool-result))
      (should (eq (face-of "4") 'agent-native-tool-result))
      ;; Diffs and errors keep their own colours.
      (should (eq (face-of "+ new") 'agent-native-diff-added))
      (should (eq (face-of "boom") 'agent-native-tool-error))
      ;; Lines shown by expanding are dim too.
      (goto-char (point-min))
      (search-forward "… +6")
      (agent-native-send)
      (should (eq (face-of "10\n") 'agent-native-tool-result)))))

(ert-deftest agent-native-draws-org-as-org ()
  (let ((agent-native-format 'org))
    (should (equal (car (agent-native-claude--format-args)) "--append-system-prompt"))
    (with-temp-buffer
      (agent-native-mode)
      (agent-native--text-whole
       (concat "* Title\nSome *bold* and ~code~, see [[https://example.com][the site]].\n"
               "| name | n |\n|-\n| a longer cell | 1 |\n"
               "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src"))
      (let ((text (buffer-substring-no-properties (point-min) agent-native--out))
            (shown (agent-native-test--visible (point-min) agent-native--out)))
        ;; The text is Org as Claude wrote it, from the first column (the
        ;; indent is drawn), so a copy of it is Org too; tables are aligned.
        (should (string-match-p "^⏺ \\* Title$" text))
        (should (string-match-p "^Some \\*bold\\* and ~code~" text))
        (should (string-match-p "^| name          | n |$" text))
        (should (string-match-p "^|---------------\\+---|$" text))
        ;; Marks and link targets hide.
        (should (string-match-p "^Some bold and code, see the site\\.$" shown)))
      (cl-flet ((at (text)
                  (goto-char (point-min))
                  (search-forward text)
                  (goto-char (match-beginning 0))))
        (at "Title")
        (should (memq 'org-level-1 (ensure-list (get-text-property (point) 'face))))
        (at "Some")
        (should (equal (get-text-property (point) 'line-prefix) agent-native--org-indent))
        (should (get-text-property (point) 'read-only))
        ;; Code is highlighted in its language.
        (at "\"hi\"")
        (should (memq 'font-lock-string-face (ensure-list (get-text-property (point) 'face))))
        ;; RET on a link opens it.
        (at "the site")
        (let (opened)
          (cl-letf (((symbol-function 'org-link-open-from-string)
                     (lambda (link &rest _) (setq opened link))))
            (agent-native-send))
          (should (equal opened "[[https://example.com]]")))))
    ;; Markdown still reads as Markdown: old sessions, slash commands.
    (with-temp-buffer
      (agent-native-mode)
      (agent-native--text-whole "# Title\nSome **bold** and `code`.")
      (should (equal (agent-native-test--visible (point-min) agent-native--out)
                     "⏺ Title\nSome bold and code.")))
    ;; The cursor and the markers around a redrawn block stay put.
    (with-temp-buffer
      (agent-native-mode)
      (agent-native--text-whole "First.")
      (agent-native--text-start)
      (agent-native--text-delta "Before\n| a | b |\n| long cell | c |\nAfter")
      (goto-char (point-min))
      (search-forward "After")
      (agent-native--block-stop)
      (should (looking-back "After" (- (point) 5)))
      (should (= (char-before agent-native--out) ?r))
      (should (equal (agent-native--input-text) "")))))

(ert-deftest agent-native-claude-sets-model-and-effort-at-once ()
  ;; Both are control requests, applied before any prompt sent after
  ;; (a /effort prompt would wait behind one); a refusal is said.
  (let (sent written (native-comp-enable-subr-trampolines nil))
    (cl-letf (((symbol-function 'agent-native-claude--control)
               (lambda (_conn subtype &optional fields callback) (push (list subtype fields callback) sent)))
              ((symbol-function 'agent-native--write) (lambda (_conn object) (push object written)))
              ((symbol-function 'agent-native-live-p) (lambda (_conn) t)))
      (with-temp-buffer
        (agent-native-mode)
        (setq agent-native--conn (agent-native-test--conn 'claude))
        (agent-native-set-model-and-effort "opus" "high")
        (should (equal (mapcar (lambda (s) (list (car s) (cadr s))) (reverse sent))
                       '(("set_model" ((model . "opus")))
                         ("apply_flag_settings" ((settings . ((effortLevel . "high"))))))))
        (should-not written)
        (setq sent nil)
        (agent-native-set-effort "low")
        (should (equal (cadr (car sent)) '((settings . ((effortLevel . "low"))))))
        ;; A model claude does not know.
        (setq sent nil)
        (agent-native-set-model "gpt-6-luna")
        (funcall (nth 2 (car sent)) '((subtype . "error") (error . "Model 'gpt-6-luna' not found")))
        (should (string-match-p "Error: Model 'gpt-6-luna' not found" (agent-native-test--transcript)))
        ;; Then what claude uses is asked for again, for the label.
        (should (equal (car (car sent)) "get_settings"))))))

(ert-deftest agent-native-says-what-always-keeps ()
  ;; Claude's suggestions, as it sends them for a Write, and for a command.
  (should (equal (agent-native-claude--always-text [((type . "setMode") (mode . "acceptEdits") (destination . "session"))])
                 "Yes, and switch to accept-edits mode in this session"))
  (should (equal (agent-native-claude--always-text
                  [((type . "addRules") (rules . [((toolName . "Bash") (ruleContent . "touch a1.txt"))])
                    (behavior . "allow") (destination . "localSettings"))])
                 "Yes, and allow Bash(touch a1.txt) in this project, from now on"))
  (should (equal (agent-native-claude--always-text nil) "Yes, this time (Claude offers nothing to keep)"))
  ;; Codex keeps a command rule for good, in every project.
  (should (equal (agent-native-codex--always-text
                  '("accept" ((acceptWithExecpolicyAmendment . ((execpolicy_amendment . ["touch" "a1.txt"])))) "cancel"))
                 "Yes, and from now on, in every project, allow commands that start with `touch a1.txt`"))
  (should (string-match-p "in this session" (agent-native-codex--always-text '("accept" "acceptForSession"))))
  ;; The question says it.
  (let (choices)
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (_prompt cs &rest _) (setq choices cs) (car cs))))
      (with-temp-buffer
        (agent-native-mode)
        (agent-native--ask-permission (list :name "Bash" :input '((command . "ls")) :always "Yes, and keep it"))
        (should (equal (nth 2 (assq ?a choices)) "Yes, and keep it"))))))

(ert-deftest agent-native-input-edits-keep-point-out-of-sight ()
  ;; The session in a window that is not selected (C-c C-t's list is): an
  ;; edit of its input lays out that window, which must not move point
  ;; there. An attached file's mark was split around it.
  (let ((session (generate-new-buffer "session"))
        (other (generate-new-buffer "other"))
        (file (make-temp-file "agent-native" nil ".png")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer session)
          (agent-native-mode)
          (setq agent-native--model "opus")
          (select-window (split-window))
          (switch-to-buffer other)
          (with-current-buffer session
            (agent-native--set-input "look:")
            (agent-native-attach file)
            (should (equal (agent-native--input-text) "look: [Image #1] "))
            (should (= (point) (point-max)))))
      (kill-buffer session)
      (kill-buffer other)
      (delete-file file))))

(ert-deftest agent-native-resumed-session-is-known-before-its-first-turn ()
  ;; Claude names the session only once a turn runs; ended before that,
  ;; starting again must still carry on with it.
  (let (started set)
    (cl-letf (((symbol-function 'agent-native-backend-start)
               (lambda (_backend _directory &rest keys)
                 (push (list (plist-get keys :resume) (plist-get keys :mode)) started)
                 nil))
              ((symbol-function 'agent-native-backend-set-model)
               (lambda (_conn model effort) (push (list model effort) set))))
      (with-temp-buffer
        (agent-native--open 'claude "abc")
        (should (equal agent-native--session-id "abc"))
        ;; It goes on as it was set up.
        (setq agent-native--model "opus" agent-native--effort "high" agent-native--mode 'plan)
        (let ((buffer (current-buffer)))
          (save-window-excursion
            (cl-letf (((symbol-function 'agent-native--session-buffers) (lambda (&rest _) (list buffer))))
              (agent-native--visit 'claude default-directory))))
        (should (equal (car started) '("abc" plan)))
        (should (equal set '(("opus" "high"))))
        (should (equal (list agent-native--model agent-native--mode) '("opus" plan)))
        ;; /clear: a new conversation, set up the same, as the agent's own.
        (setq set nil)
        (agent-native-clear)
        (should (equal (car started) '(nil plan)))
        (should (equal set '(("opus" "high"))))
        (should-not agent-native--session-id)
        ;; A fresh start has none until the agent says.
        (agent-native--open 'claude)
        (should-not agent-native--session-id)))))

(ert-deftest agent-native-resume-leaves-other-buffers-alone ()
  (let (opened)
    (cl-letf (((symbol-function 'agent-native--open)
               (lambda (backend id) (setq opened (list id (current-buffer) backend)))))
      ;; From a session buffer: resumed in that buffer.
      (with-temp-buffer
        (agent-native-mode)
        (setq agent-native--backend 'claude)
        (agent-native-resume "abc" 'claude)
        (should (equal opened (list "abc" (current-buffer) 'claude)))
        ;; A session of the other backend opens in that backend's buffer.
        (let ((origin (current-buffer)))
          (save-window-excursion
            (agent-native-resume "xyz" 'codex)
            (should-not (eq (nth 1 opened) origin))
            (should (string-prefix-p "*codex: " (buffer-name (nth 1 opened))))
            (should (eq (nth 2 opened) 'codex))
            (kill-buffer (nth 1 opened)))))
      ;; From any other buffer (here read-only, like dired): resumed in the
      ;; project's session buffer; the buffer keeps its text.
      (let ((origin (generate-new-buffer "a-file"))
            session)
        (unwind-protect
            (save-window-excursion
              (with-current-buffer origin
                (insert "a file's text")
                (setq buffer-read-only t)
                (agent-native-resume "abc" 'claude))
              (setq session (nth 1 opened))
              (should-not (eq session origin))
              (should (equal (buffer-name session)
                             (with-current-buffer origin
                               (agent-native--buffer-name 'claude (agent-native--project-root)))))
              (should-not (buffer-local-value 'buffer-read-only session))
              (should (equal (with-current-buffer origin (buffer-string)) "a file's text")))
          (kill-buffer origin)
          (when (buffer-live-p session) (kill-buffer session)))))))

(ert-deftest agent-native-expands-clipped-results ()
  (with-temp-buffer
    (agent-native-mode)
    (agent-native--tool-use "t1" "Bash" '((command . "seq 1 10")))
    (agent-native--tool-result "t1" (mapconcat #'number-to-string (number-sequence 1 10) "\n") nil)
    (let ((text (buffer-substring-no-properties (point-min) agent-native--input)))
      (should (string-match-p "… \\+6 lines" text))
      (should-not (string-match-p "^ +10$" text)))
    (goto-char (point-min))
    (search-forward "… +6")
    (agent-native-send)
    (let ((text (buffer-substring-no-properties (point-min) agent-native--input)))
      (should-not (string-match-p "lines (RET" text))
      (should (string-match-p "^ +10$" text)))))

(ert-deftest agent-native-undo-leaves-the-transcript-alone ()
  (with-temp-buffer
    (agent-native-mode)
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (goto-char (point-max))
    (insert "abc")
    (undo-boundary)
    ;; Claude writes while you type: above the input.
    (agent-native--text-whole "Claude said this.")
    (undo-boundary)
    (goto-char (point-max))
    (let ((last-command nil)) (undo))
    (should (equal (agent-native--input-text) ""))
    (should (string-match-p "Claude said this\\."
                            (buffer-substring-no-properties (point-min) agent-native--input)))))

(ert-deftest agent-native-italics-leave-code-and-maths-alone ()
  (with-temp-buffer
    (agent-native-mode)
    (agent-native--text-whole "Match `*.el` and `*.py`; 2*3*4 and *it* here.")
    (should (equal (agent-native-test--visible (point-min) agent-native--out)
                   "⏺ Match *.el and *.py; 2*3*4 and it here."))))

(ert-deftest agent-native-shows-questions-and-answers ()
  (with-temp-buffer
    (agent-native-mode)
    (agent-native--tool-use "q1" "AskUserQuestion"
                             '((questions . [((question . "Tea or coffee?") (options . [((label . "Tea")) ((label . "Coffee"))]))])))
    (agent-native--tool-result "q1" "Your questions have been answered: \"Tea or coffee?\"=\"Tea\". You can now continue." nil)
    (let ((text (buffer-substring-no-properties (point-min) agent-native--input)))
      (should (string-match-p "⏺ AskUserQuestion(Tea or coffee\\?)" text))
      (should (string-match-p "⎿  · Tea or coffee\\? → Tea" text))
      (should-not (string-match-p "You can now continue" text)))))

(ert-deftest agent-native-history-offers-resumed-prompts ()
  (with-temp-buffer
    (agent-native-mode)
    (agent-native-test--replay
     (list '((type . "user") (message . ((role . "user") (content . "first"))))
           '((type . "user") (message . ((role . "user") (content . "second"))))))
    (agent-native-previous-prompt 1)
    (should (equal (agent-native--input-text) "second"))
    (agent-native-previous-prompt 1)
    (should (equal (agent-native--input-text) "first"))))

(ert-deftest agent-native-counts-read-lines ()
  (with-temp-buffer
    (agent-native-mode)
    (agent-native--tool-use "r1" "Read" '((file_path . "/x/a.txt")))
    (agent-native--tool-result "r1" "     1→alpha\n     2→beta\n     3→gamma\n\n<system-reminder>note</system-reminder>" nil)
    (should (string-match-p "Read 3 lines" (buffer-substring-no-properties (point-min) agent-native--input)))))

(ert-deftest agent-native-scrolling-keeps-the-input-at-the-bottom ()
  ;; Whatever moved the window (j into the prompt, G, C-f, zt, the wheel),
  ;; the layout check before each redisplay puts it back: the input on the
  ;; bottom row, and never blank space past the session's end.
  (with-temp-buffer
    (agent-native-mode)
    (agent-native--text-whole (mapconcat #'number-to-string (number-sequence 1 200) "\n"))
    (let ((window (selected-window)))
      (set-window-buffer window (current-buffer))
      (let ((bottom (car (agent-native--bottom window))))
        (should (> bottom (point-min)))
        ;; Cursor in the input, window scrolled past the end: pinned back.
        (goto-char (point-max))
        (set-window-start window (1- agent-native--spacer))
        (agent-native--before-redisplay window)
        (should (= (window-start window) bottom))
        ;; Cursor in the input, window scrolled up: pinned back too.
        (set-window-start window (point-min))
        (agent-native--before-redisplay window)
        (should (= (window-start window) bottom))
        ;; Reading up in the log: scrolling up is yours ...
        (goto-char (point-min))
        (set-window-start window (point-min))
        (agent-native--before-redisplay window)
        (should (= (window-start window) (point-min)))
        ;; ... but not past the end.
        (goto-char (+ bottom 3))
        (set-window-start window (1- agent-native--spacer))
        (agent-native--before-redisplay window)
        (should (= (window-start window) bottom))))))

(defun agent-native-test--png ()
  "A temp 1x1 PNG file."
  (let ((file (make-temp-file "cn-test-" nil ".png")))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert (base64-decode-string
               "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR4nGP4z8AAAAMBAQDJ/pLvAAAAAElFTkSuQmCC")))
    file))

(ert-deftest agent-native-attaches-files-to-the-prompt ()
  (with-temp-buffer
    (agent-native-mode)
    (let ((png (agent-native-test--png))
          (pdf (make-temp-file "cn-test-" nil ".pdf" "%PDF-1.4\n"))
          (txt (expand-file-name "notes.txt" default-directory)))
      (goto-char (point-max))
      (insert "look:")
      (agent-native-attach png)
      (agent-native-attach pdf)
      (should (equal (agent-native--input-text) "look: [Image #1] [PDF #2] "))
      ;; Each mark becomes a block before the text (Claude), or an input
      ;; item after it (Codex).
      (let* ((text (agent-native--input-text))
             (files (agent-native--attachments-in text))
             (content (agent-native-claude--content text files))
             (input (agent-native-codex--input text nil files)))
        (should (equal (mapcar #'car files) '(image pdf)))
        (should (vectorp content))
        (should (equal (mapcar (lambda (b) (alist-get 'type b)) content) '("image" "document" "text")))
        (should (equal (alist-get 'media_type (alist-get 'source (aref content 0))) "image/png"))
        ;; The echo of it reads as the prompt's text.
        (should (equal (agent-native-claude--prompt-text content) "look: [Image #1] [PDF #2] "))
        (should (equal (mapcar (lambda (i) (alist-get 'type i)) input) '("text" "localImage" "text")))
        (should (equal (alist-get 'path (aref input 1)) png))
        (should (equal (agent-native-codex--prompt-text `((content . ,input))) text)))
      ;; A deleted mark drops its file.
      (should (= 1 (length (agent-native--attachments-in "only [PDF #2]"))))
      (should-not (agent-native--attachments-in "no marks"))
      ;; Other files are @mentions, relative inside the session directory.
      (should (equal (agent-native--attach-mark txt) "@notes.txt"))
      (should (equal (agent-native--mention "/tmp/a b.txt") "@\"/tmp/a b.txt\""))
      ;; Interruptions come as text blocks, but are no prompt.
      (should-not (agent-native-claude--prompt-text [((type . "text") (text . "[Request interrupted by user]"))]))
      (delete-file png) (delete-file pdf))))

(ert-deftest agent-native-converts-heic-photos ()
  (skip-unless (executable-find "sips"))
  (with-temp-buffer
    (agent-native-mode)
    (let* ((png (agent-native-test--png))
           (heic (concat (file-name-sans-extension png) ".heic")))
      (call-process "sips" nil nil nil "-s" "format" "heic" png "--out" heic)
      (skip-unless (file-exists-p heic))
      (should (equal (agent-native--attach-mark heic) "[Image #1]"))
      (pcase-let ((`(,kind ,file ,media) (alist-get 1 agent-native--attachments)))
        (should (eq kind 'image))
        (should (equal media "image/jpeg"))
        (should (string-suffix-p ".jpg" file))
        (delete-file file))
      (delete-file png) (delete-file heic))))

(ert-deftest agent-native-notifies-when-a-hidden-turn-ends ()
  (let (said
        ;; Stubbing a primitive would native-compile a trampoline.
        (native-comp-enable-subr-trampolines nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (substring-no-properties (apply #'format fmt args)) said))))
      (with-temp-buffer
        (rename-buffer "*claude: ~/proj*")
        (let ((msg '(:text "All done.\nMore detail.")))
          (agent-native--notify msg)
          (should (equal (car said) "Claude finished in *claude: ~/proj*: All done."))
          (agent-native--notify '(:text "Boom" :error "Boom"))
          (should (string-prefix-p "Claude failed in" (car said)))
          (setq agent-native--backend 'codex)
          (agent-native--notify msg)
          (should (string-prefix-p "Codex finished in" (car said)))
          (setq agent-native--backend nil)
          ;; In sight: nothing.
          (setq said nil)
          (set-window-buffer (selected-window) (current-buffer))
          (agent-native--notify msg)
          (should-not said)
          (let ((agent-native-notify nil))
            (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
            (agent-native--notify msg)
            (should-not said)))))))

;; Headless claude leaves Remote Control to its host: the handshake says
;; what its settings ask for, a control request switches it.
(ert-deftest agent-native-starts-remote-control-as-claude-is-set-up ()
  (let (sent said
        (native-comp-enable-subr-trampolines nil))
    (cl-letf (((symbol-function 'agent-native-claude--control)
               (lambda (_conn subtype &optional fields callback)
                 (push (list subtype fields callback) sent)))
              ((symbol-function 'agent-native-live-p) (lambda (_conn) t))
              ((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?d "disconnect")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
      (with-temp-buffer
        (agent-native-mode)
        (setq agent-native--conn (agent-native-test--conn 'claude))
        (let* ((conn agent-native--conn)
               (request (lambda () (seq-find (lambda (it) (equal (car it) "remote_control")) sent)))
               (handshake (lambda (&rest info) (setq sent nil) (agent-native-claude--initialized conn info)))
               (answer (lambda (&rest response) (funcall (nth 2 (car sent)) response)))
               (state (lambda (&rest fields)
                        (agent-native-test--receive conn `((type . "system") (subtype . "bridge_state") ,@fields)))))
          ;; Claude's own setting decides.
          (funcall handshake '(remote_control_auto_enable . t))
          (should (equal (cadr (funcall request)) '((enabled . t))))
          (funcall handshake '(remote_control_auto_enable . :false))
          (should-not (funcall request))
          ;; Where it can never work, it is left alone.
          (funcall handshake '(remote_control_auto_enable . t) '(remote_control_available . :false))
          (should-not (funcall request))
          ;; The option overrules the setting, both ways.
          (let ((agent-native-remote-control nil))
            (funcall handshake '(remote_control_auto_enable . t))
            (should-not (funcall request)))
          (let ((agent-native-remote-control t))
            (funcall handshake)
            (should (funcall request)))
          ;; The answer brings the link; the label waits for the connection.
          (funcall (nth 2 (funcall request))
                   '((subtype . "success") (response . ((session_url . "https://claude.ai/code/session_x")))))
          (should (equal agent-native--remote-url "https://claude.ai/code/session_x"))
          (should-not (string-match-p "remote" (agent-native--label)))
          (funcall state '(state . "connected"))
          (should (string-match-p "remote" (agent-native--label)))
          (should (equal agent-native--remote-url "https://claude.ai/code/session_x"))
          ;; Lost: the label drops it, the echo area says why.
          (funcall state '(state . "failed") '(detail . "No network"))
          (should-not agent-native--remote)
          (should-not (string-match-p "remote" (agent-native--label)))
          (should (equal (car said) "Remote Control failed: No network"))
          ;; The command turns it on; a refusal is said.
          (agent-native-toggle-remote-control)
          (should (equal (cadr (car sent)) '((enabled . t))))
          (funcall answer '(subtype . "error") '(error . "Not signed in"))
          (should (equal (car said) "Remote Control: Not signed in"))
          (should-not agent-native--remote)
          ;; On, the command offers the link, and to turn it off.
          (agent-native-toggle-remote-control)
          (funcall answer '(subtype . "success") '(response . ((session_url . "https://claude.ai/code/session_y"))))
          (should (equal agent-native--remote-url "https://claude.ai/code/session_y"))
          (agent-native-toggle-remote-control)
          (should (equal (cadr (car sent)) '((enabled . :false))))
          (funcall answer '(subtype . "success"))
          (should-not agent-native--remote)
          (should-not agent-native--remote-url)
          ;; Codex has no Remote Control: the command says so.
          (setq agent-native--conn (agent-native-test--conn 'codex))
          (should-error (agent-native-toggle-remote-control) :type 'user-error))))))

;; Killed outright, claude cannot end its Remote Control session.
(ert-deftest agent-native-stop-lets-claude-sign-off ()
  (let* ((process (make-process :name "cn-test-sleep" :command '("sleep" "30")
                                :connection-type 'pipe :noquery t))
         heard
         (conn (agent-native-claude--make :process process :on-event #'ignore
                                          :on-exit (lambda (event) (push event heard)))))
    (agent-native-stop conn)
    ;; Over at once for its owner, who hears of it once.
    (should-not (agent-native-live-p conn))
    (should (equal heard '("stopped")))
    (agent-native-stop conn)
    (should (equal heard '("stopped")))
    ;; The process itself is asked to leave, not killed.
    (agent-native-test--wait (lambda () (not (process-live-p process))))
    (should (equal (list (process-status process) (process-exit-status process)) '(signal 15)))))

;;;; Bugs found 2026-11-17, kept fixed

(ert-deftest agent-native-merged-prompts-leave-the-queue ()
  ;; Prompts queued while Claude works come back joined when it takes
  ;; them up together; none may stay "queued" after.
  (with-temp-buffer
    (agent-native-mode)
    (setq agent-native--queued (list "Reply A" "Reply B" "Reply C"))
    (agent-native--prompt-taken "Reply A\nReply B")
    (should (equal agent-native--queued '("Reply C")))
    (agent-native--prompt-taken "Reply C")
    (should-not agent-native--queued)
    (should (equal (agent-native-test--transcript) "❯ Reply A\n  Reply B\n\n❯ Reply C\n\n \n❯ "))
    ;; One drawn when sent (Claude was idle), two queued behind it: each
    ;; is drawn once.
    (agent-native--set-busy nil)
    (setq agent-native--drawn (list "Count") agent-native--queued (list "Q2" "Q3"))
    (agent-native--prompt-taken "Count\nQ2\nQ3")
    (should-not (or agent-native--drawn agent-native--queued))
    (should (string-suffix-p "❯ Reply C\n\n❯ Q2\n  Q3\n\n \n❯ " (agent-native-test--transcript)))
    (agent-native--set-busy nil)))

(ert-deftest agent-native-filter-survives-a-bad-message ()
  (let* (got
         (conn (agent-native-claude--make
                :on-event (lambda (_type props)
                            (when (equal (plist-get props :text) "boom") (error "Boom"))
                            (push (plist-get props :text) got))))
         (line (lambda (text) (format "{\"type\":\"prompt_suggestion\",\"suggestion\":\"%s\"}\n" text)))
         (debug-on-error nil)
         (inhibit-message t))
    (agent-native--filter conn (concat (funcall line "a") (funcall line "boom") (funcall line "c")
                                       "{\"type\":\"prompt_suggestion\",\"suggestion\":\""))
    (should (equal (reverse got) '("a" "c")))
    ;; A long line in many pieces arrives whole.
    (dotimes (_ 50) (agent-native--filter conn "long"))
    (agent-native--filter conn "\"}\n")
    (should (equal (car got) (apply #'concat (make-list 50 "long"))))))

(ert-deftest agent-native-asks-the-next-permission-after-a-cancel ()
  (let (asked timers (native-comp-enable-subr-trampolines nil))
    (cl-letf (((symbol-function 'run-at-time) (lambda (_ _ f &rest args) (push (cons f args) timers)))
              ((symbol-function 'agent-native-backend-answer) #'ignore))
      (with-temp-buffer
        (agent-native-mode)
        (let* ((request (lambda (id) (list :id id :tool-id (concat "t" id) :name "Bash" :input '((command . "ls")))))
               (agent-native-permission-function
                (lambda (r)
                  (push (plist-get r :tool-id) asked)
                  ;; While r1 is asked: r2 arrives, then r1 is cancelled.
                  (when (equal (plist-get r :tool-id) "tr1")
                    (agent-native--request (funcall request "r2"))
                    (agent-native--drop-request "r1"))
                  'allow)))
          (agent-native--request (funcall request "r1"))
          (while timers (let ((timer (pop timers))) (apply (car timer) (cdr timer))))
          (should (equal (reverse asked) '("tr1" "tr2")))
          (should-not agent-native--pending))))))

(ert-deftest agent-native-draws-tricky-markdown ()
  (cl-flet ((visible (text)
              (with-temp-buffer
                (agent-native-mode)
                (let ((agent-native-format 'markdown))
                  (agent-native--text-whole text))
                (let ((s (buffer-substring (point-min) agent-native--input)) (out ""))
                  (dotimes (i (length s))
                    (unless (get-text-property i 'invisible s) (setq out (concat out (string (aref s i))))))
                  (string-trim (substring out 0 (string-search "\n\n \n❯" out)))))))
    ;; A code block that opens the reply: fences hidden, # kept.
    (should (equal (visible "```python\nx = 1\n# c\n```") "⏺ x = 1\n  # c"))
    ;; A longer fence holds a shorter one.
    (should (string-match-p "```js" (visible "````md\n```js\na\n```\n````")))
    ;; Stars in code-like text are not italics.
    (should (equal (visible "f(*args, **kwargs)") "⏺ f(*args, **kwargs)"))
    (should (equal (visible "Files *.el and *.py") "⏺ Files *.el and *.py"))
    (should (equal (visible "an *it* and *two*") "⏺ an it and two"))
    ;; Bold after a marker inside inline code.
    (should (equal (visible "`x**` foo **bar**") "⏺ x** foo bar"))
    ;; Org's own marks decide, not [[ or * in code and lists.
    (should (agent-native--markdown-p "**Bold**\n```bash\n[[ -f x ]]\n```"))
    (should (agent-native--markdown-p "**Steps**\n* one\n* two"))))

(ert-deftest agent-native-draws-awkward-results ()
  (with-temp-buffer
    (agent-native-mode)
    ;; An empty text block leaves no bullet.
    (agent-native--text-whole "\n\n")
    (agent-native--tool-use "t1" "Bash" '((command . "A")))
    (agent-native--tool-result "t1" (make-string 5000 ?x) nil)
    (agent-native--tool-use "t2" "Read" '((file_path . "/x.png")))
    (agent-native--tool-result "t2" [((type . "image"))] nil)
    (agent-native--tool-use "t3" "Bash" '((command . "B")))
    (agent-native--tool-result "t3" "a\r\nb\r\n" nil)
    (agent-native--tool-use "t4" "WebFetch" '((url)))
    (let ((text (agent-native-test--transcript)))
      (should (string-prefix-p "⏺ Bash(A)" text))
      (should (string-match-p "x… (4600 more characters)" text))
      (should (string-match-p "Read image" text))
      (should-not (string-match-p "\r" text))
      (should (string-match-p "⏺ WebFetch\n" text)))))

(ert-deftest agent-native-history-reads-real-shapes ()
  (let* ((dir (make-temp-file "cn-hist-" t))
         (file (expand-file-name "s.jsonl" dir))
         (agent-native-claude--chunk 100))
    (unwind-protect
        (progn
          (with-temp-file file
            ;; A big record before the first prompt, past the first piece.
            (insert (json-serialize `((type . "api-request-blob") (data . ,(make-string 500 ?z)))) "\n")
            (insert (json-serialize '((type . "user") (isCompactSummary . t)
                                      (message . ((role . "user") (content . "This session is being continued")))))
                    "\n")
            (insert (json-serialize '((type . "user") (message . ((role . "user") (content . "First prompt"))))) "\n")
            ;; A prompt sent while claude worked, taken into the turn.
            (insert (json-serialize '((type . "attachment")
                                      (attachment . ((type . "queued_command") (prompt . "Also this")
                                                     (commandMode . "prompt")))))
                    "\n")
            (insert (json-serialize '((type . "queue-operation") (operation . "remove") (content . "Also this"))) "\n")
            (insert (json-serialize '((type . "assistant")
                                      (message . ((content . [((type . "tool_use") (id . "b1") (name . "Bash")
                                                               (input . ((command . "rm x"))))])))))
                    "\n")
            (insert (json-serialize '((type . "user")
                                      (message . ((role . "user")
                                                  (content . [((type . "tool_result") (tool_use_id . "b1") (is_error . t)
                                                               (content . "The user doesn't want to proceed with this tool use. The tool use was rejected."))])))))
                    "\n")
            (insert (json-serialize '((type . "user")
                                      (message . ((role . "user")
                                                  (content . [((type . "text") (text . "[Request interrupted by user]"))])))))
                    "\n")
            (insert (json-serialize '((type . "system") (subtype . "compact_boundary") (content . "Conversation compacted")))))
          (should (equal (agent-native-claude--first-prompt file) "First prompt"))
          (with-temp-buffer
            (agent-native-mode)
            (agent-native-test--replay (agent-native--read-lines file))
            (let ((text (agent-native-test--transcript)))
              (should-not (string-match-p "being continued" text))
              (should (string-match-p "❯ First prompt\n\n❯ Also this\n\n⏺ Bash(rm x)\n  ⎿  Denied\n\n.*Interrupted" text))
              (should (= 1 (how-many "Also this" (point-min) (point-max))))
              (should (string-match-p "✻ Conversation compacted" text)))))
      (delete-directory dir t)))
  ;; An emoji is two UTF-16 units, so two dashes, as Claude Code names it.
  (should (string-suffix-p "-x--y" (agent-native-claude--sessions-dir "/tmp/x😀y"))))

(ert-deftest agent-native-usage-skips-unknown-limits ()
  (should (equal (mapcar #'car (agent-native-claude--usage-limits
                                '((five_hour . ((utilization)))
                                  (seven_day . ((utilization . 12))))))
                 '("Current week (all models)")))
  (should (equal (car (car (agent-native-claude--usage-limits
                            '((limits . [((kind . "weekly_scoped") (percent . 5)
                                          (scope . ((surface . "Claude Code"))))])))))
                 "Current week (Claude Code)")))

(ert-deftest agent-native-arrows-go-through-prompts ()
  (with-temp-buffer
    (agent-native-mode)
    (setq agent-native--history (list "two\nlines" "one"))
    (agent-native--set-input "draft")
    ;; Up on the input's only line: the newest prompt, the draft kept.
    (agent-native-up 1)
    (should (equal (agent-native--input-text) "two\nlines"))
    ;; Point is at its end: up moves within it first, then to older.
    (agent-native-up 1)
    (should (equal (agent-native--input-text) "two\nlines"))
    (should (<= (point) (+ agent-native--input (length "two"))))
    (agent-native-up 1)
    (should (equal (agent-native--input-text) "one"))
    (agent-native-up 1)
    (should (equal (agent-native--input-text) "one"))
    (agent-native-down 1)
    (should (equal (agent-native--input-text) "two\nlines"))
    (agent-native-down 1)
    (should (equal (agent-native--input-text) "draft"))
    ;; Not in history: down leaves the draft alone.
    (agent-native-down 1)
    (should (equal (agent-native--input-text) "draft"))))

(ert-deftest agent-native-spinner-stops-after-a-queued-slash-command ()
  ;; Claude Code never echoes a slash command back, so its turn has to
  ;; start at the previous turn's result, or the spinner never stops.
  (with-temp-buffer
    (agent-native-mode)
    (agent-native--set-busy t)
    (setq agent-native--queued (list "/cost"))
    (agent-native--turn-ended nil)
    (should agent-native--busy)
    (should-not agent-native--queued)
    (agent-native--turn-ended nil)
    (should-not agent-native--busy)
    (should-not agent-native--spinner)))

;;;; Codex

(defmacro agent-native-test--codex (&rest body)
  "Run BODY in a Codex session buffer with no process: `conn' is its
connection, `written' (newest first) what it sent, `receive' hands it
messages. Its thread is \"th\", ready for prompts."
  (declare (indent 0))
  `(with-temp-buffer
     (agent-native-mode)
     (setq default-directory "/tmp/sb/")
     (let* ((conn (agent-native-test--conn 'codex))
            (written nil)
            (native-comp-enable-subr-trampolines nil)
            (receive (lambda (&rest messages) (apply #'agent-native-test--receive conn messages))))
       (setq agent-native--conn conn)
       (setf (agent-native-codex-thread conn) "th"
             (agent-native-codex-ready conn) t
             (agent-native-codex-model conn) "gpt-6-luna"
             (agent-native-codex-effort conn) "low")
       (cl-letf (((symbol-function 'agent-native--write) (lambda (_conn object) (push object written)))
                 ((symbol-function 'agent-native-live-p) (lambda (_conn) t)))
         ,@body))))

(defun agent-native-test--note (method &rest params)
  "A notification METHOD about thread \"th\" with PARAMS (cons cells)."
  `((method . ,method) (params . ((threadId . "th") ,@params))))

(defun agent-native-test--item (phase item)
  "ITEM started or completed (PHASE) in thread \"th\"."
  (agent-native-test--note (format "item/%s" phase) `(item . ,item)))

(defun agent-native-test--sent (written method)
  "The params of the newest request METHOD in WRITTEN."
  (alist-get 'params (seq-find (lambda (o) (equal (alist-get 'method o) method)) written)))

(ert-deftest agent-native-draws-a-codex-turn ()
  (agent-native-test--codex
    (let ((agent-native-permission-function (lambda (_) 'allow))
          (shell '((type . "commandExecution") (id . "c1") (command . "/bin/zsh -lc 'echo hello'")
                   (commandActions . [((type . "unknown") (command . "echo hello"))]) (status . "inProgress"))))
      (agent-native-test--type-and-send "Make notes")
      (should (equal (alist-get 'text (aref (alist-get 'input (agent-native-test--sent written "turn/start")) 0))
                     "Make notes"))
      (funcall receive
               (agent-native-test--note "turn/started" '(turn . ((id . "t1"))))
               (agent-native-test--item "completed" '((type . "userMessage") (id . "u1")
                                                      (content . [((type . "text") (text . "Make notes"))])))
               (agent-native-test--item "started" '((type . "agentMessage") (id . "m1") (text . "")))
               (agent-native-test--note "item/agentMessage/delta" '(itemId . "m1") '(delta . "On "))
               (agent-native-test--note "item/agentMessage/delta" '(itemId . "m1") '(delta . "it."))
               (agent-native-test--item "completed" '((type . "agentMessage") (id . "m1") (text . "On it.")))
               (agent-native-test--item "started" shell)
               '((id . 0) (method . "item/commandExecution/requestApproval")
                 (params . ((threadId . "th") (itemId . "c1") (command . "/bin/zsh -lc 'echo hello'")
                            (availableDecisions . ["accept" "cancel"])))))
      ;; Asked as Claude's are: a waiting note, then the answer.
      (should (string-match-p "Waiting for permission" (agent-native-test--transcript)))
      (agent-native-answer-pending t)
      (should (equal (car written) '((id . 0) (result . ((decision . "accept"))))))
      (should-not (string-match-p "Waiting for permission" (agent-native-test--transcript)))
      (funcall receive
               (agent-native-test--note "serverRequest/resolved" '(requestId . 0))
               (agent-native-test--item "completed" `(,@shell (aggregatedOutput . "hello\n") (exitCode . 0)
                                                      (status . "completed")))
               (agent-native-test--item "started" '((type . "fileChange") (id . "f1") (status . "inProgress")
                                                    (changes . [((path . "/tmp/sb/notes.txt") (kind . ((type . "add")))
                                                                 (diff . "hi\n"))])))
               (agent-native-test--item "completed" '((type . "fileChange") (id . "f1") (status . "completed")
                                                      (changes . [((path . "/tmp/sb/notes.txt") (kind . ((type . "add")))
                                                                   (diff . "hi\n"))])))
               (agent-native-test--item "completed" '((type . "fileChange") (id . "f2") (status . "completed")
                                                      (changes . [((path . "/tmp/sb/notes.txt") (kind . ((type . "update")))
                                                                   (diff . "@@ -1 +1 @@\n-hi\n+hello\n"))])))
               (agent-native-test--item "completed" '((type . "commandExecution") (id . "r1") (status . "completed")
                                                      (command . "/bin/zsh -lc 'cat notes.txt'")
                                                      (commandActions . [((type . "read") (command . "cat notes.txt")
                                                                          (name . "notes.txt") (path . "notes.txt"))])
                                                      (aggregatedOutput . "hello\n") (exitCode . 0)))
               (agent-native-test--note "turn/plan/updated"
                                        '(plan . [((step . "Write notes") (status . "completed"))
                                                  ((step . "Check them") (status . "inProgress"))]))
               (agent-native-test--item "completed" '((type . "agentMessage") (id . "m2") (text . "Done.")))
               (agent-native-test--note "turn/completed" '(turn . ((id . "t1") (status . "completed") (durationMs . 3200)))))
      (let ((text (agent-native-test--transcript)))
        (should (string-match-p "^❯ Make notes" text))
        (should (string-match-p "^⏺ On it\\.$" text))
        (should (string-match-p "⏺ Shell(echo hello)\n  ⎿  hello" text))
        (should (string-match-p "⏺ Write(notes.txt)\n  ⎿  Wrote 1 line to notes.txt" text))
        (should (string-match-p "⏺ Edit(notes.txt)\n  ⎿  - hi\n     \\+ hello" text))
        (should (string-match-p "⏺ Read(notes.txt)\n  ⎿  Read 1 line" text))
        (should (string-match-p "⏺ Update Todos\n  ⎿  ☒ Write notes\n     ◐ Check them" text))
        (should (string-match-p "^⏺ Done\\.$" text))
        (should (string-match-p "✻ Worked for 3s" text))
        ;; Drawn once, though Codex echoes it.
        (should (= 1 (how-many "❯ Make notes" (point-min) (point-max)))))
      (should-not agent-native--busy)
      ;; After the turn: a guess at the next prompt is asked for.
      (should (equal (agent-native-test--sent written "thread/prediction/request")
                     '((threadId . "th") (sourceTurnId . "t1"))))
      (funcall receive (agent-native-test--note "thread/prediction/updated"
                                                '(result . ((type . "completed") (text . "Run the tests")))))
      (should (equal agent-native--suggestion "Run the tests")))))

(ert-deftest agent-native-codex-denies-and-interrupts ()
  (agent-native-test--codex
    (let ((agent-native-permission-function (lambda (_) 'deny)))
      (agent-native-test--type-and-send "Delete it")
      (funcall receive
               (agent-native-test--note "turn/started" '(turn . ((id . "t1"))))
               (agent-native-test--item "started" '((type . "commandExecution") (id . "c1") (command . "rm x")
                                                    (status . "inProgress")))
               '((id . 7) (method . "item/commandExecution/requestApproval")
                 (params . ((threadId . "th") (itemId . "c1")
                            (availableDecisions . ["accept" ((acceptWithExecpolicyAmendment . ((execpolicy_amendment . ["rm" "x"]))))
                                                   "cancel"])))))
      ;; No: Codex is told to stop the turn, as Claude is.
      (agent-native-answer-pending t)
      (should (equal (car written) '((id . 7) (result . ((decision . "cancel"))))))
      (funcall receive
               (agent-native-test--item "completed" '((type . "commandExecution") (id . "c1") (command . "rm x")
                                                      (status . "declined")))
               (agent-native-test--note "turn/completed" '(turn . ((id . "t1") (status . "interrupted") (durationMs . 9000)))))
      (let ((text (agent-native-test--transcript)))
        (should (string-match-p "⏺ Shell(rm x)\n  ⎿  Denied" text))
        (should-not (string-match-p "Declined\\|Worked for" text))
        (should (string-match-p "Interrupted · What should Codex do instead\\?" text)))
      ;; "Always" takes the rule Codex offers when it can't remember for the session.
      (should (equal (agent-native-codex--always '("accept" ((acceptWithExecpolicyAmendment . x)) "cancel"))
                     '((acceptWithExecpolicyAmendment . x))))
      (should (equal (agent-native-codex--always '("accept" "acceptForSession")) "acceptForSession"))
      ;; C-c C-c before Codex said which turn runs: sent once it does.
      (agent-native-test--type-and-send "Again")
      (agent-native-interrupt)
      (should-not (agent-native-test--sent written "turn/interrupt"))
      (funcall receive (agent-native-test--note "turn/started" '(turn . ((id . "t2")))))
      (should (equal (agent-native-test--sent written "turn/interrupt") '((threadId . "th") (turnId . "t2"))))
      (funcall receive (agent-native-test--note "turn/completed" '(turn . ((id . "t2") (status . "interrupted")))))
      ;; C-c C-c, then the turn never starts: the next one is not stopped.
      (agent-native-test--type-and-send "Once more")
      (agent-native-interrupt)
      (funcall receive `((id . ,(alist-get 'id (car written))) (error . ((message . "Overloaded")))))
      (should-not agent-native--busy)
      (agent-native-test--type-and-send "And again")
      (setq written nil)
      (funcall receive (agent-native-test--note "turn/started" '(turn . ((id . "t3")))))
      (should-not (agent-native-test--sent written "turn/interrupt")))))

(ert-deftest agent-native-codex-model-effort-and-mode-go-with-the-next-turn ()
  (agent-native-test--codex
    (setf (agent-native-codex-models conn)
          (list (list :value "gpt-6-sol" :aliases '("gpt-6-sol") :default-effort "medium" :efforts '("low" "medium"))
                (list :value "gpt-6-max" :aliases '("gpt-6-max") :default-effort "high" :efforts '("high" "xhigh"))))
    (setq agent-native--models (agent-native-codex-models conn))
    ;; Shown at once, sent with the next turn. The effort stays when the
    ;; model has it, as with Claude; else it is the model's own.
    (agent-native-set-model "gpt-6-sol")
    (should (equal (list agent-native--model agent-native--effort) '("gpt-6-sol" "low")))
    (agent-native-set-model "gpt-6-max")
    (should (equal (list agent-native--model agent-native--effort) '("gpt-6-max" "high")))
    (agent-native-set-model "gpt-6-sol")
    (should (equal (list agent-native--model agent-native--effort) '("gpt-6-sol" "medium")))
    (should-not written)
    (setq agent-native--mode 'default)
    (agent-native-cycle-mode)
    (agent-native-cycle-mode)
    (should (eq agent-native--mode 'plan))
    (should (string-match-p "plan mode on" (agent-native--status-line)))
    (agent-native-test--type-and-send "Plan it")
    (let ((sent (agent-native-test--sent written "turn/start")))
      (should (equal (alist-get 'model sent) "gpt-6-sol"))
      (should (equal (alist-get 'effort sent) "medium"))
      (should (equal (alist-get 'approvalPolicy sent) "on-request"))
      (should (equal (alist-get 'sandboxPolicy sent) '((type . "readOnly"))))
      (should (equal (alist-get 'mode (alist-get 'collaborationMode sent)) "plan")))
    ;; Sent once: the turn after has none of it.
    (setf (agent-native-codex-turn conn) nil)
    (agent-native-test--type-and-send "Go on")
    (should-not (alist-get 'model (agent-native-test--sent written "turn/start")))
    ;; A prompt sent while a turn runs joins it; settings wait for the next.
    (setf (agent-native-codex-turn conn) "t9")
    (agent-native-set-effort "low")
    (agent-native-test--type-and-send "Also this")
    (should-not (alist-get 'effort (agent-native-test--sent written "turn/start")))
    (setf (agent-native-codex-turn conn) nil)
    (agent-native-test--type-and-send "Next")
    (should (equal (alist-get 'effort (agent-native-test--sent written "turn/start")) "low"))
    ;; /model NAME and /compact.
    (agent-native-test--type-and-send "/model gpt-6-sol")
    (should (equal agent-native--model "gpt-6-sol"))
    (agent-native-test--type-and-send "/compact")
    (should (equal (agent-native-test--sent written "thread/compact/start") '((threadId . "th"))))))

(ert-deftest agent-native-codex-sub-agents ()
  (agent-native-test--codex
    (agent-native-test--type-and-send "Spawn one")
    (funcall receive
             (agent-native-test--note "turn/started" '(turn . ((id . "t1"))))
             (agent-native-test--item "started" '((type . "subAgentActivity") (id . "call_1") (kind . "started")
                                                  (agentThreadId . "child") (agentPath . "/root/echo")
                                                  (model . "gpt-6.1-terra") (reasoningEffort . "low")))
             ;; The sub-agent's own thread: steps, never the transcript.
             '((method . "item/started")
               (params . ((threadId . "child")
                          (item . ((type . "commandExecution") (id . "x1") (command . "echo sub-hello")))))))
    (should (equal (mapcar (lambda (a) (plist-get a :name)) agent-native--agents) '("echo")))
    (should (equal (plist-get (car agent-native--agents) :model) "gpt-6.1-terra"))
    (funcall receive
             '((method . "thread/tokenUsage/updated")
               (params . ((threadId . "child") (tokenUsage . ((total . ((totalTokens . 24259))))))))
             '((method . "item/completed")
               (params . ((threadId . "child")
                          (item . ((type . "agentMessage") (id . "x2") (text . "The output was sub-hello")))))))
    (should (equal (plist-get (car agent-native--agents) :tokens) 24259))
    (should (equal (plist-get (car agent-native--agents) :step) "Shell(echo sub-hello)"))
    (let ((text (agent-native-test--transcript)))
      (should (string-match-p "⏺ Agent(echo)\n  ⎿  Shell(echo sub-hello) · 1 tool use" text))
      (should-not (string-match-p "The output was" text)))
    (funcall receive (agent-native-test--item "completed" '((type . "subAgentActivity") (id . "done_1") (kind . "completed")
                                                            (agentThreadId . "child"))))
    (should-not agent-native--agents)
    (should (string-match-p "⏺ Agent(echo)\n  ⎿  Done (1 tool use)" (agent-native-test--transcript)))))

(ert-deftest agent-native-codex-draws-a-resumed-session ()
  (agent-native-test--codex
    (setf (agent-native-codex-ready conn) nil)
    (agent-native--emit conn 'history-start)
    (agent-native-codex--load-history conn nil)
    ;; The turns, as thread/turns/list gives them.
    (funcall receive
             `((id . ,(alist-get 'id (car written)))
               (result
                . ((data
                    . [((id . "t1") (status . "completed")
                        (items . [((type . "userMessage") (id . "u1") (content . [((type . "text") (text . "First"))]))
                                  ((type . "commandExecution") (id . "c1") (command . "ls") (status . "completed")
                                   (aggregatedOutput . "a.txt\n") (exitCode . 0))
                                  ((type . "agentMessage") (id . "m1") (text . "One file."))]))
                       ((id . "t2") (status . "interrupted")
                        (items . [((type . "userMessage") (id . "u2") (content . [((type . "text") (text . "Second"))]))]))
                       ((id . "t3") (status . "completed")
                        (items . [((type . "contextCompaction") (id . "k1"))]))])))))
    (let ((text (agent-native-test--transcript)))
      (should (string-match-p "❯ First\n\n⏺ Shell(ls)\n  ⎿  a.txt\n\n⏺ One file\\.\n\n❯ Second" text))
      ;; An interrupted turn says so, as Claude's do.
      (should (string-match-p "❯ Second\n+ *⎿  Interrupted · What should Codex do instead\\?" text))
      (should (string-match-p "Conversation compacted\n\n↑ resumed" text)))
    (should (agent-native-codex-ready conn))
    (should-not agent-native--busy)
    (agent-native-previous-prompt 1)
    (should (equal (agent-native--input-text) "Second"))))

(ert-deftest agent-native-codex-usage ()
  (let ((text (substring-no-properties
               (agent-native--usage-text
                (agent-native-codex--usage-report
                 '((rateLimitsByLimitId
                    . ((codex . ((limitId . "codex")
                                 (primary . ((usedPercent . 7) (windowDurationMins . 10080) (resetsAt . 1794997482)))
                                 (credits . ((hasCredits . t) (unlimited . :false) (balance . "62500.0000000000")))))))
                   (rateLimits . ((credits . ((hasCredits . t) (unlimited . :false) (balance . "62500.0000000000"))))))
                 '((total . ((totalTokens . 46986) (cachedInputTokens . 36608)))
                   (last . ((totalTokens . 23514)))
                   (modelContextWindow . 475000))
                 `((summary . ((currentStreakDays . 24) (peakDailyTokens . 289593809)))
                   (dailyUsageBuckets . [((startDate . ,(format-time-string "%F" nil t)) (tokens . 1200000))
                                         ((startDate . "2001-01-01") (tokens . 5))])))))))
    (should (string-match-p "Current week +█░+   7%  resets " text))
    (should (string-match-p "Context +█░+   5%  24k of 475k tokens" text))
    (should (string-match-p "Session +47k tokens · 37k cached · 62500 credits left" text))
    (should (string-match-p "Last 7d · 1.2M tokens · 24-day streak" text))
    (should (string-match-p "Today 1.2M tokens" text))
    (should (string-match-p "Peak day 290M tokens" text))))

(ert-deftest agent-native-codex-unwraps-commands ()
  (should (equal (agent-native-codex--unwrap "/bin/zsh -lc 'echo hello'") "echo hello"))
  (should (equal (agent-native-codex--unwrap "/bin/zsh -lc 'echo '\\''hi'\\'''") "echo 'hi'"))
  (should (equal (agent-native-codex--unwrap "/bin/bash -c \"printf \\\"a\\\" > f\"") "printf \"a\" > f"))
  (should (equal (agent-native-codex--unwrap "/bin/zsh -lc 'a\nb'") "a\nb"))
  (should (equal (agent-native-codex--unwrap "ls -la") "ls -la")))

(ert-deftest agent-native-replays-a-codex-session ()
  (let ((recording (expand-file-name "dev/recordings/codex1.jsonl" agent-native-test--root)))
    ;; Recordings hold account data, so they stay out of git: make one with
    ;; dev/record-codex.py to run this.
    (skip-unless (file-exists-p recording))
    (let* ((cwd (file-name-as-directory (agent-native-test--recorded-cwd recording)))
           (made (not (file-directory-p cwd)))
           (process-environment (cons (concat "AGENT_NATIVE_REPLAY=" recording) process-environment))
           (agent-native-codex-program (expand-file-name "dev/fake-codex.py" agent-native-test--root))
           (agent-native-permission-function (lambda (_) 'allow))
           (agent-native-permission-mode 'default)
           (buffer (get-buffer-create "*agent-native-codex-test*")))
      (make-directory cwd t)
      (unwind-protect
          (with-current-buffer buffer
            (setq default-directory cwd)
            (agent-native--open 'codex)
            (agent-native-test--wait (lambda () (and agent-native--models (assoc "compact" agent-native--commands))))
            (should (string-match-p "✻ Codex  ·" (agent-native-test--transcript)))
            (should (member "gpt-6-luna" (mapcar (lambda (m) (plist-get m :value)) agent-native--models)))
            (agent-native-test--type-and-send
             (concat "Run `echo hello` in the shell. Then use the apply_patch tool (not the shell) to create notes.txt "
                     "containing the line hi. Then use apply_patch to change hi to hello. Then reply with one short sentence."))
            (agent-native-test--wait (lambda () (and (not agent-native--busy)
                                                     (string-match-p "Worked for" (agent-native-test--transcript)))))
            (let ((text (agent-native-test--transcript)))
              (should (= 1 (how-many "^❯ Run `echo hello`" (point-min) (point-max))))
              (should (string-match-p "⏺ Shell(echo hello)\n  ⎿  hello" text))
              (should (string-match-p "⏺ Write(notes.txt)\n  ⎿  Wrote 1 line to notes.txt" text))
              (should (string-match-p "⏺ Edit(notes.txt)\n  ⎿  - hi\n     \\+ hello" text))
              (should (string-match-p "^⏺ Ran `echo hello`" text))
              (should-not (string-match-p "Waiting for permission\\|Error" text)))
            ;; The model and effort the thread runs with, as with Claude.
            (should (string-match-p "gpt-6-luna · ◐ low"
                                    (substring-no-properties (overlay-get agent-native--label-overlay 'after-string))))
            ;; The same /usage, from Codex's numbers.
            (agent-native-test--type-and-send "/usage")
            (agent-native-test--wait (lambda () (get-buffer "*codex usage*")))
            (let ((usage (with-current-buffer "*codex usage*" (buffer-substring-no-properties (point-min) (point-max)))))
              (kill-buffer "*codex usage*")
              (should (string-match-p "Current week +[█░]+ +[0-9]+%  resets" usage))
              (should (string-match-p "Context +[█░]+ +[0-9]+%  [0-9.]+k of [0-9.]+k tokens" usage))
              (should (string-match-p "Session +[0-9.]+k tokens" usage))
              (should (string-match-p "Last 7d · " usage)))
            ;; An interrupted turn.
            (agent-native-test--type-and-send "Run `sleep 30` in the shell, then say done.")
            (agent-native-test--wait (lambda () (string-match-p "Shell(sleep 30)" (agent-native-test--transcript))))
            (agent-native-interrupt)
            (agent-native-test--wait (lambda () (not agent-native--busy)))
            (should (string-match-p "Interrupted · What should Codex do instead\\?" (agent-native-test--transcript)))
            (should (equal (agent-native--input-text) "")))
        (with-current-buffer buffer (agent-native-stop agent-native--conn))
        (kill-buffer buffer)
        (when made (delete-directory cwd t))))))

(ert-deftest agent-native-chooses-provider-then-model-then-effort ()
  (let* ((answers nil) (asked nil)
         (native-comp-enable-subr-trampolines nil)
         (claude-models (list (list :value "opus" :aliases '("opus") :efforts '("low" "high"))))
         (codex-models (list (list :value "gpt-6-sol" :aliases '("gpt-6-sol") :default-effort "medium"
                                   :efforts '("low" "medium"))))
         (codex-buffer (generate-new-buffer " *codex test*"))
         written visited)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest args)
                 (push (list prompt (all-completions "" collection) (nth 4 args)) asked)
                 (pop answers)))
              ((symbol-function 'agent-native-backend-available-p) (lambda (_) t))
              ((symbol-function 'agent-native-live-p) (lambda (conn) (and conn t)))
              ((symbol-function 'agent-native--write) (lambda (_conn object) (push object written)))
              ((symbol-function 'agent-native-claude--control) #'ignore)
              ((symbol-function 'agent-native--visit)
               (lambda (backend _directory) (setq visited backend) (set-buffer codex-buffer))))
      (unwind-protect
          (with-temp-buffer
            (agent-native-mode)
            (setq agent-native--conn (agent-native-test--conn 'claude)
                  agent-native--models claude-models
                  agent-native--model "opus" agent-native--effort "high")
            (with-current-buffer codex-buffer
              (agent-native-mode)
              (setq agent-native--conn (agent-native-test--conn 'codex)
                    agent-native--models codex-models)
              (setf (agent-native-codex-models agent-native--conn) codex-models))
            ;; The session's own provider: listed first, its model changes here.
            (setq answers (list "Claude" "opus" "low"))
            (agent-native-choose-model)
            (should-not visited)
            (should (equal (mapcar #'car (reverse asked)) '("Provider: " "Model: " "Effort for opus: ")))
            (should (equal (nth 1 (car (last asked))) '("Claude" "OpenAI")))
            ;; Defaults: this session's model and effort.
            (should (equal (nth 2 (nth 1 (reverse asked))) "opus"))
            (should (equal (nth 2 (car asked)) "high"))
            ;; The other provider: its session, with the model set there.
            (let ((claude-buffer (current-buffer)))
              (setq answers (list "OpenAI" "gpt-6-sol" "medium") asked nil)
              (agent-native-choose-model)
              (should (eq visited 'codex))
              (should (eq (current-buffer) codex-buffer))
              (should (equal (list agent-native--model agent-native--effort) '("gpt-6-sol" "medium")))
              (should (equal (alist-get 'model (agent-native-codex-overrides agent-native--conn)) "gpt-6-sol"))
              ;; Its effort default is the model's own.
              (should (equal (nth 2 (car asked)) "medium"))
              ;; The Claude session is left as it was.
              (should (equal (buffer-local-value 'agent-native--model claude-buffer) "opus"))
              ;; No process to stop when the buffers go.
              (setq agent-native--conn nil)
              (with-current-buffer claude-buffer (setq agent-native--conn nil))))
        (with-current-buffer codex-buffer (setq agent-native--conn nil))
        (kill-buffer codex-buffer)))))
