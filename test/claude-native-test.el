;;; claude-native-test.el --- replay real sessions through the UI -*- lexical-binding: t -*-

;; Each test runs a session buffer against dev/fake-claude.py, which replays
;; a recording of a real claude session (dev/record.py), and checks what the
;; buffer shows. Run: emacs --batch -L . -l test/claude-native-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'claude-native)

(defconst claude-native-test--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defconst claude-native-test--sandbox
  (file-name-as-directory (or (getenv "CN_SANDBOX") (expand-file-name "cn-sandbox" temporary-file-directory)))
  "Where the recording ran; tool paths in it are under this.
Set CN_SANDBOX to the directory dev/record.py recorded in.")

(defun claude-native-test--wait (predicate &optional seconds)
  "Process output until PREDICATE holds, or fail after SECONDS."
  (let ((end (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) end))
      (accept-process-output nil 0.05))
    (should (funcall predicate))))

(defmacro claude-native-test--session (recording &rest body)
  "Run BODY in a session buffer replaying RECORDING."
  (declare (indent 1))
  ;; Recordings hold account data, so they stay out of git: make one with
  ;; dev/record.py to run these tests.
  `(let* ((_ (skip-unless (file-exists-p (expand-file-name ,recording claude-native-test--root))))
          (process-environment (cons (concat "CLAUDE_NATIVE_REPLAY=" (expand-file-name ,recording claude-native-test--root))
                                     process-environment))
          (claude-native-program (expand-file-name "dev/fake-claude.py" claude-native-test--root))
          (claude-native-permission-function (lambda (_) 'allow))
          (default-directory claude-native-test--sandbox)
          (buffer (get-buffer-create "*claude-native-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (setq default-directory claude-native-test--sandbox)
           (claude-native--open)
           ,@body)
       (with-current-buffer buffer (claude-native-stop claude-native--conn))
       (kill-buffer buffer))))

(defun claude-native-test--type-and-send (text)
  (goto-char (point-max))
  (insert text)
  (claude-native-send))

(defun claude-native-test--transcript ()
  (buffer-substring-no-properties (point-min) claude-native--input))

(ert-deftest claude-native-replays-a-session ()
  (claude-native-test--session "dev/recordings/session1.jsonl"
    ;; Handshake: commands arrive.
    (claude-native-test--wait (lambda () claude-native--commands))
    (should (assoc "compact" claude-native--commands))
    ;; Turn 1: Write, Edit (permission granted), Bash, then text.
    (claude-native-test--type-and-send "Use the Write tool to create notes.txt")
    (claude-native-test--wait (lambda () (string-match-p "printed" (claude-native-test--transcript))))
    (claude-native-test--wait (lambda () (not claude-native--busy)))
    (let ((text (claude-native-test--transcript)))
      (should (string-match-p "^❯ Use the Write tool" text))
      (should (string-match-p "⏺ Write(notes.txt)" text))
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
    (should (equal claude-native--model "claude-haiku-5-5"))
    ;; Model and effort show at the right end of the rule above the input.
    (claude-native-test--wait (lambda () (equal claude-native--effort "medium")))
    (should (string-match-p "haiku-5-5 · ◐ medium"
                            (substring-no-properties (overlay-get claude-native--label-overlay 'after-string))))
    (should (> claude-native--cost 0))
    ;; The input area is empty and still editable.
    (should (equal (claude-native--input-text) ""))
    ;; Permission mode, then an interrupted turn.
    (claude-native-cycle-mode)
    (claude-native-test--wait (lambda () (equal claude-native--mode "acceptEdits")))
    (claude-native-test--type-and-send "Think step by step")
    (claude-native-test--wait (lambda () (> claude-native--thinking-tokens 0)))
    (claude-native-interrupt)
    (claude-native-test--wait (lambda () (string-match-p "Interrupted" (claude-native-test--transcript))))
    (claude-native-test--wait (lambda () (not claude-native--busy)))
    ;; The interrupted turn ends as an error result; only "Interrupted" shows.
    (should-not (string-match-p "error_during_execution" (claude-native-test--transcript)))
    ;; A slash command's output arrives whole, not streamed.
    (claude-native-test--type-and-send "/cost")
    (claude-native-test--wait (lambda () (string-match-p "subscription" (claude-native-test--transcript))))))

(ert-deftest claude-native-transcript-is-read-only ()
  (claude-native-test--session "dev/recordings/session1.jsonl"
    (claude-native-test--wait (lambda () claude-native--commands))
    (goto-char (point-min))
    (should-error (insert "x") :type 'text-read-only)
    (goto-char (point-max))
    (insert "typed")
    (should (equal (claude-native--input-text) "typed"))))

(provide 'claude-native-test)

(ert-deftest claude-native-completes-commands-and-files ()
  (claude-native-test--session "dev/recordings/session1.jsonl"
    (claude-native-test--wait (lambda () claude-native--commands))
    (goto-char (point-max))
    (insert "/compa")
    (let ((capf (claude-native--complete)))
      (should capf)
      (should (member "compact" (all-completions "compa" (nth 2 capf)))))
    ;; The file to complete: made here, as temp dirs get cleaned.
    (make-directory claude-native-test--sandbox t)
    (with-temp-file (expand-file-name "notes.txt" claude-native-test--sandbox))
    (claude-native--set-input "look at @no")
    (goto-char (point-max))
    (let ((capf (claude-native--complete)))
      (should capf)
      (should (member "notes.txt" (all-completions "no" (nth 2 capf)))))
    ;; A slash later in the text is no command.
    (claude-native--set-input "see a/b")
    (goto-char (point-max))
    (should-not (claude-native--complete))))

(defconst claude-native-test--live
  (file-name-as-directory (or (getenv "CN_LIVE") (expand-file-name "cn-live" temporary-file-directory)))
  "A directory with real sessions from manual testing (set CN_LIVE).")

(ert-deftest claude-native-lists-and-draws-past-sessions ()
  (skip-unless (file-directory-p (claude-native--sessions-dir claude-native-test--live)))
  (let ((sessions (claude-native--sessions claude-native-test--live)))
    (should sessions)
    ;; Newest first, each with its first prompt.
    (should (cl-every (lambda (s) (stringp (plist-get s :prompt))) sessions))
    (should (time-less-p (plist-get (car (last sessions)) :time) (plist-get (car sessions) :time)))
    (let* ((with-apple (seq-find (lambda (s) (let ((case-fold-search nil))
                                                  (string-match-p "APPLE BANANA" (plist-get s :prompt))))
                                 sessions))
           (file (claude-native--session-file claude-native-test--live (plist-get with-apple :id))))
      (with-temp-buffer
        (setq default-directory claude-native-test--live)
        (claude-native-mode)
        (claude-native--draw-history file)
        (let ((text (buffer-substring-no-properties (point-min) claude-native--input)))
          (should (string-match-p "^❯ Reply with exactly: APPLE BANANA" text))
          (should (string-match-p "^⏺ APPLE BANANA" text)))))))

(ert-deftest claude-native-knows-native-commands ()
  (should (eq (claude-native--native-command "/resume") 'claude-native-resume))
  (should (eq (claude-native--native-command "/model ") 'claude-native-set-model))
  (should (eq (claude-native--native-command "/effort") 'claude-native-set-effort))
  (should-not (claude-native--native-command "/effort high"))
  (should-not (claude-native--native-command "/compact"))
  (should-not (claude-native--native-command "please /resume")))

(defun claude-native-test--visible (start end)
  "The text from START to END that is not invisible."
  (let ((out ""))
    (dotimes (i (- end start))
      (let ((p (+ start i)))
        (unless (get-text-property p 'invisible) (setq out (concat out (string (char-after p)))))))
    out))

(ert-deftest claude-native-renders-markdown-like-the-terminal ()
  (with-temp-buffer
    (claude-native-mode)
    (claude-native--text-whole "# Title\nSome **bold**, *it* and `code`.\n```\nraw **kept** `x`\n```")
    (let ((shown (claude-native-test--visible (point-min) claude-native--out)))
      (should (string-match-p "^⏺ Title$" shown))
      (should (string-match-p "Some bold, it and code\\." shown))
      ;; Code blocks keep their text, lose their fences.
      (should (string-match-p "raw \\*\\*kept\\*\\* `x`" shown))
      (should-not (string-match-p "```" shown)))))

(ert-deftest claude-native-expands-clipped-results ()
  (with-temp-buffer
    (claude-native-mode)
    (claude-native--tool-use "t1" "Bash" '((command . "seq 1 10")))
    (claude-native--tool-result "t1" (mapconcat #'number-to-string (number-sequence 1 10) "\n") nil)
    (let ((text (buffer-substring-no-properties (point-min) claude-native--input)))
      (should (string-match-p "… \\+6 lines" text))
      (should-not (string-match-p "^ +10$" text)))
    (goto-char (point-min))
    (search-forward "… +6")
    (claude-native-send)
    (let ((text (buffer-substring-no-properties (point-min) claude-native--input)))
      (should-not (string-match-p "lines (RET" text))
      (should (string-match-p "^ +10$" text)))))

(ert-deftest claude-native-undo-leaves-the-transcript-alone ()
  (with-temp-buffer
    (claude-native-mode)
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (goto-char (point-max))
    (insert "abc")
    (undo-boundary)
    ;; Claude writes while you type: above the input.
    (claude-native--text-whole "Claude said this.")
    (undo-boundary)
    (goto-char (point-max))
    (let ((last-command nil)) (undo))
    (should (equal (claude-native--input-text) ""))
    (should (string-match-p "Claude said this\\."
                            (buffer-substring-no-properties (point-min) claude-native--input)))))

(ert-deftest claude-native-italics-leave-code-and-maths-alone ()
  (with-temp-buffer
    (claude-native-mode)
    (claude-native--text-whole "Match `*.el` and `*.py`; 2*3*4 and *it* here.")
    (should (equal (claude-native-test--visible (point-min) claude-native--out)
                   "⏺ Match *.el and *.py; 2*3*4 and it here."))))

(ert-deftest claude-native-shows-questions-and-answers ()
  (with-temp-buffer
    (claude-native-mode)
    (claude-native--tool-use "q1" "AskUserQuestion"
                             '((questions . [((question . "Tea or coffee?") (options . [((label . "Tea")) ((label . "Coffee"))]))])))
    (claude-native--tool-result "q1" "Your questions have been answered: \"Tea or coffee?\"=\"Tea\". You can now continue." nil)
    (let ((text (buffer-substring-no-properties (point-min) claude-native--input)))
      (should (string-match-p "⏺ AskUserQuestion(Tea or coffee\\?)" text))
      (should (string-match-p "⎿  · Tea or coffee\\? → Tea" text))
      (should-not (string-match-p "You can now continue" text)))))

(ert-deftest claude-native-history-offers-resumed-prompts ()
  (with-temp-buffer
    (claude-native-mode)
    (claude-native--draw-records
     (list '((type . "user") (message . ((role . "user") (content . "first"))))
           '((type . "user") (message . ((role . "user") (content . "second"))))))
    (claude-native-previous-prompt 1)
    (should (equal (claude-native--input-text) "second"))
    (claude-native-previous-prompt 1)
    (should (equal (claude-native--input-text) "first"))))

(ert-deftest claude-native-counts-read-lines ()
  (with-temp-buffer
    (claude-native-mode)
    (claude-native--tool-use "r1" "Read" '((file_path . "/x/a.txt")))
    (claude-native--tool-result "r1" "     1→alpha\n     2→beta\n     3→gamma\n\n<system-reminder>note</system-reminder>" nil)
    (should (string-match-p "Read 3 lines" (buffer-substring-no-properties (point-min) claude-native--input)))))

(ert-deftest claude-native-scrolling-keeps-the-input-at-the-bottom ()
  ;; Whatever moved the window (j into the prompt, G, C-f, zt, the wheel),
  ;; the layout check before each redisplay puts it back: the input on the
  ;; bottom row, and never blank space past the session's end.
  (with-temp-buffer
    (claude-native-mode)
    (claude-native--text-whole (mapconcat #'number-to-string (number-sequence 1 200) "\n"))
    (let ((window (selected-window)))
      (set-window-buffer window (current-buffer))
      (let ((bottom (car (claude-native--bottom window))))
        (should (> bottom (point-min)))
        ;; Cursor in the input, window scrolled past the end: pinned back.
        (goto-char (point-max))
        (set-window-start window (1- claude-native--spacer))
        (claude-native--before-redisplay window)
        (should (= (window-start window) bottom))
        ;; Cursor in the input, window scrolled up: pinned back too.
        (set-window-start window (point-min))
        (claude-native--before-redisplay window)
        (should (= (window-start window) bottom))
        ;; Reading up in the log: scrolling up is yours ...
        (goto-char (point-min))
        (set-window-start window (point-min))
        (claude-native--before-redisplay window)
        (should (= (window-start window) (point-min)))
        ;; ... but not past the end.
        (goto-char (+ bottom 3))
        (set-window-start window (1- claude-native--spacer))
        (claude-native--before-redisplay window)
        (should (= (window-start window) bottom))))))

(defun claude-native-test--png ()
  "A temp 1x1 PNG file."
  (let ((file (make-temp-file "cn-test-" nil ".png")))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert (base64-decode-string
               "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR4nGP4z8AAAAMBAQDJ/pLvAAAAAElFTkSuQmCC")))
    file))

(ert-deftest claude-native-attaches-files-to-the-prompt ()
  (with-temp-buffer
    (claude-native-mode)
    (let ((png (claude-native-test--png))
          (pdf (make-temp-file "cn-test-" nil ".pdf" "%PDF-1.4\n"))
          (txt (expand-file-name "notes.txt" default-directory)))
      (goto-char (point-max))
      (insert "look:")
      (claude-native-attach png)
      (claude-native-attach pdf)
      (should (equal (claude-native--input-text) "look: [Image #1] [PDF #2] "))
      ;; Each mark becomes a block before the text.
      (let ((content (claude-native--prompt-content (claude-native--input-text))))
        (should (vectorp content))
        (should (equal (mapcar (lambda (b) (alist-get 'type b)) content) '("image" "document" "text")))
        (should (equal (alist-get 'media_type (alist-get 'source (aref content 0))) "image/png"))
        ;; The echo of it reads as the prompt's text.
        (should (equal (claude-native--prompt-text content) "look: [Image #1] [PDF #2] ")))
      ;; A deleted mark drops its file.
      (should (= 2 (length (claude-native--prompt-content "only [PDF #2]"))))
      (should (stringp (claude-native--prompt-content "no marks")))
      ;; Other files are @mentions, relative inside the session directory.
      (should (equal (claude-native--attach-mark txt) "@notes.txt"))
      (should (equal (claude-native--mention "/tmp/a b.txt") "@\"/tmp/a b.txt\""))
      ;; Interruptions come as text blocks, but are no prompt.
      (should-not (claude-native--prompt-text [((type . "text") (text . "[Request interrupted by user]"))]))
      (delete-file png) (delete-file pdf))))

(ert-deftest claude-native-converts-heic-photos ()
  (skip-unless (executable-find "sips"))
  (with-temp-buffer
    (claude-native-mode)
    (let* ((png (claude-native-test--png))
           (heic (concat (file-name-sans-extension png) ".heic")))
      (call-process "sips" nil nil nil "-s" "format" "heic" png "--out" heic)
      (skip-unless (file-exists-p heic))
      (should (equal (claude-native--attach-mark heic) "[Image #1]"))
      (pcase-let ((`(,kind ,file ,media) (alist-get 1 claude-native--attachments)))
        (should (eq kind 'image))
        (should (equal media "image/jpeg"))
        (should (string-suffix-p ".jpg" file))
        (delete-file file))
      (delete-file png) (delete-file heic))))
