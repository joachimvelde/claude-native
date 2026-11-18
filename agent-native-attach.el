;;; agent-native-attach.el --- attach files to the next prompt -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; C-c C-a picks a file and marks it in the input; nothing is sent until
;; you press RET, so you can write around it. Images show as [Image #N],
;; PDFs as [PDF #N]; on send the file of each mark still in the input goes
;; along with the prompt (delete the mark to drop the file). Any other
;; file goes in as an @path mention, which the agent reads itself.
;; In dired, C-c C-a attaches the marked files (m, as for deleting).

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'seq)

(defvar agent-native--input)

(defcustom agent-native-image-max-bytes 3750000
  "Images larger than this are shrunk to JPEG before they are attached.
The API takes at most 5 MB of base64, which is 3.75 MB of image."
  :type 'integer
  :group 'agent-native)

(defconst agent-native--image-types
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("webp" . "image/webp"))
  "Image types the API takes as they are.")

(defconst agent-native--convertible '("heic" "heif" "tif" "tiff" "bmp" "avif")
  "Image types turned into JPEG (with macOS sips) before attaching.")

(defconst agent-native--attach-regexp "\\[\\(Image\\|PDF\\) #\\([0-9]+\\)\\]"
  "A mark in the input for an attached file.")

(defvar-local agent-native--attachments nil
  "Files attached in this session: (N . (KIND FILE MEDIA-TYPE)), KIND
`image' or `pdf'. Kept after sending, so M-p brings a prompt back whole.")

(defvar-local agent-native--attach-count 0 "The last attachment number used.")

(defvar-local agent-native--temp-files nil
  "JPEG copies made for attaching; deleted with the session buffer.")

;; Kept over /clear, which restarts the mode: the files are still there.
(put 'agent-native--temp-files 'permanent-local t)

(defun agent-native--delete-temp-files ()
  "Delete this session's JPEG copies."
  (dolist (file agent-native--temp-files)
    (ignore-errors (delete-file file)))
  (setq agent-native--temp-files nil))

(defun agent-native--shrink-image (file)
  "A JPEG copy of FILE, at most 2000 pixels a side, made with sips."
  (unless (executable-find "sips")
    (user-error "Can't convert %s: needs macOS sips" (file-name-nondirectory file)))
  (let ((out (make-temp-file "agent-native-" nil ".jpg")))
    (unless (zerop (call-process "sips" nil nil nil "-s" "format" "jpeg" "-Z" "2000"
                                 (expand-file-name file) "--out" out))
      (delete-file out)
      (user-error "sips could not convert %s" (file-name-nondirectory file)))
    (push out agent-native--temp-files)
    out))

(defun agent-native--mention (file)
  "FILE as an @mention: relative to the session directory when inside it,
quoted when it has spaces."
  (let* ((file (expand-file-name file))
         (path (if (file-in-directory-p file default-directory)
                   (file-relative-name file default-directory)
                 (abbreviate-file-name file))))
    (when (file-directory-p file) (setq path (file-name-as-directory path)))
    (if (string-match-p " " path) (format "@\"%s\"" path) (concat "@" path))))

(defun agent-native--attach-mark (file)
  "Register FILE and return the text that marks it in the input."
  (let* ((ext (downcase (or (file-name-extension file) "")))
         (size (file-attribute-size (file-attributes file)))
         (native (assoc ext agent-native--image-types)))
    (cond
     ;; A directory, whatever its name: the agent reads it from the mention.
     ((file-directory-p file) (agent-native--mention file))
     ;; Images and PDFs are read here, when sent: they must be there.
     ((and (or native (member ext agent-native--convertible) (equal ext "pdf"))
           (not (file-readable-p file)))
      (user-error "Can't read %s" (abbreviate-file-name file)))
     ((or (member ext agent-native--convertible)
          (and native (> size agent-native-image-max-bytes)))
      (agent-native--register 'image (agent-native--shrink-image file) "image/jpeg"))
     (native (agent-native--register 'image file (cdr native)))
     ((equal ext "pdf")
      ;; The request may be 32 MB, and base64 makes the file a third bigger.
      (when (> size 24000000) (user-error "%s is too big: PDFs over 24 MB exceed the API's 32 MB request limit"
                                          (file-name-nondirectory file)))
      (agent-native--register 'pdf file "application/pdf"))
     (t (agent-native--mention file)))))

(defun agent-native--register (kind file media)
  "Keep attachment FILE of KIND and MEDIA type; return its mark."
  (let ((n (cl-incf agent-native--attach-count)))
    (push (cons n (list kind (expand-file-name file) media)) agent-native--attachments)
    (format "[%s #%d]" (if (eq kind 'image) "Image" "PDF") n)))

(defun agent-native--insert-marks (files)
  "Put the marks for FILES at the cursor in the input."
  (let ((marks (mapcar #'agent-native--attach-mark files)))
    (when (< (point) agent-native--input) (goto-char (point-max)))
    (unless (or (= (point) agent-native--input) (memq (char-before) '(?\s ?\n)))
      (insert " "))
    (insert (string-join marks " ") " ")))

(defun agent-native-attach (file)
  "Attach FILE to the prompt you are writing; RET sends it with the text."
  (interactive (list (read-file-name "Attach: " nil nil t)))
  (agent-native--insert-marks (list file)))

(defun agent-native--attachments-in (text)
  "The files the marks in prompt TEXT stand for, in order: (KIND FILE
MEDIA-TYPE) lists, as `agent-native-backend-send' takes them. A mark whose
file is gone, or from before this session, stops the send."
  (let (files (start 0))
    (while (string-match agent-native--attach-regexp text start)
      (setq start (match-end 0))
      (let ((entry (or (alist-get (string-to-number (match-string 2 text)) agent-native--attachments)
                       ;; From a resumed session's prompt (M-p): its files are not known here.
                       (user-error "%s is from before this session started; attach the file again"
                                   (match-string 0 text)))))
        (unless (file-readable-p (nth 1 entry))
          (user-error "Attached file is gone: %s" (nth 1 entry)))
        (push entry files)))
    (nreverse files)))

(defun agent-native--count-attachments (text)
  "Number new attachments past those marked in TEXT, an old prompt."
  (let ((start 0))
    (while (string-match agent-native--attach-regexp text start)
      (setq start (match-end 0)
            agent-native--attach-count (max agent-native--attach-count
                                            (string-to-number (match-string 2 text)))))))

;;;; Dired: attach the marked files

(defun agent-native--session-for (file)
  "The session to attach FILE to: the most recent one whose
directory holds FILE, else the most recent one."
  (let ((sessions (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) 'agent-native-mode))
                              (buffer-list))))
    (or (seq-find (lambda (b) (file-in-directory-p file (buffer-local-value 'default-directory b)))
                  sessions)
        (car sessions)
        (user-error "No agent session: M-x agent-native starts one"))))

(declare-function dired-get-marked-files "dired")
(defun agent-native-dired-attach ()
  "Attach the marked files (or the one at point) to an agent session's
prompt, and go there to write the rest."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (session (agent-native--session-for (car files))))
    (pop-to-buffer session)
    (agent-native--insert-marks files)
    (message "Attached %d file%s" (length files) (if (cdr files) "s" ""))))

(with-eval-after-load 'dired
  (define-key (symbol-value 'dired-mode-map) (kbd "C-c C-a") #'agent-native-dired-attach))

(provide 'agent-native-attach)
;;; agent-native-attach.el ends here
