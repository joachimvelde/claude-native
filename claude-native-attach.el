;;; claude-native-attach.el --- attach files to the next prompt -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; C-c C-a picks a file and marks it in the input; nothing is sent until
;; you press RET, so you can write around it. Images show as [Image #N],
;; PDFs as [PDF #N]; on send each mark still in the input goes along as an
;; image or document block (delete the mark to drop the file). Any other
;; file goes in as an @path mention, which Claude Code reads in itself.
;; In dired, C-c C-a attaches the marked files (m, as for deleting).

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'seq)

(defvar claude-native--input)

(defcustom claude-native-image-max-bytes 3750000
  "Images larger than this are shrunk to JPEG before they are attached.
The API takes at most 5 MB of base64, which is 3.75 MB of image."
  :type 'integer
  :group 'claude-native)

(defconst claude-native--image-types
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("webp" . "image/webp"))
  "Image types the API takes as they are.")

(defconst claude-native--convertible '("heic" "heif" "tif" "tiff" "bmp" "avif")
  "Image types turned into JPEG (with macOS sips) before attaching.")

(defconst claude-native--attach-regexp "\\[\\(Image\\|PDF\\) #\\([0-9]+\\)\\]"
  "A mark in the input for an attached file.")

(defvar-local claude-native--attachments nil
  "Files attached in this session: (N . (KIND FILE MEDIA-TYPE)), KIND
`image' or `pdf'. Kept after sending, so M-p brings a prompt back whole.")

(defvar-local claude-native--attach-count 0 "The last attachment number used.")

(defun claude-native--shrink-image (file)
  "A JPEG copy of FILE, at most 2000 pixels a side, made with sips."
  (unless (executable-find "sips")
    (user-error "Can't convert %s: needs macOS sips" (file-name-nondirectory file)))
  (let ((out (make-temp-file "claude-native-" nil ".jpg")))
    (unless (zerop (call-process "sips" nil nil nil "-s" "format" "jpeg" "-Z" "2000"
                                 (expand-file-name file) "--out" out))
      (user-error "sips could not convert %s" (file-name-nondirectory file)))
    out))

(defun claude-native--mention (file)
  "FILE as an @mention: relative to the session directory when inside it,
quoted when it has spaces."
  (let* ((file (expand-file-name file))
         (path (if (file-in-directory-p file default-directory)
                   (file-relative-name file default-directory)
                 (abbreviate-file-name file))))
    (when (file-directory-p file) (setq path (file-name-as-directory path)))
    (if (string-match-p " " path) (format "@\"%s\"" path) (concat "@" path))))

(defun claude-native--attach-mark (file)
  "Register FILE and return the text that marks it in the input."
  (let* ((ext (downcase (or (file-name-extension file) "")))
         (size (file-attribute-size (file-attributes file)))
         (native (assoc ext claude-native--image-types)))
    (cond
     ((or (member ext claude-native--convertible)
          (and native (> size claude-native-image-max-bytes)))
      (claude-native--register 'image (claude-native--shrink-image file) "image/jpeg"))
     (native (claude-native--register 'image file (cdr native)))
     ((equal ext "pdf")
      (when (> size 32000000) (user-error "%s is over the 32 MB PDF limit" (file-name-nondirectory file)))
      (claude-native--register 'pdf file "application/pdf"))
     (t (claude-native--mention file)))))

(defun claude-native--register (kind file media)
  "Keep attachment FILE of KIND and MEDIA type; return its mark."
  (let ((n (cl-incf claude-native--attach-count)))
    (push (cons n (list kind (expand-file-name file) media)) claude-native--attachments)
    (format "[%s #%d]" (if (eq kind 'image) "Image" "PDF") n)))

(defun claude-native--insert-marks (files)
  "Put the marks for FILES at the cursor in the input."
  (let ((marks (mapcar #'claude-native--attach-mark files)))
    (when (< (point) claude-native--input) (goto-char (point-max)))
    (unless (or (= (point) claude-native--input) (memq (char-before) '(?\s ?\n)))
      (insert " "))
    (insert (string-join marks " ") " ")))

(defun claude-native-attach (file)
  "Attach FILE to the prompt you are writing; RET sends it with the text."
  (interactive (list (read-file-name "Attach: " nil nil t)))
  (claude-native--insert-marks (list file)))

(defun claude-native--prompt-content (text)
  "What to send for prompt TEXT: TEXT itself, or with the files its marks
name, a vector of their blocks and then the text."
  (let (blocks (start 0))
    (while (string-match claude-native--attach-regexp text start)
      (setq start (match-end 0))
      (when-let* ((entry (alist-get (string-to-number (match-string 2 text)) claude-native--attachments)))
        (pcase-let ((`(,kind ,file ,media) entry))
          (unless (file-readable-p file) (user-error "Attached file is gone: %s" file))
          (push `((type . ,(if (eq kind 'image) "image" "document"))
                  (source . ((type . "base64") (media_type . ,media)
                             (data . ,(with-temp-buffer
                                        (set-buffer-multibyte nil)
                                        (insert-file-contents-literally file)
                                        (base64-encode-region (point-min) (point-max) t)
                                        (buffer-string))))))
                blocks))))
    (if blocks
        (vconcat (nreverse blocks) (list `((type . "text") (text . ,text))))
      text)))

(defun claude-native--prompt-text (content)
  "The text of prompt CONTENT: a string, or a vector of blocks (a prompt
with files attached). Nil if CONTENT is not a prompt."
  (cond ((stringp content) content)
        ;; Blocks are a prompt only with a file in them: interruptions
        ;; and tool results come as blocks too.
        ((and (vectorp content)
              (seq-some (lambda (b) (member (alist-get 'type b) '("image" "document"))) content))
         (let ((texts (delq nil (mapcar (lambda (b) (and (equal (alist-get 'type b) "text")
                                                          (alist-get 'text b)))
                                        content))))
           (and texts (string-join texts "\n"))))))

;;;; Dired: attach the marked files

(defun claude-native--session-for (file)
  "The Claude session to attach FILE to: the most recent one whose
directory holds FILE, else the most recent one."
  (let ((sessions (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) 'claude-native-mode))
                              (buffer-list))))
    (or (seq-find (lambda (b) (file-in-directory-p file (buffer-local-value 'default-directory b)))
                  sessions)
        (car sessions)
        (user-error "No Claude session: M-x claude-native starts one"))))

(declare-function dired-get-marked-files "dired")
(defun claude-native-dired-attach ()
  "Attach the marked files (or the one at point) to a Claude session's
prompt, and go there to write the rest."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (session (claude-native--session-for (car files))))
    (pop-to-buffer session)
    (claude-native--insert-marks files)
    (message "Attached %d file%s" (length files) (if (cdr files) "s" ""))))

(with-eval-after-load 'dired
  (define-key (symbol-value 'dired-mode-map) (kbd "C-c C-a") #'claude-native-dired-attach))

(provide 'claude-native-attach)
;;; claude-native-attach.el ends here
