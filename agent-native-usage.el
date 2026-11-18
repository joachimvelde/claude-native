;;; agent-native-usage.el --- draw /usage: plan limits, context, session -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Draws a usage report (see `agent-native-backend-usage') in a popup: a
;; bar per plan limit and one for the context window, a line on what the
;; session used, then sections such as what used the plan most. Each
;; backend builds the report from what its harness tells; the drawing is
;; one for all.

;;; Code:

(require 'agent-native-render)

(defconst agent-native--usage-bar-width 20
  "How many characters wide a usage bar is.")

(defun agent-native--usage-bar (percent)
  "A bar filled to PERCENT, coloured as it nears full."
  (let* ((percent (max 0 (min 100 percent)))
         ;; Any use at all shows as one cell.
         (filled (if (> percent 0)
                     (max 1 (round (* agent-native--usage-bar-width percent) 100))
                   0)))
    (concat (propertize (make-string filled ?█)
                        'face (cond ((>= percent 90) 'agent-native-tool-error)
                                    ((>= percent 70) 'warning)
                                    (t 'agent-native-accent)))
            (propertize (make-string (- agent-native--usage-bar-width filled) ?░)
                        'face 'agent-native-dim))))

(defun agent-native--usage-text (report)
  "REPORT (see `agent-native-backend-usage') as text."
  (let* ((rows (plist-get report :rows))
         (width (apply #'max (mapcar (lambda (row) (string-width (car row)))
                                     (cons '("Session") rows)))))
    (string-join
     (append
      (mapcar (lambda (row)
                (pcase-let ((`(,title ,percent ,detail) row))
                  (concat (string-pad title (+ width 2))
                          (agent-native--usage-bar percent)
                          (format " %3d%%" (round percent))
                          (when detail (propertize (concat "  " detail) 'face 'agent-native-dim)))))
              rows)
      (when-let* ((session (plist-get report :session)))
        (list (concat (string-pad "Session" (+ width 2))
                      (propertize session 'face 'agent-native-dim))))
      (mapcan (lambda (section)
                (cons "" (cons (car section)
                               (mapcar (lambda (line) (propertize line 'face 'agent-native-dim))
                                       (cdr section)))))
              (plist-get report :sections)))
     "\n")))

(define-derived-mode agent-native-usage-mode special-mode "Agent usage"
  "The usage report; q closes it."
  (setq-local truncate-lines t
              mode-line-format nil
              ;; Room at the sides; blank lines give it above and below.
              left-margin-width 3
              right-margin-width 3))

;; With evil: motion state, where q still closes the window.
(declare-function evil-set-initial-state "evil-core")
(with-eval-after-load 'evil
  (evil-set-initial-state 'agent-native-usage-mode 'motion))

(defun agent-native--show-usage (name report)
  "Show usage REPORT in a window below, in a buffer named for NAME."
  (let ((buffer (get-buffer-create (format "*%s usage*" (downcase name)))))
    (with-current-buffer buffer
      (agent-native-usage-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n" (agent-native--usage-text report) "\n\n")
        (goto-char (point-min))))
    (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-at-bottom)
                            (window-height . fit-window-to-buffer)))))

(provide 'agent-native-usage)
;;; agent-native-usage.el ends here
