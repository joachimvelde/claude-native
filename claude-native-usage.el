;;; claude-native-usage.el --- draw /usage: plan limits, context, cost -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Turns claude's answers to the get_usage and get_context_usage control
;; requests into the text /usage shows: a bar per plan limit, one for the
;; context window, what the session cost, and what uses the plan most,
;; and shows it in a popup buffer. Nothing here talks to claude;
;; `claude-native-usage' does that.

;;; Code:

(require 'iso8601)
(require 'claude-native-render)

(defconst claude-native--usage-bar-width 20
  "How many characters wide a usage bar is.")

(defun claude-native--usage-bar (percent)
  "A bar filled to PERCENT, coloured as it nears full."
  (let* ((percent (max 0 (min 100 percent)))
         ;; Any use at all shows as one cell.
         (filled (if (> percent 0)
                     (max 1 (round (* claude-native--usage-bar-width percent) 100))
                   0)))
    (concat (propertize (make-string filled ?█)
                        'face (cond ((>= percent 90) 'claude-native-tool-error)
                                    ((>= percent 70) 'warning)
                                    (t 'claude-native-accent)))
            (propertize (make-string (- claude-native--usage-bar-width filled) ?░)
                        'face 'claude-native-dim))))

(defun claude-native--usage-resets (iso)
  "\"resets Nov 13, 18:00\" for ISO, a timestamp; nil if it is not one."
  (when (stringp iso)
    (condition-case nil
        ;; To the nearest minute: 17:59:59.9 is 18:00.
        (format-time-string "resets %b %-e, %H:%M"
                            (time-add (encode-time (iso8601-parse iso)) 30))
      (error nil))))

(defun claude-native--usage-tokens (count)
  "COUNT tokens in short: 950, 23k, 1.2M."
  (cond ((>= count 1000000) (format "%.3gM" (/ count 1e6)))
        ((>= count 1000) (format "%dk" (round count 1000)))
        (t (number-to-string count))))

(defun claude-native--usage-limits (rate-limits)
  "The plan limits in RATE-LIMITS, as rows (TITLE PERCENT DETAIL)."
  (or (mapcar (lambda (limit)
                (let ((model (alist-get 'display_name (alist-get 'model (alist-get 'scope limit)))))
                  (list (pcase (alist-get 'kind limit)
                          ("session" "Current session")
                          ("weekly_all" "Current week (all models)")
                          (kind (if model (format "Current week (%s)" model) kind)))
                        (alist-get 'percent limit)
                        (claude-native--usage-resets (alist-get 'resets_at limit)))))
              (alist-get 'limits rate-limits))
      ;; A claude that lists no limits still names the two windows.
      (delq nil (mapcar (lambda (window)
                          (when-let* ((limit (alist-get (car window) rate-limits)))
                            (list (cdr window) (alist-get 'utilization limit)
                                  (claude-native--usage-resets (alist-get 'resets_at limit)))))
                        '((five_hour . "Current session")
                          (seven_day . "Current week (all models)"))))))

(defun claude-native--usage-session (session)
  "What SESSION (cost, time, lines changed) comes to, on one line."
  (let ((seconds (/ (or (alist-get 'total_api_duration_ms session) 0) 1000))
        (added (or (alist-get 'total_lines_added session) 0))
        (removed (or (alist-get 'total_lines_removed session) 0)))
    (string-join
     (delq nil (list (format "$%.2f" (or (alist-get 'total_cost_usd session) 0))
                     (when (> seconds 0)
                       (concat (format-seconds "%hh %mm %z%ss" seconds) " API time"))
                     (when (> (+ added removed) 0)
                       (format "+%d −%d lines" added removed))))
     " · ")))

(defun claude-native--usage-behaviors (title period)
  "Lines on what used the plan most in PERIOD, headed by TITLE."
  (when period
    (append
     (list ""
           (format "%s · %d requests · %d sessions" title
                   (or (alist-get 'request_count period) 0) (or (alist-get 'session_count period) 0)))
     (mapcar (lambda (advice)
               (propertize (format "  %3d%%  %s%s" (alist-get 'pct advice) (alist-get 'title advice)
                                   (if-let* ((detail (alist-get 'detail advice))) (concat " · " detail) ""))
                           'face 'claude-native-dim))
             (alist-get 'recommendations period))
     (delq nil
           (mapcar (lambda (top)
                     (when-let* ((items (append (alist-get (car top) period) nil)))
                       (propertize
                        (format "  Top %s: %s" (cadr top)
                                (mapconcat (lambda (item)
                                             (format "%s%s %d%%" (caddr top) (alist-get 'name item) (alist-get 'pct item)))
                                           items ", "))
                        'face 'claude-native-dim)))
                   '((skills "skills" "/") (agents "subagents" "") (plugins "plugins" "")
                     (mcp_servers "MCP servers" "")))))))

(defun claude-native--usage-text (usage context)
  "The usage report: USAGE answers get_usage, CONTEXT get_context_usage
\(nil when claude gave none)."
  (let* ((rate-limits (and (eq (alist-get 'rate_limits_available usage) t)
                           (alist-get 'rate_limits usage)))
         (rows (append
                (claude-native--usage-limits rate-limits)
                (when-let* ((percent (alist-get 'percentage context)))
                  (list (list "Context" percent
                              (format "%s of %s tokens"
                                      (claude-native--usage-tokens (or (alist-get 'totalTokens context) 0))
                                      (claude-native--usage-tokens (or (alist-get 'maxTokens context) 0))))))))
         (width (apply #'max (mapcar (lambda (row) (string-width (car row)))
                                     (cons '("Session") rows))))
         (behaviors (alist-get 'behaviors usage)))
    (string-join
     (append
      (mapcar (lambda (row)
                (pcase-let ((`(,title ,percent ,detail) row))
                  (concat (string-pad title (+ width 2))
                          (claude-native--usage-bar percent)
                          (format " %3d%%" (round percent))
                          (when detail (propertize (concat "  " detail) 'face 'claude-native-dim)))))
              rows)
      (list (concat (string-pad "Session" (+ width 2))
                    (propertize (claude-native--usage-session (alist-get 'session usage))
                                'face 'claude-native-dim)))
      (claude-native--usage-behaviors "Last 24h" (alist-get 'day behaviors))
      (claude-native--usage-behaviors "Last 7d" (alist-get 'week behaviors)))
     "\n")))

(define-derived-mode claude-native-usage-mode special-mode "Claude usage"
  "The usage report; q closes it."
  (setq-local truncate-lines t
              mode-line-format nil
              ;; Room at the sides; blank lines give it above and below.
              left-margin-width 3
              right-margin-width 3))

;; With evil: motion state, where q still closes the window.
(declare-function evil-set-initial-state "evil-core")
(with-eval-after-load 'evil
  (evil-set-initial-state 'claude-native-usage-mode 'motion))

(defun claude-native--show-usage (usage context)
  "Show the usage report for USAGE and CONTEXT in a window below."
  (let ((buffer (get-buffer-create "*claude usage*")))
    (with-current-buffer buffer
      (claude-native-usage-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n" (claude-native--usage-text usage context) "\n\n")
        (goto-char (point-min))))
    (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-at-bottom)
                            (window-height . fit-window-to-buffer)))))

(provide 'claude-native-usage)
;;; claude-native-usage.el ends here
