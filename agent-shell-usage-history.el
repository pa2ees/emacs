;;; agent-shell-usage-history.el --- Persistent usage tracking across sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Erik van Zwol

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Provides persistent usage tracking across agent-shell sessions.
;; Records usage data to disk and provides aggregate views by day/week/month.

;;; Code:

(require 'map)
(require 'json)

(defvar agent-shell--state)
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--format-number-compact "agent-shell-usage")

(defcustom agent-shell-usage-history-file
  (expand-file-name "agent-shell-usage-history.json"
                    (or (getenv "XDG_DATA_HOME")
                        (expand-file-name ".local/share" "~")))
  "File path for persistent usage history storage."
  :type 'file
  :group 'agent-shell)

(defvar agent-shell-usage-history--last-saved-usage nil
  "Last saved usage data to avoid duplicate writes.")

(defun agent-shell-usage-history--load-history ()
  "Load usage history from disk.
Returns a list of usage records, each with timestamp and usage data."
  (when (file-exists-p agent-shell-usage-history-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents agent-shell-usage-history-file)
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol))
            (json-read)))
      (error
       (message "Error loading usage history: %s" err)
       nil))))

(defun agent-shell-usage-history--save-history (history)
  "Save usage HISTORY to disk."
  (let ((dir (file-name-directory agent-shell-usage-history-file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file agent-shell-usage-history-file
    (let ((json-encoding-pretty-print t))
      (insert (json-encode history)))))

(defun agent-shell-usage-history--record-usage (usage)
  "Record USAGE data to persistent history.
USAGE should be an alist with token counts, context, and cost."
  (when (and usage
             (> (or (map-elt usage :total-tokens) 0) 0))
    (let* ((history (or (agent-shell-usage-history--load-history) '()))
           (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
           (record (list (cons 'timestamp timestamp)
                        (cons 'total_tokens (or (map-elt usage :total-tokens) 0))
                        (cons 'input_tokens (or (map-elt usage :input-tokens) 0))
                        (cons 'output_tokens (or (map-elt usage :output-tokens) 0))
                        (cons 'thought_tokens (or (map-elt usage :thought-tokens) 0))
                        (cons 'cached_read_tokens (or (map-elt usage :cached-read-tokens) 0))
                        (cons 'cached_write_tokens (or (map-elt usage :cached-write-tokens) 0))
                        (cons 'context_used (or (map-elt usage :context-used) 0))
                        (cons 'context_size (or (map-elt usage :context-size) 0))
                        (cons 'cost_amount (or (map-elt usage :cost-amount) 0))
                        (cons 'cost_currency (or (map-elt usage :cost-currency) "USD")))))
      (push record history)
      (agent-shell-usage-history--save-history history))))

(defun agent-shell-usage-history--maybe-save ()
  "Save current session usage if it has changed.
Should be called after each agent turn."
  (when (derived-mode-p 'agent-shell-mode)
    (when-let* ((usage (map-elt agent-shell--state :usage))
                (total (map-elt usage :total-tokens))
                ((> total 0))
                ((or (null agent-shell-usage-history--last-saved-usage)
                     (not (equal total (map-elt agent-shell-usage-history--last-saved-usage :total-tokens))))))
      (agent-shell-usage-history--record-usage usage)
      (setq agent-shell-usage-history--last-saved-usage usage))))

(defun agent-shell-usage-history--parse-date (timestamp)
  "Parse TIMESTAMP string to time value."
  (date-to-time timestamp))

(defun agent-shell-usage-history--filter-by-period (history start-time)
  "Filter HISTORY records to only include those after START-TIME."
  (seq-filter (lambda (record)
                (let ((record-time (agent-shell-usage-history--parse-date
                                   (map-elt record 'timestamp))))
                  (time-less-p start-time record-time)))
              history))

(defun agent-shell-usage-history--aggregate-usage (records)
  "Aggregate usage from RECORDS into totals."
  (let ((total-tokens 0)
        (input-tokens 0)
        (output-tokens 0)
        (thought-tokens 0)
        (cached-read-tokens 0)
        (total-cost 0)
        (currency "USD")
        (session-count (length records)))
    (dolist (record records)
      (cl-incf total-tokens (or (map-elt record 'total_tokens) 0))
      (cl-incf input-tokens (or (map-elt record 'input_tokens) 0))
      (cl-incf output-tokens (or (map-elt record 'output_tokens) 0))
      (cl-incf thought-tokens (or (map-elt record 'thought_tokens) 0))
      (cl-incf cached-read-tokens (or (map-elt record 'cached_read_tokens) 0))
      (cl-incf total-cost (or (map-elt record 'cost_amount) 0))
      (when-let ((cur (map-elt record 'cost_currency)))
        (setq currency cur)))
    (list :total-tokens total-tokens
          :input-tokens input-tokens
          :output-tokens output-tokens
          :thought-tokens thought-tokens
          :cached-read-tokens cached-read-tokens
          :cost-amount total-cost
          :cost-currency currency
          :session-count session-count)))

(defun agent-shell-usage-history--get-period-stats (period)
  "Get usage statistics for PERIOD.
PERIOD can be 'day, 'week, or 'month."
  (let* ((now (current-time))
         (start-time (pcase period
                       ('day (time-subtract now (days-to-time 1)))
                       ('week (time-subtract now (days-to-time 7)))
                       ('month (time-subtract now (days-to-time 30)))
                       (_ (error "Invalid period: %s" period))))
         (history (agent-shell-usage-history--load-history))
         (filtered (agent-shell-usage-history--filter-by-period history start-time)))
    (if filtered
        (agent-shell-usage-history--aggregate-usage filtered)
      nil)))

(defun agent-shell-usage-history--format-stats (stats period-name)
  "Format STATS for display with PERIOD-NAME label."
  (if (null stats)
      (format "No usage data for %s" period-name)
    (let* ((sessions (plist-get stats :session-count))
           (total (plist-get stats :total-tokens))
           (input (plist-get stats :input-tokens))
           (output (plist-get stats :output-tokens))
           (thought (plist-get stats :thought-tokens))
           (cached (plist-get stats :cached-read-tokens))
           (cost (plist-get stats :cost-amount))
           (currency (plist-get stats :cost-currency)))
      (concat
       (propertize (format "=== %s Usage ===" period-name)
                   'face 'bold)
       "\n"
       (format "Sessions: %d\n" sessions)
       (format "Total tokens: %s\n"
               (agent-shell--format-number-compact total))
       (format "  Input: %s\n"
               (agent-shell--format-number-compact input))
       (format "  Output: %s\n"
               (agent-shell--format-number-compact output))
       (when (> thought 0)
         (format "  Thought: %s\n"
                 (agent-shell--format-number-compact thought)))
       (when (> cached 0)
         (format "  Cached read: %s\n"
                 (agent-shell--format-number-compact cached)))
       (format "Cost: %s%.2f\n" currency cost)))))

(defun agent-shell-usage-history-show-daily ()
  "Show usage statistics for the last 24 hours."
  (interactive)
  (let ((stats (agent-shell-usage-history--get-period-stats 'day)))
    (message "%s" (agent-shell-usage-history--format-stats stats "Last 24 Hours"))))

(defun agent-shell-usage-history-show-weekly ()
  "Show usage statistics for the last 7 days."
  (interactive)
  (let ((stats (agent-shell-usage-history--get-period-stats 'week)))
    (message "%s" (agent-shell-usage-history--format-stats stats "Last 7 Days"))))

(defun agent-shell-usage-history-show-monthly ()
  "Show usage statistics for the last 30 days."
  (interactive)
  (let ((stats (agent-shell-usage-history--get-period-stats 'month)))
    (message "%s" (agent-shell-usage-history--format-stats stats "Last 30 Days"))))

(defun agent-shell-usage-history-show-all ()
  "Show usage statistics for all time periods in a dedicated buffer."
  (interactive)
  (let ((daily (agent-shell-usage-history--get-period-stats 'day))
        (weekly (agent-shell-usage-history--get-period-stats 'week))
        (monthly (agent-shell-usage-history--get-period-stats 'month))
        (buf (get-buffer-create "*Agent Shell Usage History*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (agent-shell-usage-history--format-stats daily "Last 24 Hours"))
        (insert "\n")
        (insert (agent-shell-usage-history--format-stats weekly "Last 7 Days"))
        (insert "\n")
        (insert (agent-shell-usage-history--format-stats monthly "Last 30 Days"))
        (insert "\n")
        (insert (propertize "Press q to close" 'face 'font-lock-comment-face))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf)))

(defun agent-shell-usage-history-clear-old (days)
  "Remove usage records older than DAYS from history."
  (interactive "nKeep records from last N days: ")
  (let* ((cutoff-time (time-subtract (current-time) (days-to-time days)))
         (history (agent-shell-usage-history--load-history))
         (filtered (agent-shell-usage-history--filter-by-period history cutoff-time))
         (removed-count (- (length history) (length filtered))))
    (if (> removed-count 0)
        (progn
          (agent-shell-usage-history--save-history filtered)
          (message "Removed %d old record(s), kept %d record(s) from last %d days"
                   removed-count (length filtered) days))
      (message "No records older than %d days" days))))

(defun agent-shell-usage-history-export ()
  "Export usage history to a CSV file."
  (interactive)
  (let* ((default-file (format "agent-shell-usage-%s.csv"
                               (format-time-string "%Y%m%d")))
         (filename (read-file-name "Export to CSV: " nil nil nil default-file))
         (history (agent-shell-usage-history--load-history)))
    (if (null history)
        (message "No usage history to export")
      (with-temp-file filename
        (insert "timestamp,total_tokens,input_tokens,output_tokens,thought_tokens,cached_read_tokens,context_used,context_size,cost_amount,cost_currency\n")
        (dolist (record history)
          (insert (format "%s,%d,%d,%d,%d,%d,%d,%d,%.4f,%s\n"
                          (map-elt record 'timestamp)
                          (or (map-elt record 'total_tokens) 0)
                          (or (map-elt record 'input_tokens) 0)
                          (or (map-elt record 'output_tokens) 0)
                          (or (map-elt record 'thought_tokens) 0)
                          (or (map-elt record 'cached_read_tokens) 0)
                          (or (map-elt record 'context_used) 0)
                          (or (map-elt record 'context_size) 0)
                          (or (map-elt record 'cost_amount) 0.0)
                          (or (map-elt record 'cost_currency) "USD")))))
      (message "Exported %d records to %s" (length history) filename))))

(defun agent-shell-usage-history--advice-save-usage (orig-fun &rest args)
  "Advice to save usage after agent-shell saves it.
ORIG-FUN is the original function, ARGS are its arguments."
  (prog1
      (apply orig-fun args)
    ;; After usage is saved to session state, also persist it
    (when-let* ((state (plist-get args :state))
                (usage (map-elt state :usage)))
      (agent-shell-usage-history--record-usage usage))))

(defun agent-shell-usage-history--setup-advice ()
  "Set up advice to automatically save usage after each turn."
  (advice-add 'agent-shell--save-usage :around
              #'agent-shell-usage-history--advice-save-usage))

(defun agent-shell-usage-history--remove-advice ()
  "Remove usage saving advice."
  (advice-remove 'agent-shell--save-usage
                 #'agent-shell-usage-history--advice-save-usage))

;; Set up advice when loaded
(with-eval-after-load 'agent-shell
  (agent-shell-usage-history--setup-advice))

(provide 'agent-shell-usage-history)
;;; agent-shell-usage-history.el ends here
