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

;; Keymap for usage history commands
(defvar agent-shell-usage-history-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'agent-shell-usage-history-show-all)
    (define-key map "1" #'agent-shell-usage-history-show-daily)
    (define-key map "7" #'agent-shell-usage-history-show-weekly)
    (define-key map "3" #'agent-shell-usage-history-show-monthly)
    (define-key map "e" #'agent-shell-usage-history-export)
    (define-key map "c" #'agent-shell-usage-history-clear-old)
    (define-key map "d" #'agent-shell-usage-history-toggle-debug)
    map)
  "Keymap for agent-shell usage history commands, bound to C-c a U.")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--format-number-compact "agent-shell-usage")

(defcustom agent-shell-usage-history-directory
  (expand-file-name "agent-shell-usage-history"
                    (or (getenv "XDG_DATA_HOME")
                        (expand-file-name ".local/share" "~")))
  "Directory path for persistent usage history storage."
  :type 'directory
  :group 'agent-shell)

(defvar agent-shell-usage-history-debug nil
  "When non-nil, output debug messages to the usage history debug buffer.")

(defvar agent-shell-usage-history-debug-buffer-name "*Agent Shell Usage History Debug*"
  "Name of the buffer for debug messages.")

(defun agent-shell-usage-history--debug (format-string &rest args)
  "Log a debug message to the usage history debug buffer if debug is enabled."
  (when agent-shell-usage-history-debug
    (with-current-buffer (get-buffer-create agent-shell-usage-history-debug-buffer-name)
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert (apply #'format format-string args))
      (insert "\n"))))

(defun agent-shell-usage-history--get-week-string (&optional time)
  "Get ISO week string (e.g., '2026-W20') for TIME (defaults to current time)."
  (let* ((time (or time (current-time)))
         (decoded (decode-time time))
         (year (nth 5 decoded))
         (week (string-to-number (format-time-string "%V" time))))
    (format "%04d-W%02d" year week)))

(defun agent-shell-usage-history--get-week-file (&optional time)
  "Get the history file path for the week containing TIME (defaults to current time)."
  (let ((week-string (agent-shell-usage-history--get-week-string time)))
    (expand-file-name (format "usage-%s.jsonl" week-string)
                      agent-shell-usage-history-directory)))

(defun agent-shell-usage-history--load-week-file (file)
  "Load usage history from a single JSONL FILE.
Returns a list of usage records."
  (if (file-exists-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (let ((json-object-type 'alist)
                  (json-key-type 'symbol)
                  (records '()))
              (goto-char (point-min))
              (while (not (eobp))
                (unless (looking-at-p "^[[:space:]]*$")
                  (let ((line (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
                    (when (> (length line) 0)
                      (push (json-read-from-string line) records))))
                (forward-line 1))
              (nreverse records)))
        (error
         (agent-shell-usage-history--debug "Error loading file %s: %s" file err)
         nil))
    nil))

(defun agent-shell-usage-history--load-history ()
  "Load usage history from disk.
Returns a list of usage records, each with timestamp and usage data."
  (let ((week-files (when (file-directory-p agent-shell-usage-history-directory)
                      (directory-files agent-shell-usage-history-directory t "^usage-.*\\.jsonl$")))
        (all-records nil))
    (dolist (file week-files)
      (agent-shell-usage-history--debug "Loading week file: %s" file)
      (let ((records (agent-shell-usage-history--load-week-file file)))
        (setq all-records (append all-records records))))
    (agent-shell-usage-history--debug "Loaded %d total records" (length all-records))
    all-records))

(defun agent-shell-usage-history--append-to-week-file (record)
  "Append a single RECORD to the appropriate weekly JSONL file.
Uses file locking and retries to handle concurrent writes safely."
  (condition-case err
      (let* ((timestamp (map-elt record 'timestamp))
             (time (when timestamp (date-to-time timestamp)))
             (week-file (agent-shell-usage-history--get-week-file time))
             (max-retries 5)
             (retry-delay 0.1)
             (success nil))
        (agent-shell-usage-history--debug "Appending record to: %s" week-file)
        (unless (file-directory-p agent-shell-usage-history-directory)
          (agent-shell-usage-history--debug "Creating directory: %s" agent-shell-usage-history-directory)
          (make-directory agent-shell-usage-history-directory t))

        ;; Retry loop for handling concurrent access
        (let ((attempt 0))
          (while (and (not success) (< attempt max-retries))
            (setq attempt (1+ attempt))
            (condition-case write-err
                (progn
                  ;; Use write-region with append mode and let Emacs handle locking
                  (with-temp-buffer
                    (insert (json-encode record))
                    (insert "\n")
                    ;; write-region with APPEND=t and LOCKNAME=nil uses Emacs' file locking
                    (let ((coding-system-for-write 'utf-8-unix))
                      (write-region (point-min) (point-max) week-file t 'silent)))
                  (setq success t)
                  (agent-shell-usage-history--debug "Append complete on attempt %d" attempt))
              (file-error
               (agent-shell-usage-history--debug "Write failed on attempt %d: %S" attempt write-err)
               (when (< attempt max-retries)
                 (agent-shell-usage-history--debug "Retrying after %f seconds" retry-delay)
                 (sleep-for retry-delay))))))

        (unless success
          (error "Failed to write after %d attempts" max-retries)))
    (error
     (agent-shell-usage-history--debug "Error appending to week file: %S" err)
     (message "Error saving usage history: %S" err))))


(defun agent-shell-usage-history--record-usage (usage)
  "Record USAGE data to persistent history.
USAGE should be an alist with token counts, context, and cost."
  (condition-case err
      (progn
        (agent-shell-usage-history--debug "record-usage called with: %S" usage)
        (if (not usage)
            (agent-shell-usage-history--debug "No usage data provided, skipping")
          (let ((total-tokens (or (map-elt usage :total-tokens) 0)))
            (if (<= total-tokens 0)
                (agent-shell-usage-history--debug "Total tokens is %d, skipping record" total-tokens)
              (let* ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
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
                (agent-shell-usage-history--debug "Recording usage: tokens=%d timestamp=%s" total-tokens timestamp)
                (agent-shell-usage-history--append-to-week-file record)
                (agent-shell-usage-history--debug "Usage recorded successfully"))))))
    (error
     (agent-shell-usage-history--debug "Error in record-usage: %S" err)
     (message "Error recording usage history: %S" err))))


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
  "Remove usage records older than DAYS from history.
Deletes old weekly JSONL files."
  (interactive "nKeep records from last N days: ")
  (let* ((cutoff-time (time-subtract (current-time) (days-to-time days)))
         (week-files (when (file-directory-p agent-shell-usage-history-directory)
                       (directory-files agent-shell-usage-history-directory t "^usage-.*\\.jsonl$")))
         (deleted-count 0)
         (kept-count 0))
    (dolist (file week-files)
      ;; Check if file has any records newer than cutoff
      (let* ((records (agent-shell-usage-history--load-week-file file))
             (has-recent (seq-some
                         (lambda (record)
                           (let ((record-time (agent-shell-usage-history--parse-date
                                              (map-elt record 'timestamp))))
                             (time-less-p cutoff-time record-time)))
                         records)))
        (if has-recent
            (cl-incf kept-count)
          (progn
            (delete-file file)
            (cl-incf deleted-count)
            (agent-shell-usage-history--debug "Deleted old file: %s" file)))))
    (if (> deleted-count 0)
        (message "Deleted %d old file(s), kept %d file(s)" deleted-count kept-count)
      (message "No files older than %d days" days))))

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

(defun agent-shell-usage-history-toggle-debug ()
  "Toggle debug output for usage history tracking."
  (interactive)
  (setq agent-shell-usage-history-debug (not agent-shell-usage-history-debug))
  (message "Usage history debug %s" (if agent-shell-usage-history-debug "enabled" "disabled"))
  (when agent-shell-usage-history-debug
    (agent-shell-usage-history--debug "Debug mode enabled")))

(defun agent-shell-usage-history--on-turn-complete (event-data)
  "Handle turn-complete event and save usage.
EVENT-DATA is an alist with :event and :data."
  (condition-case err
      (progn
        (agent-shell-usage-history--debug "turn-complete event received")
        (agent-shell-usage-history--debug "event-data type: %S" (type-of event-data))
        (agent-shell-usage-history--debug "event-data: %S" event-data)
        (if-let* ((data (map-elt event-data :data))
                  (usage (map-elt data :usage)))
            (agent-shell-usage-history--record-usage usage)
          (agent-shell-usage-history--debug "No usage data found in event-data")))
    (error
     (agent-shell-usage-history--debug "Error in on-turn-complete: %S" err)
     (message "Error in usage history turn-complete handler: %S" err))))

(defvar-local agent-shell-usage-history--subscription-token nil
  "Subscription token for this buffer's turn-complete handler.")

(defun agent-shell-usage-history--setup-subscription ()
  "Set up subscription to turn-complete events.
Should be called from agent-shell-mode-hook."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not agent-shell-usage-history--subscription-token))
    (setq agent-shell-usage-history--subscription-token
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :event 'turn-complete
           :on-event #'agent-shell-usage-history--on-turn-complete))))

;; Set up subscription hook
(add-hook 'agent-shell-mode-hook #'agent-shell-usage-history--setup-subscription)

(provide 'agent-shell-usage-history)
;;; agent-shell-usage-history.el ends here
