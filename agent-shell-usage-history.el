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

(defvar-local agent-shell-usage-history--session-id nil
  "Unique session ID for this agent-shell buffer.")

(defvar-local agent-shell-usage-history--project-root nil
  "Project root directory for this agent-shell session.")

(defvar-local agent-shell-usage-history--last-cumulative-usage nil
  "Last cumulative usage data from agent-shell, used to calculate per-turn deltas.")

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
USAGE should be an alist with token counts, context, and cost.
Calculates per-turn deltas from cumulative usage data."
  (condition-case err
      (progn
        (agent-shell-usage-history--debug "record-usage called with: %S" usage)
        (if (not usage)
            (agent-shell-usage-history--debug "No usage data provided, skipping")
          (let ((total-tokens (or (map-elt usage :total-tokens) 0)))
            (if (<= total-tokens 0)
                (agent-shell-usage-history--debug "Total tokens is %d, skipping record" total-tokens)
              ;; Calculate deltas only for cumulative fields (cost)
              ;; Tokens appear to be per-turn already based on data inspection
              (let* ((prev agent-shell-usage-history--last-cumulative-usage)
                     (current-cost (or (map-elt usage :cost-amount) 0))
                     (prev-cost (if prev (or (map-elt prev :cost-amount) 0) 0))
                     (delta-cost (- current-cost prev-cost))
                     ;; These appear to be per-turn already, use as-is
                     (total-tokens (or (map-elt usage :total-tokens) 0))
                     (input-tokens (or (map-elt usage :input-tokens) 0))
                     (output-tokens (or (map-elt usage :output-tokens) 0))
                     (thought-tokens (or (map-elt usage :thought-tokens) 0))
                     (cached-read-tokens (or (map-elt usage :cached-read-tokens) 0))
                     (cached-write-tokens (or (map-elt usage :cached-write-tokens) 0))
                     (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
                     (record (list (cons 'timestamp timestamp)
                                  (cons 'session_id agent-shell-usage-history--session-id)
                                  (cons 'project_root agent-shell-usage-history--project-root)
                                  (cons 'total_tokens total-tokens)
                                  (cons 'input_tokens input-tokens)
                                  (cons 'output_tokens output-tokens)
                                  (cons 'thought_tokens thought-tokens)
                                  (cons 'cached_read_tokens cached-read-tokens)
                                  (cons 'cached_write_tokens cached-write-tokens)
                                  (cons 'context_used (or (map-elt usage :context-used) 0))
                                  (cons 'context_size (or (map-elt usage :context-size) 0))
                                  (cons 'cost_amount delta-cost)
                                  (cons 'cost_currency (or (map-elt usage :cost-currency) "USD")))))
                (agent-shell-usage-history--debug "Buffer: %s" (buffer-name))
                (agent-shell-usage-history--debug "Prev object: %S" prev)
                (agent-shell-usage-history--debug "Cost calculation: current=$%.2f prev=$%.2f delta=$%.2f"
                                                 current-cost prev-cost delta-cost)
                (agent-shell-usage-history--debug "Recording per-turn usage: tokens=%d cost=$%.2f timestamp=%s"
                                                 total-tokens delta-cost timestamp)
                (agent-shell-usage-history--append-to-week-file record)
                ;; Save current cumulative for next delta calculation (make a copy!)
                (setq agent-shell-usage-history--last-cumulative-usage (copy-tree usage))
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
        (unique-sessions (make-hash-table :test 'equal)))
    (dolist (record records)
      (cl-incf total-tokens (or (map-elt record 'total_tokens) 0))
      (cl-incf input-tokens (or (map-elt record 'input_tokens) 0))
      (cl-incf output-tokens (or (map-elt record 'output_tokens) 0))
      (cl-incf thought-tokens (or (map-elt record 'thought_tokens) 0))
      (cl-incf cached-read-tokens (or (map-elt record 'cached_read_tokens) 0))
      (cl-incf total-cost (or (map-elt record 'cost_amount) 0))
      (when-let ((cur (map-elt record 'cost_currency)))
        (setq currency cur))
      ;; Track unique session IDs
      (when-let ((session-id (map-elt record 'session_id)))
        (puthash session-id t unique-sessions)))
    (list :total-tokens total-tokens
          :input-tokens input-tokens
          :output-tokens output-tokens
          :thought-tokens thought-tokens
          :cached-read-tokens cached-read-tokens
          :cost-amount total-cost
          :cost-currency currency
          :session-count (hash-table-count unique-sessions))))

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

(defun agent-shell-usage-history--abbreviate-project (project-root)
  "Abbreviate PROJECT-ROOT for display (e.g., /home/user/projects/foo -> ~/projects/foo)."
  (if project-root
      (abbreviate-file-name project-root)
    "unknown"))

(defun agent-shell-usage-history--format-current-session ()
  "Format current session usage statistics."
  (let ((output ""))
    (setq output (concat output
                        (propertize "=== Current Session ===\n" 'face 'bold)))
    ;; Find the agent-shell buffer
    (let ((agent-shell-buffer (seq-find (lambda (buf)
                                          (with-current-buffer buf
                                            (derived-mode-p 'agent-shell-mode)))
                                        (buffer-list))))
      (if agent-shell-buffer
          (with-current-buffer agent-shell-buffer
            (let* ((usage (map-elt agent-shell--state :usage))
                   (total-tokens (or (map-elt usage :total-tokens) 0))
                   (input-tokens (or (map-elt usage :input-tokens) 0))
                   (output-tokens (or (map-elt usage :output-tokens) 0))
                   (thought-tokens (or (map-elt usage :thought-tokens) 0))
                   (cached-read-tokens (or (map-elt usage :cached-read-tokens) 0))
                   (context-used (or (map-elt usage :context-used) 0))
                   (context-size (or (map-elt usage :context-size) 0))
                   (cost (or (map-elt usage :cost-amount) 0))
                   (currency (or (map-elt usage :cost-currency) "USD"))
                   (session-id agent-shell-usage-history--session-id)
                   (project agent-shell-usage-history--project-root))
              (setq output (concat output
                                  (format "Session: %s\n"
                                         (agent-shell-usage-history--get-session-id-suffix session-id))
                                  (format "Project: %s\n"
                                         (agent-shell-usage-history--abbreviate-project project))
                                  (format "Total tokens: %s (cached: %s)\n"
                                         (agent-shell--format-number-compact total-tokens)
                                         (agent-shell--format-number-compact cached-read-tokens))
                                  (format "  in: %s, out: %s\n"
                                         (agent-shell--format-number-compact input-tokens)
                                         (agent-shell--format-number-compact output-tokens))
                                  (when (> thought-tokens 0)
                                    (format "Thought tokens: %s\n"
                                           (agent-shell--format-number-compact thought-tokens)))
                                  (format "Context: %s / %s (%.1f%%)\n"
                                         (agent-shell--format-number-compact context-used)
                                         (agent-shell--format-number-compact context-size)
                                         (if (> context-size 0)
                                             (* 100.0 (/ (float context-used) context-size))
                                           0.0))
                                  (format "Cost: $%.2f\n" cost)))))
        (setq output (concat output "No active agent-shell session\n"))))
    (concat output "\n")))

(defun agent-shell-usage-history--get-today-records ()
  "Get all records from today, grouped by project."
  (let* ((history (agent-shell-usage-history--load-history))
         (now (current-time))
         (decoded (decode-time now))
         (today-start (encode-time 0 0 0
                                  (nth 3 decoded)  ; day
                                  (nth 4 decoded)  ; month
                                  (nth 5 decoded)  ; year
                                  (nth 8 decoded))) ; timezone
         (today-records (agent-shell-usage-history--filter-by-period history today-start))
         (by-project (make-hash-table :test 'equal)))
    ;; Group by project
    (dolist (record today-records)
      (let* ((project (or (map-elt record 'project_root) "unknown"))
             (existing (gethash project by-project)))
        (puthash project (cons record existing) by-project)))
    by-project))

(defun agent-shell-usage-history--get-session-id-suffix (session-id)
  "Get the last part of SESSION-ID (after the last dash)."
  (if (and session-id (string-match "-\\([^-]+\\)$" session-id))
      (match-string 1 session-id)
    (or session-id "unknown")))

(defun agent-shell-usage-history--format-today-sessions ()
  "Format today's sessions in a table sorted by project."
  (let ((by-project (agent-shell-usage-history--get-today-records))
        (output "")
        (all-sessions '()))
    (setq output (concat output
                        (propertize (format "=== Today (%s) ===\n"
                                          (format-time-string "%Y-%m-%d"))
                                   'face 'bold)))
    (if (zerop (hash-table-count by-project))
        (setq output (concat output "No sessions today\n"))
      ;; Collect all sessions with their project info
      (maphash
       (lambda (project records)
         ;; Group records by session_id
         (let ((by-session (make-hash-table :test 'equal)))
           (dolist (record records)
             (let* ((session-id (or (map-elt record 'session_id) "unknown"))
                    (existing (gethash session-id by-session)))
               (puthash session-id (cons record existing) by-session)))

           ;; Collect session data
           (maphash
            (lambda (session-id session-records)
              (let ((session-input 0)
                    (session-output 0)
                    (session-cost 0.0)
                    (turn-count (length session-records)))
                ;; Aggregate session data
                (dolist (record session-records)
                  (cl-incf session-input (or (map-elt record 'input_tokens) 0))
                  (cl-incf session-output (or (map-elt record 'output_tokens) 0))
                  (cl-incf session-cost (or (map-elt record 'cost_amount) 0)))

                ;; Add to all-sessions list
                (push (list :session-id session-id
                           :project project
                           :input session-input
                           :output session-output
                           :cost session-cost
                           :turns turn-count)
                      all-sessions)))
            by-session)))
       by-project)

      ;; Sort by project
      (setq all-sessions (sort all-sessions
                              (lambda (a b)
                                (string< (plist-get a :project)
                                        (plist-get b :project)))))

      ;; Display table header
      (setq output (concat output
                          (format "%-10s %10s %10s %6s  %s\n"
                                 "Session" "Tokens" "Cost" "Turns" "Project")
                          (format "%s\n"
                                 (make-string 70 ?-))))

      ;; Display each session
      (dolist (session all-sessions)
        (setq output (concat output
                            (format "%-10s %10s %10s %6d  %s\n"
                                   (agent-shell-usage-history--get-session-id-suffix
                                    (plist-get session :session-id))
                                   (agent-shell--format-number-compact
                                    (+ (plist-get session :input)
                                       (plist-get session :output)))
                                   (format "$%.2f" (plist-get session :cost))
                                   (plist-get session :turns)
                                   (agent-shell-usage-history--abbreviate-project
                                    (plist-get session :project))))))

      (setq output (concat output "\n")))
    output))

(defun agent-shell-usage-history--format-stats (stats period-name)
  "Format STATS for display with PERIOD-NAME label."
  (if (null stats)
      (format "No usage data for %s" period-name)
    (let* ((sessions (plist-get stats :session-count))
           (total (plist-get stats :total-tokens))
           (cost (plist-get stats :cost-amount)))
      (format "Sessions: %d | Total: %s | Cost: $%.2f"
              sessions
              (agent-shell--format-number-compact total)
              cost))))

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
        ;; Current session
        (insert (agent-shell-usage-history--format-current-session))
        ;; Today's sessions grouped by project
        (insert (agent-shell-usage-history--format-today-sessions))
        (insert "\n")
        ;; Period summaries
        (insert (propertize "=== Last 24 Hours ===\n" 'face 'bold))
        (insert (agent-shell-usage-history--format-stats daily "Last 24 Hours"))
        (insert "\n\n")
        (insert (propertize "=== Last 7 Days ===\n" 'face 'bold))
        (insert (agent-shell-usage-history--format-stats weekly "Last 7 Days"))
        (insert "\n\n")
        (insert (propertize "=== Last 30 Days ===\n" 'face 'bold))
        (insert (agent-shell-usage-history--format-stats monthly "Last 30 Days"))
        (insert "\n\n")
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

(defun agent-shell-usage-history--generate-session-id ()
  "Generate a unique session ID."
  (format "%s-%d-%04x"
          (format-time-string "%Y%m%d-%H%M%S")
          (emacs-pid)
          (random 65536)))

(defun agent-shell-usage-history--setup-subscription ()
  "Set up subscription to turn-complete events and initialize session data.
Should be called from agent-shell-mode-hook."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not agent-shell-usage-history--subscription-token))
    ;; Generate session ID and capture project root
    (unless agent-shell-usage-history--session-id
      (setq agent-shell-usage-history--session-id
            (agent-shell-usage-history--generate-session-id))
      (agent-shell-usage-history--debug "Generated session ID: %s"
                                       agent-shell-usage-history--session-id))

    (unless agent-shell-usage-history--project-root
      (setq agent-shell-usage-history--project-root
            (or (and (fboundp 'projectile-project-root)
                     (projectile-project-root))
                default-directory))
      (agent-shell-usage-history--debug "Captured project root: %s"
                                       agent-shell-usage-history--project-root))

    ;; Initialize cumulative usage tracker to zeros at session start
    (unless agent-shell-usage-history--last-cumulative-usage
      (setq agent-shell-usage-history--last-cumulative-usage
            (list (cons :total-tokens 0)
                  (cons :input-tokens 0)
                  (cons :output-tokens 0)
                  (cons :thought-tokens 0)
                  (cons :cached-read-tokens 0)
                  (cons :cached-write-tokens 0)
                  (cons :cost-amount 0)
                  (cons :cost-currency "USD")))
      (agent-shell-usage-history--debug "Initialized last cumulative usage to zeros"))

    ;; Subscribe to turn-complete events
    (setq agent-shell-usage-history--subscription-token
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :event 'turn-complete
           :on-event #'agent-shell-usage-history--on-turn-complete))))

;; Set up subscription hook
(add-hook 'agent-shell-mode-hook #'agent-shell-usage-history--setup-subscription)

(provide 'agent-shell-usage-history)
;;; agent-shell-usage-history.el ends here
