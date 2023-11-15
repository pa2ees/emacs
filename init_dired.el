;; This advises functions to be aware of subdirs in dired mode
(defun dired-subdir-aware (orig-fun &rest args)
  (if (eq major-mode 'dired-mode)
      (let ((default-directory (dired-current-directory)))
        (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'read-file-name :around 'dired-subdir-aware)
(advice-add 'find-file-read-args :around 'dired-subdir-aware)
(advice-add 'dired-do-compress-to :around 'dired-subdir-aware)

(defun dired-sort-set-mode-line-extended ()
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let ((sorting-reversed (string-match-p (dired-get-match-string-for-switch "r") dired-actual-switches))
                case-fold-search)
            (let ((rev-str (if sorting-reversed
                               " Rev "
                             " ")))

              (cond ((string-match-p
                      (dired-get-match-string-for-switch "X") dired-actual-switches)
                     (concat "Dired by Ext" rev-str))
                    ((string-match-p
                      (dired-get-match-string-for-switch "U") dired-actual-switches)
                     (concat "Dired by dir order" rev-str))
                    ((string-match-p
                      (dired-get-match-string-for-switch "S") dired-actual-switches)
                     (concat "Dired by Size" rev-str))
                    ((string-match-p
                      (dired-get-match-string-for-switch "t") dired-actual-switches)
                     (concat "Dired by Date" rev-str))
                    (t
                     (concat "Dired by Name" rev-str))))))
    (force-mode-line-update)))

(advice-add 'dired-sort-set-mode-line :override 'dired-sort-set-mode-line-extended)


(defun dired-remove-sort-all ()
  (progn
    (dired-remove-sort "t")
    (dired-remove-sort "S")
    (dired-remove-sort "X")
    (dired-remove-sort "U")))

(defun dired-get-match-string-for-switch (arg)
  (if (> (string-to-char arg) (string-to-char "Z"))
      (concat "\\(\\`\\| \\)-\\([a-"
              (char-to-string (- (string-to-char arg) 1))
              (char-to-string (+ (string-to-char arg) 1))
              "-zA-Z]*\\)\\("
              arg
              "\\)\\([^ ]*\\)")
    (concat "\\(\\`\\| \\)-\\([a-"
            (char-to-string (- (string-to-char arg) 1))
            (char-to-string (+ (string-to-char arg) 1))
            "-zA-Z]*\\)\\("
            arg
            "\\)\\([^ ]*\\)")))

(defun dired-remove-switch (arg)
  (let ((switch-regexp (dired-get-match-string-for-switch arg))
                       case-fold-search)
    ;; Remove the switch.
    (while (string-match switch-regexp dired-actual-switches)
      (if (and (equal (match-string 2 dired-actual-switches) "")
               (equal (match-string 4 dired-actual-switches) ""))
          ;; Remove a stand-alone switch.
          (progn
            (setq dired-actual-switches
                  (replace-match "" t t dired-actual-switches)))
        ;; Remove a switch of the form -XaY for some X and Y where a is the switch to be removed.
        (setq dired-actual-switches
              (replace-match "" t t dired-actual-switches 3))))))

(defun dired-add-switch (arg)
  (let ((switch-regexp "\\(.*?\\)\\(\\`\\| +\\)\\(-[[:alnum:]]+\\)\\(.*\\)"))
    (setq dired-actual-switches
          (if (string-match switch-regexp dired-actual-switches)
              (concat (match-string 1 dired-actual-switches)
                      (match-string 2 dired-actual-switches)
                      (match-string 3 dired-actual-switches)
                      arg
                      (match-string 4 dired-actual-switches))
            (concat dired-actual-switches
                    " -"
                    arg))))
  (dired-sort-set-mode-line))

(defun dired-switch-toggle (arg)
  "Toggles dired switch, if it exists"
  (let ((switch-exists (string-match-p (dired-get-match-string-for-switch arg) dired-actual-switches))
        case-fold-search)
    (if switch-exists
        (progn
          (dired-remove-switch arg))
      (dired-add-switch arg))))


(defun dired-do-sort (arg)
  "Adds sorting to dired buffer based on value of arg"
  (dired-add-switch arg))
  
(defun dired-remove-sort (arg)
  "Removes sorting in dired buffer based on value of arg"
  (dired-remove-switch arg))

(defun dired-sort-file-extension ()
  "Sort dired buffer by file extension"
  (interactive)
  (dired-remove-sort-all)
  (dired-do-sort "X")
  (revert-buffer)
  (message "Sorting by File Extension"))

(defun dired-sort-filename ()
  "Sort dired buffer by filename"
  (interactive)
  (dired-remove-sort-all)
  (dired-sort-set-mode-line)
  (revert-buffer)
  (message "Sorting by Filename"))

(defun dired-sort-directory-order ()
  "Sort dired buffer by directory order"
  (interactive)
  (dired-remove-sort-all)
  (dired-do-sort "U")
  (revert-buffer)
  (message "Sorting by Directory Order"))

(defun dired-sort-size ()
  "Sort dired buffer by file size"
  (interactive)
  (dired-remove-sort-all)
  (dired-do-sort "S")
  (revert-buffer)
  (message "Sorting by File Size"))

(defun dired-sort-time ()
  "Sort dired buffer by timestamp"
  (interactive)
  (dired-remove-sort-all)
  (dired-do-sort "t")
  (revert-buffer)
  (message "Sorting by File Modification Date/Time"))

(defun dired-sort-reversed-toggle ()
  "Cause all sorting to be reversed in dired buffer"
  (interactive)
  (dired-switch-toggle "r")
  (dired-sort-set-mode-line)
  (revert-buffer)
  (message "Toggled Reverse Sorting"))

(defun dired-reset-switches ()
  "Resets all dired sorting and filtering switches to default in dired buffer"
  (interactive)
  (setq dired-actual-switches "-Blh --group-directories-first")
  (dired-sort-set-mode-line)
  (revert-buffer)
  (message "Reset dired switches"))

(defun dired-show-all-toggle ()
  "Shows all files (removes all filters) in dired buffer"
  (interactive)
  (dired-remove-switch "B")
  (dired-remove-switch "A")
  (dired-switch-toggle "a")
  (revert-buffer))

(defun dired-human-readable-toggle ()
  "Toggle human-readability in dired buffer"
  (interactive)
  (dired-switch-toggle "h")
  (revert-buffer))

(defun dired-owner-toggle ()
  "Toggle owner column"
  (interactive)
  (dired-switch-toggle "g")
  (revert-buffer))

(defun dired-group-toggle ()
  "Toggle group column"
  (interactive)
  (dired-switch-toggle "G")
  (revert-buffer))

(defun dired-backup-toggle ()
  "Toggle showing backup files (ie *~)"
  (interactive)
  (dired-switch-toggle "B")
  (revert-buffer))

(defvar my-dired-mode-map 
  (let ((map (make-sparse-keymap)))
    ;; sort keybindings
    (define-key map (kbd "M-s s x") 'dired-sort-file-extension)
    (define-key map (kbd "M-s s D") 'dired-sort-directory-order)
    (define-key map (kbd "M-s s f") 'dired-sort-filename)
    (define-key map (kbd "M-s s n") 'dired-sort-filename)
    (define-key map (kbd "M-s s s") 'dired-sort-size)
    (define-key map (kbd "M-s s t") 'dired-sort-time)
    (define-key map (kbd "M-s s d") 'dired-sort-time)
    (define-key map (kbd "M-s s r") 'dired-sort-reversed-toggle)
    (define-key map (kbd "M-s r") 'dired-reset-switches)
    (define-key map (kbd "M-s a") 'dired-show-all-toggle)
    (define-key map (kbd "M-s H") 'dired-human-readable-toggle)
    (define-key map (kbd "M-s g") 'dired-owner-toggle)
    (define-key map (kbd "M-s G") 'dired-group-toggle)
    (define-key map (kbd "M-s b") 'dired-backup-toggle)
    
    ;; filter keybindings
    (define-key map (kbd "f") 'dired-narrow)
    
    ;; subtree keybindings
    (define-key map (kbd "s s") 'dired-subtree-toggle)
    (define-key map (kbd "s i") 'dired-subtree-insert)
    (define-key map (kbd "s k") 'dired-subtree-remove)
    (define-key map (kbd "s p") 'dired-subtree-up)
    (define-key map (kbd "s n") 'dired-subtree-down)
    (define-key map (kbd "s c") 'dired-subtree-cycle)
    (define-key map (kbd "s t") 'dired-subtree-toggle)
    (define-key map (kbd "s M-p") 'dired-subtree-beginning)
    (define-key map (kbd "s M-n") 'dired-subtree-end)
    map)
  "Custom keymap for Dired mode.")

(add-hook 'dired-mode-hook (lambda ()
                             (dired-reset-switches)
                             ;; add dired-mode-map as parent to keep existing key bindings
                             (set-keymap-parent my-dired-mode-map dired-mode-map)

                             (use-local-map my-dired-mode-map)

                             ;; (linum-mode -1)))
                             (display-line-numbers-mode -1)))
