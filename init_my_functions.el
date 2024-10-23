(defun clang-format-enable-this-buffer ()
  "Enable clang-formatting on save for this buffer"
  (add-hook 'before-save-hook #'evz/clang-format-this-buffer nil t))

(defun clang-format-disable-this-buffer ()
  "Disable clang-formatting on save for this buffer"
  (remove-hook 'before-save-hook #'evz/clang-format-this-buffer t))

(defun evz/clang-format-this-buffer ()
  "Clang-format this buffer"
  (let ((spec-file-dir (locate-dominating-file "." ".clang-format")))
    (if spec-file-dir
        (message "Clang-formatting buffer using %s.clang-format" spec-file-dir)
      (message "Clang-formatting buffer (.clang-format spec not found)"))
    (clang-format-buffer)))
      
(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (progn
                (when (locate-dominating-file "." ".clang-format")
                  (message "Running Clang-Format on buffer before save")
                  (clang-format-buffer))
                ;; Continue to save.
                nil))
            nil
            ;; Buffer local hook.
            t))


;; kill-line without saving to kill ring
(defun kill-line-no-save-to-kill-ring ()
  (interactive)
  (progn
    (kill-line)
    (when kill-ring
      (setq kill-ring (cdr kill-ring)))
    (when kill-ring-yank-pointer
      (setq kill-ring-yank-pointer kill-ring))))


;; reverting buffers
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t t)
    (message "Buffer reverted!!"))

(defun kill-buffer-other-window-and-close()
  "If there are multiple windows, then close the other window and kill the buffer in it also."
  (interactive)
  (let ((local-buff (buffer-name)))
    (other-window 1)
    (let ((other-window-buff (buffer-name)))
      (other-window -1)
      (if (string= local-buff other-window-buff)
          (progn
            (other-window 1)
            (if (not (one-window-p))
                (delete-window)))
        (progn
          (other-window 1)
          (kill-this-buffer)
          (if (not (one-window-p))
              (delete-window)))))))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
