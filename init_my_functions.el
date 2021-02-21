
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
