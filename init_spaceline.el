
(defface large-font
  '((t :height 140))
  "my large font face"
  )

(use-package spaceline :ensure t
  :config
  (use-package spaceline-config
    :config
    ;(spaceline-toggle-minor-modes-off)
    ;(spaceline-toggle-buffer-encoding-off)
    ;(spaceline-toggle-buffer-encoding-abbrev-off)
    ;(setq powerline-default-separator 'rounded)
    (setq powerline-default-separator 'zigzag)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (spaceline-define-segment line-column
      "The current line and column numbers."
      "%l:%c")
    (spaceline-define-segment datetime
      "current datetime"
      (propertize
       (format-time-string "%d %h %H:%M" )
       'face 'large-font))
    (spaceline-define-segment time
      "The current time."
      (format-time-string "%H:%M"))
    (spaceline-define-segment date
      "The current date."
      (format-time-string "%h %d"))
    (spaceline-define-segment fart
      "a big fart"
      "fart")
    (spaceline-toggle-time-on)
    (spaceline-toggle-buffer-id-on)
    (spaceline-emacs-theme 'date 'time 'datetime)))

(defun get-project-name ()
  "Gets the name of the project. If TRAMP, don't use projectile"
  (if (file-remote-p default-directory)
      "Remote"
    (if
	(and
	 (buffer-file-name) (projectile-project-root))
	(projectile-project-root))))


(spaceline-define-segment new-projectile-root
  "Prevents slowdowns when using TRAMP"
  (if (file-remote-p default-directory)
      "Remote"
    (when (fboundp 'projectile-project-name)
      (let ((project-name (projectile-project-name)))
	(unless (or (string= project-name "-")
                    (string= project-name (buffer-name)))
          project-name)))))
  
;; Create new segment with filename as large font
(spaceline-define-segment new-buffer-id
  "Name of buffer."
  (s-trim (spaceline--string-trim-from-center
           (propertize
            (if (file-remote-p default-directory)
		(powerline-buffer-id 'default-face)
	      (if
                  (and
                   (buffer-file-name) (projectile-project-root))
                  (file-relative-name buffer-file-name (projectile-project-root))
		(powerline-buffer-id 'default-face)))
              'face 'large-font)
            spaceline-buffer-id-max-length)))

(spaceline-compile
  ; left side
  '(((persp-name
      workspace-number
      window-number)
     :fallback evil-state
     :face highlight-face
     :priority 100)
    (anzu :priority 95)
    auto-compile
    ((buffer-modified buffer-size)
     :priority 98)
    ((new-projectile-root new-buffer-id)
     :priority 97
     :separator " | ")
    (major-mode :priority 79)
    (process :when active)
    ;; ((flycheck-error flycheck-warning flycheck-info)
    ;;  :when active
    ;;  :priority 89)
    ;; (minor-modes :when active
    ;;              :priority 9)
    (mu4e-alert-segment :when active)
    (erc-track :when active)
    (version-control;; :when active
                     :priority 78)
    (org-pomodoro :when active)
    (org-clock :when active)
    nyan-cat)
  ; right side
  '(which-function
    (python-pyvenv :fallback python-pyenv)
    (purpose :priority 94)
    ;; (battery :when active)
    (selection-info :priority 95)
    ;input-method
    ;; ((buffer-encoding-abbrev
    ;;   point-position
    ;;   line-column)
    ;;  :separator " | "
    ;;  :priority 96)
                                        ;(global :when active)
    (line-column :priority 90)
    ;; (datetime :priority 5)
    (buffer-position :priority 99)
    (hud :priority 99)))
