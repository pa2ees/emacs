;; ----------------------------
;; Dependencies
;; ----------------------------
(require 'powerline)
;; (require 'powerline-evil) ;; for Evil state colors
(require 'projectile)
;; (require 'org-pomodoro nil t) ;; optional, only if installed
;; (require 'pyvenv nil t)       ;; optional, only if installed
;; (require 'vc)                 ;; version control info

(defface large-font
  '((t :height 140))
  "Face for large buffer/project names.")

(defface evz/powerline-active-hud
  '((t (:background "goldenrod" :foreground "white" :inherit mode-line)))
  "Powerline hud face."
  :group 'powerline)

;; ----------------------------
;; Custom segments
;; ----------------------------

(defun evz/powerline-project-name ()
  "Return project name or 'Remote' for TRAMP buffers."
  (if (file-remote-p default-directory)
      "Remote"
    (when (fboundp 'projectile-project-name)
      (let ((name (projectile-project-name)))
        (unless (or (string= name "-") (string= name (buffer-name)))
          name)))))

(defun evz/powerline-buffer-name-large ()
  "Return buffer name in large font, relative to project if possible."
  (propertize
   (if (file-remote-p default-directory)
       (buffer-name)
     (if (and (buffer-file-name) (projectile-project-root))
         (file-name-nondirectory (file-relative-name buffer-file-name (projectile-project-root)))
       (buffer-name)))
   'face 'large-font))

(defun evz/powerline-project-and-buffer ()
  "Return a string combining project name and buffer name.
If a project exists, separate with `|`. If no project, return just the buffer name."
  (let* ((project (evz/powerline-project-name))
         (buffer (evz/powerline-buffer-name-large)))
    (if (and project (not (string-empty-p project)))
        (concat project " | " buffer)
      buffer)))

;; (defun evz/powerline-line-column ()
;;   "Return line:column string."
;;   (format "%d:%d" (line-number-at-pos) (current-column)))

;; (defun evz/powerline-python-venv ()
;;   "Return current Python virtual environment if available."
;;   (when (boundp 'pyvenv-virtual-env-name)
;;     (when pyvenv-virtual-env-name
;;       (format "⎈ %s" pyvenv-virtual-env-name))))

;; (defun evz/powerline-org-pomodoro ()
;;   "Return Pomodoro status if org-pomodoro is active."
;;   (when (and (boundp 'org-pomodoro-timer) org-pomodoro-timer)
;;     (format "🍅 %s" (org-pomodoro-format-seconds org-pomodoro-timer))))

(require 'cl-lib)
(defun evz/powerline-optional-segments (prev-face alt-face next-face separator space)
  (let ((result nil)
        (seg-prev-face prev-face)
        (seg-this-face alt-face)
        (display-vc (powerline-vc))
        ;; (lhs-width (powerline-width lhs))
        ;; (rhs-width (powerline-width rhs))
        (margin 1))
    
    (when display-vc
      (let* ((vc-segments (list
                           (funcall separator seg-prev-face seg-this-face)
                           (powerline-vc seg-this-face 'l)
                           (powerline-raw " " seg-this-face)))
             (vc-width (powerline-width vc-segments))
             (result-width (powerline-width result)))
        ;; if the width is good, add the segments
        (when (< (+ space vc-width result-width margin) (window-width))
          (dolist (seg vc-segments)
            (push seg result))
          ;; swap prev and this face
          (cl-rotatef seg-prev-face seg-this-face))))

    (when (buffer-narrowed-p)
      (let* ((narrow-segments (list
                           (funcall separator seg-prev-face seg-this-face)
                           (powerline-narrow seg-this-face 'l)
                           (powerline-raw " " seg-this-face)))
             (narrow-width (powerline-width narrow-segments))
             (result-width (powerline-width result)))
        ;; if the width is good, add the segments
        (when (< (+ space narrow-width result-width margin) (window-width))
          (dolist (seg narrow-segments)
            (push seg result))
          ;; swap prev and this face
          (cl-rotatef seg-prev-face seg-this-face))))

    ;; add separator to the next face
    (push (funcall separator seg-prev-face next-face) result)
    (setq result (nreverse result))
    result))

(defun evz/powerline-build ()
  "Setup a custom mode-line"
  (interactive)
  (let* ((active (powerline-selected-window-active))
         (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
         (mode-line (if active 'mode-line 'mode-line-inactive))
         (face0 (if active 'powerline-active0 'powerline-inactive0))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (separator-left (intern (format "powerline-%s-%s"
                                         (powerline-current-separator)
                                         (car powerline-default-separator-dir))))
         (separator-right (intern (format "powerline-%s-%s"
                                          (powerline-current-separator)
                                          (cdr powerline-default-separator-dir))))
         (lhs (list ;; all fixed segments
               (powerline-raw "%*" face1 'l)
               (when powerline-display-buffer-size
                 (powerline-buffer-size face1 'l))
               (powerline-raw " " face1)
               (funcall separator-left face1 face0)
               (powerline-raw (evz/powerline-project-and-buffer) face0 'l)
               (powerline-raw " " face0)
               (funcall separator-left face0 face1)
               (powerline-major-mode face1 'l)
               (powerline-raw " " face1)))
         (rhs (list
               (funcall separator-right face2 face1)
               (powerline-raw global-mode-string face1 'r)
               (powerline-raw " %l:%c" face1 'r)
               (funcall separator-right face1 face0)
               (powerline-raw " %p" face0 'r)
               (powerline-hud 'evz/powerline-active-hud face2)))
         (lhs-size (powerline-width lhs))
         (rhs-size (powerline-width rhs))
         (lhs-dynamic
               ;; add optional segments
          (evz/powerline-optional-segments face1 face0 face2 separator-left (+ lhs-size rhs-size))))
    
    (concat (powerline-render lhs)
            (powerline-render lhs-dynamic)
            (powerline-fill face2 (powerline-width rhs))
            (powerline-render rhs))))

;; ----------------------------
;; Activate the mode-line
;; ----------------------------
(setq-default powerline-default-separator 'zigzag)
(setq-default mode-line-format
              '("%e" (:eval (evz/powerline-build))))
