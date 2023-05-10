(setq projectile-switch-project-action #'projectile-dired)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

(require 'helm-projectile)

(helm-projectile-on)


;; (defun evz/projectile-compilation-command (compile-dir)
;;   "echo hello")
;; (advice-add 'projectile-compilation-command :override 'evz/projectile-compilation-command)
;; (file-exists-p (concat (projectile-compilation-dir) "build.sh"))
;; (message projectile-project-compilation-cmddd)

;; (message  (if (projectile--cache-project-commands-p) "yes" "no"))
