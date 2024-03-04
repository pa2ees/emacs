(setq projectile-switch-project-action #'projectile-dired)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

(require 'helm-projectile)

(helm-projectile-on)

;; (load-file (concat user-emacs-init-directory "project_comphist.el"))

;; This is temporary until project-comphist is available on MELPA
(push "~/projects/projectile-comphist/" load-path)

(require 'projectile-comphist)

(advice-add 'projectile-compile-project :override 'projectile-comphist-compile)



;; (defun evz/projectile-compilation-command (compile-dir)
;;   "echo hello")
;; (advice-add 'projectile-compilation-command :override 'evz/projectile-compilation-command)
;; (file-exists-p (concat (projectile-compilation-dir) "build.sh"))
;; (message projectile-project-compilation-cmddd)

;; (message  (if (projectile--cache-project-commands-p) "yes" "no"))
