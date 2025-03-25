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

(defun evz/get-project-hierarchy ()
  (let ((hierarchy '())
        (current (projectile-project-root)))
    (while current
      (push current hierarchy)
      (setq current (projectile-project-root (file-name-directory (directory-file-name current)))))
    (reverse hierarchy)))

(defun evz/get-project-candidates ()
  "Return an alist of project candidates."
  (mapcar (lambda (proj)
            (cons (projectile-project-name proj) proj))
          (evz/get-project-hierarchy)))

(defun evz/helm-select-project (&optional prompt)
  (let* ((projects (evz/get-project-hierarchy))
         (candidates (mapcar (lambda (cand)
                               (cons (format "%-20s  %s" (car cand) (cdr cand))
                                     (cdr cand)))
                             (evz/get-project-candidates))))
    (cond
     ((null projects)
      nil)
     ((= (length projects) 1)
      (car projects))
     (t
      (helm :sources (helm-build-sync-source (or prompt "Select Project")
                       :candidates candidates
                       :action #'identity)
            :buffer "*helm project hierarchy*")))))

(defun evz/helm-projectile-ag (&rest args)
  (interactive "P")
  (let ((default-directory (or (evz/helm-select-project "Select Project for Ag") default-directory)))
    (apply #'helm-projectile-ag args)
    ))

;; remove key mapping to projectile-switch-buffer
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-x p b") nil)
  (define-key projectile-mode-map (kbd "C-x p s s") 'evz/helm-projectile-ag))


;; (defun evz/projectile-compilation-command (compile-dir)
;;   "echo hello")
;; (advice-add 'projectile-compilation-command :override 'evz/projectile-compilation-command)
;; (file-exists-p (concat (projectile-compilation-dir) "build.sh"))
;; (message projectile-project-compilation-cmddd)

;; (message  (if (projectile--cache-project-commands-p) "yes" "no"))
