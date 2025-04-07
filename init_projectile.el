(setq projectile-switch-project-action #'projectile-dired)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

(require 'helm-projectile)

(helm-projectile-on)

;; (load-file (concat user-emacs-init-directory "project_comphist.el"))

;; This is temporary until project-comphist is available on MELPA
(push "~/projects/projectile-comphist/" load-path)

(require 'projectile-comphist)

;; (advice-add 'projectile-compile-project :override 'projectile-comphist-compile)

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

(load-file (concat user-emacs-init-directory "init_helm_projectile_ag.el"))

(defun evz/helm-select-project--helm-projectile-compile-project (&rest args)
  (interactive "P")
  (let ((default-directory (or (evz/helm-select-project "Select Project for compiling") default-directory)))
    (apply #'projectile-comphist-compile args)))
    

;; remove key mapping to projectile-switch-buffer
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-x p b") nil)
  (define-key projectile-mode-map (kbd "C-x p s s") 'evz/helm-select-project--helm-projectile-ag)
  (define-key projectile-mode-map (kbd "C-x p c c") 'evz/helm-select-project--helm-projectile-compile-project))

