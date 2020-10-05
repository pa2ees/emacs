(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

(require 'helm-projectile)
(helm-projectile-on)

