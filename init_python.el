
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; jedi - python completion
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; (setq python-shell-interpreter "ipython")

(add-hook 'python-mode-hook
	  (lambda () (progn
		       (jedi:setup)
		       (setq jedi:complete-on-dot t)
		       (setq python-indent-offset 4))))
