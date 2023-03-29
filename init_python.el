
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; (add-hook 'inferior-python-mode-hook
;;         (lambda ()
;;           (setq company-mode nil)))


;; ;; jedi - python completion
;; (add-hook 'python-mode-hook 'jedi:setup)
;; ;; (setq jedi:complete-on-dot t)

;; ;; (setq python-shell-interpreter "ipython")

;; (add-hook 'python-mode-hook
;; 	  (lambda () (progn
;; 		       (jedi:setup)
;; 		       (setq jedi:complete-on-dot t)
;; 		       (setq python-indent-offset 4))))

;;
;; really didn't like using company for python completion
;;

;; (use-package company
;;   :ensure t
;;   :config
;;   (setq company-frontends nil)
;;   (add-hook 'after-init-hook 'global-company-mode))

;; (use-package helm-company
;;   :init (progn
;;           (defun my:code::helm-company-complete ()
;;             (interactive)
;;             (when (company-complete) (helm-company)))
;;           (add-to-list 'completion-at-point-functions
;;                        #'comint-dynamic-complete-filename))
;;   :general (general-def
;;   	:keymaps '(company-mode-map company-active-map)
;;              "TAB" #'helm-company
;;              "<tab>" #'helm-company))

;; (use-package helm-company
;; 	     :ensure t)

  
;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-jedi)
;;   (setq jedi:complete-on-dot t))

;;(if (version< emacs-version "26.3")
;;    (progn
      
      ;; ;; jedi - python completion
      ;; (add-hook 'python-mode-hook 'jedi:setup)
      ;; ;; (setq jedi:complete-on-dot t)
      
      ;; ;; (setq python-shell-interpreter "ipython")
      
      ;; (add-hook 'python-mode-hook
      ;;           (lambda () (progn
      ;;                        (jedi:setup)
      ;;                        (setq jedi:complete-on-dot t)
      ;;                        (setq python-indent-offset 4))))
;;    (progn
      ;; (use-package company
      ;;   :ensure t
      ;;   :config
       
      ;;   (add-hook 'after-init-hook 'global-company-mode))

      ;; (use-package helm-company
      ;;   :ensure t
      ;;   :init (progn
      ;;           (defun my:code::helm-company-complete ()
      ;;             (interactive)
      ;;             (when (company-complete) (helm-company)))
      ;;           (add-to-list 'completion-at-point-functions
      ;;                        #'comint-dynamic-complete-filename))
      ;;   :general (general-def
      ;;             :keymaps '(company-mode-map company-active-map)
      ;;             "TAB" #'helm-company
      ;;             "<tab>" #'helm-company))
     
      ;; (use-package company-jedi
      ;;   :ensure t
      ;;   :config
      ;;   (add-to-list 'company-backends 'company-jedi)
      ;;   (setq jedi:complete-on-dot t))))
      ;; ELPY doesn't work with linum mode???
      ;; (use-package elpy
      ;;   :ensure t
      ;;   :config
      ;;   (elpy-enable))))

;; NOTE: if python gives "error in process sentinel..." run M-x elpy-rpc-reinstall-virtualenv
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))
