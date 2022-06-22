;; Place the following in the default .emacs file
;; which is usually found at c:\users\youruser\AppData\Roaming

;; ;; For Windows
;; (setq user-init-file "C:/_projects/emacs/.emacs")
;; (setq user-emacs-directory "C:/_projects/emacs/.emacs.d/")
;; (setq user-emacs-init-directory "C:/_projects/emacs/")

;; ;; For Linux
;; (setq user-init-file "~/projects/emacs/.emacs")
;; (setq user-emacs-directory "~/projects/emacs/.emacs.d/")
;; (setq user-emacs-init-directory "~/projects/emacs/")

;; ;;(setenv "HOME" "C:/_projects/emacs")
;; ;; the setenv "Home" really screws things up, and you can actually never
;; ;; get back to this file.
;; ;; to customize the init file, please manually visit user-init-file
;; (load user-init-file)

;; C:/_projects/emacs/.emacs should be set to wherever the .emacs file actually is.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" default))
 '(inhibit-startup-screen t)
 '(logview-additional-level-mappings
   '(("imsar_levels"
      (error "error")
      (warning "warning")
      (information "info")
      (debug "debug")
      (trace "trace")
      (aliases))))
 '(logview-additional-submodes
   '(("imsar"
      (format . "[TIMESTAMP][LEVEL][NAME]")
      (levels . "imsar_levels")
      (timestamp "imsar_timestamp")
      (aliases))))
 '(logview-additional-timestamp-formats
   '(("imsar_timestamp"
      (java-pattern . "HH:mm:ss.SSS")
      (datetime-options))))
 '(package-selected-packages
   '(graphviz-dot-mode yaml-mode log4j-mode logview cmake-mode xterm-color helm-ag clang-format clang-format+ helm-gtags ggtags helm-lsp elpy general company company-jedi yasnippet yasnippet-snippets spaceline smart-mode-line-powerline-theme smart-mode-line helm-projectile projectile treemacs treemacs-projectile swiper swiper-helm helm dired-sidebar dired-toggle diredfl jedi smartparens magit highlight-parentheses abyss-theme)))

(package-initialize)


;; TODO
;; flycheck (syntax checking)
;; company (complete-anything)
  ;; including company-jedi (already using jedi) ??
  ;; use irony with this?
  ;; works with helm!
;; elpy for python stuff?

;; https://trivialfis.github.io/emacs/2017/08/02/C-C++-Development-Environment-on-Emacs.html

;; check out ibuffer-projectile or ibuffer-vc (ibuffer version control)

;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; installs packages that are needed, but not installed
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (progn
      (package-refresh-contents)
      (package-install package))))

(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (progn
      (package-refresh-contents)
      (package-install package))))

(use-package general
  :ensure t)

(use-package neotree
  :ensure t)

(use-package treemacs
  :ensure t
  :bind
  (:map global-map
        ([f3] . treemacs))
  :config
  (setq treemacs-is-never-other-window t
        treemacs-default-visit-action 'treemacs-visit-node-in-most-recently-used-window))


;; misc setup things
(setq inhibit-startup-screen t)
(put 'narrow-to-region 'disabled nil)
;; prevent ffap (find file at point) from pinging weird websites if the thing at point
;; looks something like a web address. ie test.my (looks for some server in Malaysia)???
(setq ffap-machine-p-known 'reject)

;; make default font height bigger!
(set-face-attribute 'default nil :height 120)
;;(set-face-attribute 'default nil :height 150)

;; programming files basic stuff
(setq c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)


;; (setq scroll-step 1)
(setq scroll-margin 4) ;; sets how far away from top or bottom we start to scroll
(setq scroll-conservatively 1) ;; seems to work ok
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Truncate lines always
(set-default 'truncate-lines t)


;; Themes
;; (load-theme 'abyss t t)
;; (enable-theme 'abyss)
(load-theme 'manoj-dark t)
(enable-theme 'manoj-dark)
;; (set-background-color "black")
;; (let ((frame-background-mode 'light)) (frame-set-background-mode nil))

;; ************* COMPANY MODE STUFF ****************


;; ************* LINUM STUFF ***********************
(global-linum-mode)
;; (setq linum-format "%d ")

;; image mode and doc view mode don't like linum mode
(add-hook 'doc-view-mode-hook (lambda () (linum-mode -1)))
(add-hook 'image-mode-hook (lambda () (linum-mode -1)))


;; ************* CONFLUENCE STUFF ******************
;; unfortunately this is not working
;; (add-to-list 'load-path (concat user-emacs-init-directory "confluence/confluence-el"))
;; (require 'confluence)
;; (setq confluence-url "https://confluence.imsar.us")

;; ************* ORG MODE STUFF ********************
(global-set-key (kbd "C-c l") 'org-store-link)


;; ************* DIRED MODE STUFF ******************
(load-file (concat user-emacs-init-directory "init_dired.el"))


;; ************* PYTHON STUFF **********************
;; something is wrong in the init_python.el file that slows down emacs
;; when writing python
(load-file (concat user-emacs-init-directory "init_python.el"))


;; ************* PDF STUFF *************************
;; (load-file (concat user-emacs-init-directory "init_pdf.el"))

;; ************* COLORING STUFF ********************
;; this helps colorize the compilation terminal
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; ************* PARENS STUFF **********************
;; smart-parens were getting kinda annoying
;; (require 'smartparens-config)
;; (smartparens-global-mode 1)
;; (show-smartparens-global-mode -1)

;; highlighting parens is good
(global-highlight-parentheses-mode)


;; ************* MY FUNCTIONS **********************
(load-file (concat user-emacs-init-directory "init_my_functions.el"))

;; bind keyboard commands to my functions
(global-set-key (kbd "C-S-k") 'kill-line-no-save-to-kill-ring)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-<f5>") 'revert-all-buffers)
(global-set-key (kbd "C-x K") 'kill-buffer-other-window-and-close)
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-F") 'forward-symbol)
(global-set-key (kbd "s-n") 'set-frame-name)

;; ************** CLANG-FORMAT *********************
;; Run this for each mode you want to use the hook.
(add-hook 'c-mode-common-hook (lambda () (clang-format-save-hook-for-this-buffer)))
;; (add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
;; (add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))


(setq-default tab-width 4)

;; ************* YASNIPPET STUFF *******************
;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (use-package yasnippet-snippets
;;     :ensure t)
;;   (setq yas-snippet-dirs (cons (concat user-emacs-init-directory "snippets/") yas-snippet-dirs))
;;   (yas-reload-all))

;; this got annoying, better to enable when needed? or in the mode needed?
;; (yas-global-mode 1)


;; ************* C++ STUFF *************************

(defun toggle-folding ()
  (interactive)
  (progn
    (beginning-of-line)
    (c-end-of-defun)
    (c-beginning-of-defun)
    (c-syntactic-re-search-forward "{" nil 'eob)
    (backward-char)
    (hs-toggle-hiding)))

(defun beginning-of-next-defun ()
  (interactive)
  (c-beginning-of-defun -1))

(add-hook 'c-initialization-hook
          (lambda ()
            (local-set-key (kbd "C-M-p") 'c-beginning-of-defun)
            (local-set-key (kbd "C-M-n") 'beginning-of-next-defun)
            (local-set-key (kbd "C-M-N") 'c-end-of-defun)
            (local-set-key (kbd "C-;") 'toggle-folding)
            (local-set-key (kbd "C-c f t") 'toggle-folding) ;; f(old) t(oggle)
            (local-set-key (kbd "C-c f s") 'hs-show-all) ;; f(old) s(how all)
            (local-set-key (kbd "C-c f h") 'hs-hide-all) ;; f(old) h(ide all)
            ))

(add-hook 'c-mode-common-hook
          (lambda ()
            (hs-minor-mode)))


;; ************* MAGIT STUFF ***********************
(global-set-key (kbd "C-x g") 'magit-status)
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")


;; ************* IBUFF STUFF ***********************
(load-file (concat user-emacs-init-directory "init_ibuffer.el"))


;; ************* HELM MODE STUFF *******************
(require 'helm-config)
;;(require 'swiper-helm)

(helm-mode 1)

;;(global-set-key (kbd "C-s") 'swiper-helm)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)


;; ************* VERILOG STUFF *********************
(load-file (concat user-emacs-init-directory "init_verilog.el"))

;; ************* PROJECTILE STUFF ******************
(load-file (concat user-emacs-init-directory "init_projectile.el"))

;; ************* SPACELINE STUFF *******************
(load-file (concat user-emacs-init-directory "init_spaceline.el"))

;; ************* LSP-MODE STUFF ********************
(load-file (concat user-emacs-init-directory "init_lsp-mode.el"))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


