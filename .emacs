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
(package-initialize)


;; TODO
;; flycheck (syntax checking)
;; company (complete-anything)
  ;; including company-jedi (already using jedi) ??
  ;; use irony with this?
  ;; works with helm!
;; ggtags http://www.mycpu.org/emacs-rtags-helm/ uses gnu-global
;; rtags completion tags
  ;;can use helm
  ;; needs libclang?
;; irony completion tags
  ;; needs libclang?
;; elpy for python stuff?

;; https://trivialfis.github.io/emacs/2017/08/02/C-C++-Development-Environment-on-Emacs.html


(use-package general
  :ensure t)


;; misc setup things
(setq inhibit-startup-screen t)
(put 'narrow-to-region 'disabled nil)

;; make default font height bigger!
(set-face-attribute 'default nil :height 150)

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

;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

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


;; ************* ORG MODE STUFF ********************
(global-set-key (kbd "C-c l") 'org-store-link)


;; ************* DIRED MODE STUFF ******************
(load-file (concat user-emacs-init-directory "init_dired.el"))


;; ************* PYTHON STUFF **********************
(load-file (concat user-emacs-init-directory "init_python.el"))


;; ************* PDF STUFF *************************
(load-file (concat user-emacs-init-directory "init_pdf.el"))


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
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-<f5>") 'revert-all-buffers)
(global-set-key (kbd "C-x K") 'kill-buffer-other-window-and-close)


;; ************* YASNIPPET STUFF *******************
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (setq yas-snippet-dirs (cons (concat user-emacs-init-directory "snippets/") yas-snippet-dirs))
  (yas-reload-all))

(yas-global-mode 1)


;; ************* MAGIT STUFF ***********************
(global-set-key (kbd "C-x g") 'magit-status)
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")


;; ************* IBUFF STUFF ***********************
(load-file (concat user-emacs-init-directory "init_ibuffer.el"))


;; ************* HELM MODE STUFF *******************
(require 'helm-config)
(require 'swiper-helm)

(helm-mode 1)

(global-set-key (kbd "C-s") 'swiper-helm)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)


;; ************* VERILOG STUFF *********************
(load-file (concat user-emacs-init-directory "init_verilog.el"))


;; ************* PROJECTILE STUFF ******************
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)


(require 'helm-projectile)
(helm-projectile-on)


;; ************* SPACELINE STUFF *******************
(load-file (concat user-emacs-init-directory "init_spaceline.el"))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (elpy general company company-jedi yasnippet yasnippet-snippets spaceline smart-mode-line-powerline-theme smart-mode-line helm-projectile projectile treemacs treemacs-projectile swiper swiper-helm helm dired-sidebar dired-toggle diredfl pdf-tools jedi smartparens magit highlight-parentheses abyss-theme))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


