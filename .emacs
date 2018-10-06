;; Place the following in the default .emacs file
;; which is usually found at c:\users\youruser\AppData\Roaming

;; (setq user-init-file "C:/_projects/emacs/.emacs")
;; (setq user-emacs-directory "C:/_projects/emacs/.emacs.d/")
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

(setq inhibit-startup-screen t)

(put 'narrow-to-region 'disabled nil)

(setq c-default-style "linux")
(setq-default c-basic-offset 4)

;; (setq scroll-step 1)
(setq scroll-margin 4) ;; sets how far away from top or bottom we start to scroll
(setq scroll-conservatively 1) ;; seems to work ok
(menu-bar-mode -1)
(tool-bar-mode -1)


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(load-theme 'abyss t)
(global-linum-mode)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode -1)

(global-highlight-parentheses-mode)

(setq python-shell-interpreter "ipython")

(add-hook 'python-mode-hook
	  (lambda () (progn
		       (jedi:setup)
		       (setq jedi:complete-on-dot t)
		       (setq python-indent-offset 4))))

;; reverting buffers
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t t)
    (message "Buffer reverted!!"))

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-<f5>") 'revert-all-buffers)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

