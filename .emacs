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

;; Truncate lines always
(set-default 'truncate-lines t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; (load-theme 'abyss t t)
;; (enable-theme 'abyss)
(load-theme 'manoj-dark t)
(enable-theme 'manoj-dark)
;; (set-background-color "black")
;; (let ((frame-background-mode 'light)) (frame-set-background-mode nil))


;; linum stuff
(global-linum-mode)
(setq linum-format "%d ")

(add-hook 'doc-view-mode-hook (lambda () (linum-mode -1)))

(add-hook 'dired-mode-hook (lambda () (linum-mode -1)))
(add-hook 'image-mode-hook (lambda () (linum-mode -1)))

;; dired mode stuff
;; This advises functions to be aware of subdirs in dired mode
(defun dired-subdir-aware (orig-fun &rest args)
  (if (eq major-mode 'dired-mode)
      (let ((default-directory (dired-current-directory)))
        (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'read-file-name :around 'dired-subdir-aware)
(advice-add 'find-file-read-args :around 'dired-subdir-aware)
(advice-add 'dired-do-compress-to :around 'dired-subdir-aware)


(defun kill-buffer-other-window-and-close()
  "If there are multiple windows, then close the other window and kill the buffer in it also."
  (interactive)
  (let ((local-buff (buffer-name)))
    (other-window 1)
    (let ((other-window-buff (buffer-name)))
      (other-window -1)
      (if (string= local-buff other-window-buff)
          (progn
            (other-window 1)
            (if (not (one-window-p))
                (delete-window)))
        (progn
          (other-window 1)
          (kill-this-buffer)
          (if (not (one-window-p))
              (delete-window)))))))
        

(global-set-key (kbd "C-x K") 'kill-buffer-other-window-and-close)

;; PDF Tools
(pdf-tools-install)

(add-hook 'pdf-view-mode-hook (lambda () (progn
                                           (linum-mode -1)
                                           (local-set-key (kbd "C-p") (lambda () (interactive) (pdf-view-previous-line-or-previous-page 6)))
                                           (local-set-key (kbd "C-n") (lambda () (interactive) (pdf-view-next-line-or-next-page 6))))))

(setq-default pdf-view-display-size 'fit-page)
;; (setq-default pdf-view-continuous nil)


;; (pdf-view-next-line-or-next-page 6)


(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; (require 'smartparens-config)
;; (smartparens-global-mode 1)
;; (show-smartparens-global-mode -1)

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

;; jedi - python completion
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


;; ibuffer - an enhanced buffer switching tool
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("perl" (mode . cperl-mode))
               ("python" (mode . python-mode))
               ("verilog" (mode . verilog-mode))
               ("erc" (mode . erc-mode))
               ("planner" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (mode . muse-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("magit" (or
                         (name . "^magit*")))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))))))

(setq ibuffer-never-show-predicates (list "^\\*tramp*" "^\\*epc*"))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)


;; Verilog mode disable auto formatting                                   

(eval-after-load 'verilog-mode
    '(progn
        ;; same for all the electric-verilog-* commands in                
        ;; the mode's map (see verilog-mode.el)                      
        (define-key verilog-mode-map (kbd ";") 'self-insert-command)
        (define-key verilog-mode-map (kbd ":") 'self-insert-command)
        (define-key verilog-mode-map (kbd "RET") 'evil-ret)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (pdf-tools jedi smartparens magit highlight-parentheses abyss-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

