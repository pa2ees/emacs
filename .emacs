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

(add-hook 'image-mode-hook (lambda () (linum-mode -1)))


;; ************* DIRED MODE STUFF ********************

;; This advises functions to be aware of subdirs in dired mode
(defun dired-subdir-aware (orig-fun &rest args)
  (if (eq major-mode 'dired-mode)
      (let ((default-directory (dired-current-directory)))
        (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'read-file-name :around 'dired-subdir-aware)
(advice-add 'find-file-read-args :around 'dired-subdir-aware)
(advice-add 'dired-do-compress-to :around 'dired-subdir-aware)

(defun dired-sort-set-mode-line-extended ()
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let ((sorting-reversed (string-match-p (dired-get-match-string-for-switch "r") dired-actual-switches))
                case-fold-search)
            (let ((rev-str (if sorting-reversed
                               " Rev "
                             " ")))

              (cond ((string-match-p
                      (dired-get-match-string-for-switch "X") dired-actual-switches)
                     (concat "Dired by Ext" rev-str))
                    ((string-match-p
                      (dired-get-match-string-for-switch "U") dired-actual-switches)
                     (concat "Dired by dir order" rev-str))
                    ((string-match-p
                      (dired-get-match-string-for-switch "S") dired-actual-switches)
                     (concat "Dired by Size" rev-str))
                    ((string-match-p
                      (dired-get-match-string-for-switch "t") dired-actual-switches)
                     (concat "Dired by Date" rev-str))
                    (t
                     (concat "Dired by Name" rev-str))))))
    (force-mode-line-update)))

(advice-add 'dired-sort-set-mode-line :override 'dired-sort-set-mode-line-extended)


(defun dired-remove-sort-all ()
  (progn
    (dired-remove-sort "t")
    (dired-remove-sort "S")
    (dired-remove-sort "X")
    (dired-remove-sort "U")))

(defun dired-get-match-string-for-switch (arg)
  (if (> (string-to-char arg) (string-to-char "Z"))
      (concat "\\(\\`\\| \\)-\\([a-"
              (char-to-string (- (string-to-char arg) 1))
              (char-to-string (+ (string-to-char arg) 1))
              "-zA-Z]*\\)\\("
              arg
              "\\)\\([^ ]*\\)")
    (concat "\\(\\`\\| \\)-\\([a-"
            (char-to-string (- (string-to-char arg) 1))
            (char-to-string (+ (string-to-char arg) 1))
            "-zA-Z]*\\)\\("
            arg
            "\\)\\([^ ]*\\)")))

(defun dired-remove-switch (arg)
  (let ((switch-regexp (dired-get-match-string-for-switch arg))
                       case-fold-search)
    ;; Remove the switch.
    (while (string-match switch-regexp dired-actual-switches)
      (if (and (equal (match-string 2 dired-actual-switches) "")
               (equal (match-string 4 dired-actual-switches) ""))
          ;; Remove a stand-alone switch.
          (progn
            (setq dired-actual-switches
                  (replace-match "" t t dired-actual-switches)))
        ;; Remove a switch of the form -XaY for some X and Y where a is the switch to be removed.
        (setq dired-actual-switches
              (replace-match "" t t dired-actual-switches 3))))))

(defun dired-add-switch (arg)
  (let ((switch-regexp "\\(.*?\\)\\(\\`\\| +\\)\\(-[[:alnum:]]+\\)\\(.*\\)"))
    (setq dired-actual-switches
          (if (string-match switch-regexp dired-actual-switches)
              (concat (match-string 1 dired-actual-switches)
                      (match-string 2 dired-actual-switches)
                      (match-string 3 dired-actual-switches)
                      arg
                      (match-string 4 dired-actual-switches))
            (concat dired-actual-switches
                    " -"
                    arg))))
  (dired-sort-set-mode-line))

(defun dired-switch-toggle (arg)
  (let ((switch-exists (string-match-p (dired-get-match-string-for-switch arg) dired-actual-switches))
        case-fold-search)
    (if switch-exists
        (progn
          (dired-remove-switch arg))
      (dired-add-switch arg))))


(defun dired-do-sort (arg)
  (dired-add-switch arg))
  
(defun dired-remove-sort (arg)
  (dired-remove-switch arg))

(defun dired-sort-file-extension ()
  (interactive)
  (dired-remove-sort-all)
  (dired-do-sort "X")
  (revert-buffer)
  (message "Sorting by File Extension"))

(defun dired-sort-filename ()
  (interactive)
  (dired-remove-sort-all)
  (dired-sort-set-mode-line)
  (revert-buffer)
  (message "Sorting by Filename"))

(defun dired-sort-directory-order ()
  (interactive)
  (dired-remove-sort-all)
  (dired-do-sort "U")
  (revert-buffer)
  (message "Sorting by Directory Order"))

(defun dired-sort-size ()
  (interactive)
  (dired-remove-sort-all)
  (dired-do-sort "S")
  (revert-buffer)
  (message "Sorting by File Size"))

(defun dired-sort-time ()
  (interactive)
  (dired-remove-sort-all)
  (dired-do-sort "t")
  (revert-buffer)
  (message "Sorting by File Modification Date/Time"))

(defun dired-sort-reversed-toggle ()
  (dired-switch-toggle "r")
  (dired-sort-set-mode-line)
  (revert-buffer)
  (message "Toggled Reverse Sorting"))

(defun dired-reset-switches ()
  (setq dired-actual-switches "-Blh --group-directories-first")
  (dired-sort-set-mode-line)
  (revert-buffer)
  (message "Reset dired switches"))

(defun dired-show-all-toggle ()
  (dired-remove-switch "B")
  (dired-remove-switch "A")
  (dired-switch-toggle "a")
  (revert-buffer))

(defun dired-human-readable-toggle ()
  (dired-switch-toggle "h")
  (revert-buffer))

(defun dired-owner-toggle ()
  (dired-switch-toggle "g")
  (revert-buffer))

(defun dired-group-toggle ()
  (dired-switch-toggle "G")
  (revert-buffer))

(defun dired-backup-toggle ()
  (dired-switch-toggle "B")
  (revert-buffer))

  
(add-hook 'dired-mode-hook (lambda ()
                             (dired-reset-switches)
                             (local-set-key (kbd "M-s s x") (lambda() (interactive) (dired-sort-file-extension)))
                             (local-set-key (kbd "M-s s D") (lambda() (interactive) (dired-sort-directory-order)))
                             (local-set-key (kbd "M-s s f") (lambda() (interactive) (dired-sort-filename)))
                             (local-set-key (kbd "M-s s n") (lambda() (interactive) (dired-sort-filename)))
                             (local-set-key (kbd "M-s s s") (lambda() (interactive) (dired-sort-size)))
                             (local-set-key (kbd "M-s s t") (lambda() (interactive) (dired-sort-time)))
                             (local-set-key (kbd "M-s s d") (lambda() (interactive) (dired-sort-time)))
                             (local-set-key (kbd "M-s s r") (lambda() (interactive) (dired-sort-reversed-toggle)))
                             (local-set-key (kbd "M-s r") (lambda() (interactive) (dired-reset-switches)))
                             (local-set-key (kbd "M-s a") (lambda() (interactive) (dired-show-all-toggle)))
                             (local-set-key (kbd "M-s H") (lambda() (interactive) (dired-human-readable-toggle)))
                             (local-set-key (kbd "M-s g") (lambda() (interactive) (dired-owner-toggle)))
                             (local-set-key (kbd "M-s G") (lambda() (interactive) (dired-group-toggle)))
                             (local-set-key (kbd "M-s b") (lambda() (interactive) (dired-backup-toggle)))
                             
                             (linum-mode -1)))


;; (add-hook 'diredfl-mode-hook (lambda ()
;;                                (


;; **************** END DIRED STUFF ******************


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
               ("lisp" (or
                            (mode . emacs-lisp-mode)))
               ("planner" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (mode . muse-mode)))
               ("magit" (or
                         (name . "^magit*")))
               ("archives" (or
                              (mode . archive-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Help\\*$")
                         (name . "^\\*Messages\\*$")))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))
               ("hidden" (or
                          (name . "^\\*Dired log\\*$")
                          (name . "^\\*Completions\\*$")
                          (name . "^\\*Shell Command Output\\*$")
                          (name . "^\\*Backtrace\\*$")))))))



(setq ibuffer-never-show-predicates (list "^\\*tramp*" "^\\*epc*"))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (progn
              (ibuffer-switch-to-saved-filter-groups "default")
              (push "hidden" ibuffer-hidden-filter-groups)
              (message "fart"))))

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
    (dired-sidebar dired-toggle diredfl pdf-tools jedi smartparens magit highlight-parentheses abyss-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

