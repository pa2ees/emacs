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
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
     "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb"
     default))
 '(inhibit-startup-screen t)
 '(logview-additional-level-mappings
   '(("imsar_levels" (error "error") (warning "warning")
      (information "info") (debug "debug") (trace "trace") (aliases))))
 '(logview-additional-submodes
   '(("imsar" (format . "[TIMESTAMP][LEVEL][NAME]")
      (levels . "imsar_levels") (timestamp "imsar_timestamp")
      (aliases))))
 '(logview-additional-timestamp-formats
   '(("imsar_timestamp" (java-pattern . "HH:mm:ss.SSS")
      (datetime-options))
     ("imsar_datetimestamp" (java-pattern . "yyyyMMdd_hh:MM:ss.SSS")
      (datetime-options))))
 '(package-selected-packages
   '(abyss-theme ag agent-shell auto-compile clang-format clang-format+
                 cmake-mode company company-jedi dired-narrow
                 dired-sidebar dired-toggle diredfl elpy
                 emacs-mcp-server forge general ggtags git-messenger
                 gptel graphviz-dot-mode helm helm-ag helm-gtags
                 helm-lsp helm-org helm-projectile
                 highlight-parentheses jedi log4j-mode logview
                 lsp-mode lsp-ui magit move-dup org-download
                 org-modern org-tree-slide persist plantuml-mode
                 project-persist projectile smart-mode-line
                 smart-mode-line-powerline-theme smartparens spaceline
                 swiper swiper-helm transient treemacs
                 treemacs-projectile treesit-auto vterm xterm-color yaml-mode
                 yasnippet yasnippet-snippets))
 '(package-vc-selected-packages
   '((emacs-mcp-server :url "https://github.com/rhblind/emacs-mcp-server"))))

(package-initialize)


;; TODO
;; flycheck (syntax checking)
;; company (complete-anything)
  ;; including company-jedi (already using jedi) ??
  ;; use irony with this?
  ;; works with helm!
;; elpy for python stuff?

;; https://trivialfis.github.io/emacs/2017/08/02/C-C++-Development-Environment-on-Emacs.html

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

;; ************* TREE-SITTER STUFF *****************
;; DISABLED: Interferes with lsp-mode integration
;; Configure grammar sources with ABI 14 compatible versions
;; (Emacs 30.2 with Ubuntu's libtree-sitter 0.20.3 uses ABI 14)
;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.21.0")
;;         (c "https://github.com/tree-sitter/tree-sitter-c" "v0.21.3")
;;         (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.22.0")
;;         (cmake "https://github.com/uyha/tree-sitter-cmake" "v0.5.0")
;;         (css "https://github.com/tree-sitter/tree-sitter-css" "v0.21.0")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp" "v1.3.0")
;;         (html "https://github.com/tree-sitter/tree-sitter-html" "v0.20.3")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2")
;;         (json "https://github.com/tree-sitter/tree-sitter-json" "v0.21.0")
;;         (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.2.3")
;;         (python "https://github.com/tree-sitter/tree-sitter-python" "v0.21.0")
;;         (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml" "v0.6.0")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.1" "typescript/src")
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.1" "tsx/src")
;;         (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.6.1")))

;; Prompt before installing grammars
;; (setq treesit-auto-install 'prompt)

;; treesit-auto: automatically switches to tree-sitter modes
;; (use-package treesit-auto
;;   :ensure t
;;   :config
;;   (global-treesit-auto-mode)

;;   ;; Hook copying is done later in init file after modes are loaded
;;   )

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

(use-package auto-compile
  :ensure t
  :config
  ;; Do NOT enable global modes
  (setq auto-compile-on-save-mode nil)
  (setq auto-compile-on-load-mode nil))

;; helper function for loading files
;; byte-compiles the file if the .elc doesn't exist, or the .el is newer
(setq evz/compiled-load-files '())
(defun evz/compile-maybe-and-load (file)
  "Byte-compile FILE.el if needed, then load FILE (preferring FILE.elc)."
  (let* ((el (concat file ".el"))
         (elc (concat file ".elc")))
    ;; Compile if missing or stale
    (when (or (not (file-exists-p elc))
              (file-newer-than-file-p el elc))
      (message "compiling %s..." el)
      (push el evz/compiled-load-files)
      (byte-compile-file el))
    ;; Load without extension → Emacs prefers .elc
    (load file)))


;; misc setup things
(setq frame-resize-pixelwise t)
(setq inhibit-startup-screen t)
(put 'narrow-to-region 'disabled nil)

;; TODO: unset this only when using windowed emacs
;;(global-unset-key (kbd "C-z"))

;; Control where autosaves go
(setq
 backup-by-copying t      ; don't clobber symlinks
 tramp-auto-save-directory "~/.emacs_saves/tramp/"
 tramp-allow-unsafe-temporary-files t
 backup-directory-alist
 '(("." . "~/.emacs_saves/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; prevent ffap (find file at point) from pinging weird websites if the thing at point
;; looks something like a web address. ie test.my (looks for some server in Malaysia)???
(setq ffap-machine-p-known 'reject)

;; make default font height bigger!
(set-face-attribute 'default nil :height 120)
;;(set-face-attribute 'default nil :height 150)

;; plantuml stuff
(setq plantuml-default-exec-mode 'jar)
(setq plantuml-jar-path "/opt/plantuml/plantuml.jar")
(setq plantuml-output-type "png")

;; programming files basic stuff
(setq c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Tree-sitter mode indentation settings
(setq-default c-ts-mode-indent-offset 4)
(setq-default c++-ts-mode-indent-offset 4)

;; (setq scroll-step 1)
;; (setq hscroll-step 1) ;; set horizontal scroll step to 1
(setq auto-hscroll-mode 'current-line)

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
(setq company-backends
      '((company-capf :with company-files company-keywords)))
(add-hook 'company-mode-hook #'yas-minor-mode)

;; ************* LINUM STUFF ***********************
(global-display-line-numbers-mode 1)

;; image mode and doc view mode don't like linum mode
(add-hook 'doc-view-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'image-mode-hook (lambda () (display-line-numbers-mode -1)))


;; ************* CONFLUENCE STUFF ******************
;; unfortunately this is not working
;; (add-to-list 'load-path (concat user-emacs-init-directory "confluence/confluence-el"))
;; (require 'confluence)
;; (setq confluence-url "https://confluence.imsar.us")


;; ************* DIRED MODE STUFF ******************
(evz/compile-maybe-and-load (concat user-emacs-init-directory "init_dired"))
;; (load-file (concat user-emacs-init-directory "init_dired.el"))


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

;; ************* MOVE-DUP STUFF ********************
(global-set-key (kbd "M-p") 'move-dup-move-lines-up)
(global-set-key (kbd "M-n") 'move-dup-move-lines-down)
(global-set-key (kbd "M-P") 'move-dup-duplicate-up)
(global-set-key (kbd "M-N") 'move-dup-duplicate-down)

;; ************* MY FUNCTIONS **********************
(evz/compile-maybe-and-load (concat user-emacs-init-directory "init_my_functions"))
;; (load-file (concat user-emacs-init-directory "init_my_functions.el"))

;; bind keyboard commands to my functions
(global-set-key (kbd "C-S-k") 'kill-line-no-save-to-kill-ring)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-<f5>") 'revert-all-buffers)
(global-set-key (kbd "C-x K") 'kill-buffer-other-window-and-close)
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-F") 'forward-symbol)
(global-set-key (kbd "s-n") 'set-frame-name)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "C-c z") #'evz/zoom-toggle)

;; ************** CLANG-FORMAT *********************
(add-hook 'c-mode-common-hook #'evz/clang-format-enable-this-buffer)

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
;; (evz/compile-maybe-and-load (concat user-emacs-init-directory "init_c-mode"))
;; (evz/compile-maybe-and-load (concat user-emacs-init-directory "init_python_bindings_helpers"))
(load-file (concat user-emacs-init-directory "init_c-mode.el"))
(load-file (concat user-emacs-init-directory "init_python_bindings_helpers.el"))

;; ************* GIT-MESSENGER STUFF ***************
(load-file (concat user-emacs-init-directory "init_git_messenger.el"))

;; ************* PROJECTILE STUFF ******************
;; This needs to come before magit stuff because evz/helm-select-project-magit
;; depends on it
;; (evz/compile-maybe-and-load (concat user-emacs-init-directory "init_projectile"))
(load-file (concat user-emacs-init-directory "init_projectile.el"))

;; ************* FORGE STUFF ***********************
;; see https://docs.magit.vc/devel/forge/forge.pdf for setup
(use-package forge
  ;; Point this to the "lisp" subdirectory of your clone
  :load-path "~/projects/forge/lisp"
  :after magit
  :config
  ;; Any custom forge configuration goes here
  )
;; (use-package forge
;;   :after magit)

(with-eval-after-load 'forge
  (push '("gitlabee.imsar.us"        ; GITHOST
          "gitlabee.imsar.us/api/v4" ; APIHOST
          "gitlabee.imsar.us"        ; WEBHOST and INSTANCE-ID
          forge-gitlab-repository)   ; CLASS
        forge-alist)
  (setq auth-sources '("~/.authinfo")))


;; ************* MAGIT STUFF ***********************
;; (evz/compile-maybe-and-load (concat user-emacs-init-directory "init_magit"))
(load-file (concat user-emacs-init-directory "init_magit.el"))
(global-set-key (kbd "C-x g") 'evz/helm-select-project-magit)
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

;; ************* IBUFF STUFF ***********************
(evz/compile-maybe-and-load (concat user-emacs-init-directory "init_ibuffer"))
;; (load-file (concat user-emacs-init-directory "init_ibuffer.el"))

;; ************* HELM MODE STUFF *******************
(require 'swiper-helm)

(helm-mode 1)

(global-set-key (kbd "C-s") 'swiper-helm)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; ************* ORG MODE STUFF ********************
;; required to be after helm
;; (load-file (concat user-emacs-init-directory "init_org.el"))
;; (load (concat user-emacs-init-directory "init_org"))
(evz/compile-maybe-and-load (concat user-emacs-init-directory "init_org"))

;; ************* VERILOG STUFF *********************
(load-file (concat user-emacs-init-directory "init_verilog.el"))

;; ************* POWERLINE STUFF *******************
(evz/compile-maybe-and-load (concat user-emacs-init-directory "init_powerline"))
;; (load-file (concat user-emacs-init-directory "init_powerline.el"))

;; ************* LSP-MODE STUFF ********************
(load-file (concat user-emacs-init-directory "init_lsp-mode.el"))

;; ************* AGENT-SHELL STUFF *****************
(load-file (concat user-emacs-init-directory "init_agent_shell.el"))

;; ************* TREE-SITTER HOOK SETUP ************
;; Copy hooks from traditional modes to tree-sitter modes
;; Done at end of init to ensure all modes are loaded first
;; (when (boundp 'c-mode-common-hook)
;;   (setq c-ts-mode-hook c-mode-common-hook)
;;   (setq c++-ts-mode-hook c-mode-common-hook))
;; (when (boundp 'python-mode-hook)
;;   (setq python-ts-mode-hook python-mode-hook))
;; (when (boundp 'sh-mode-hook)
;;   (setq bash-ts-mode-hook sh-mode-hook))
;; (when (boundp 'org-mode-hook)
;;   (setq org-ts-mode-hook org-mode-hook))
;; (when (boundp 'verilog-mode-hook)
;;   (setq verilog-ts-mode-hook verilog-mode-hook))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


