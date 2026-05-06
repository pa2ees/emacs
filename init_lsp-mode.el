(setq lsp-keymap-prefix "M-l")
(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (c-mode . lsp))
;;         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-ui
  :ensure t)

(define-key lsp-mode-map (kbd "M-l d") #'lsp-ui-doc-show) ;; manually pop up tooltip
(define-key lsp-mode-map (kbd "M-l f") #'lsp-execute-code-action) 
(setq lsp-ui-doc-position 'at-point) ;; make the tooltip pop up at point
(setq lsp-ui-doc-show-with-mouse nil) ;; make tooltip not pop up with mouse hover
(setq lsp-ui-doc-show-with-cursor nil) ;; make tooltip not pop up automatically
(setq lsp-ui-doc-always-hide t) ;; does nothing???
(setq lsp-clients-clangd-args
      '("--header-insertion-decorators=0"
        "--background-index"))

(add-hook 'pre-command-hook #'lsp-ui-doc-hide) ;; force the tooltip to go away 

;; NOTES:

;; *** .clang-tidy notes ***
;; *************************
;; To use clang-tidy, put a .clang-tidy file in the project root
;; (or a containing folder).  Example:

;; *** .clang-tidy example ***
;; ***************************
;; # Disable all checks first (-*), then explicitly enable specific groups
;; Checks: >
;;   -*,
;;   bugprone-*,
;;   -bugprone-easily-swappable-parameters,
;;   cppcoreguidelines-*,
;;   -cppcoreguidelines-avoid-non-const-global-variables,
;;   modernize-*,
;;   -modernize-use-trailing-return-type,
;;   -modernize-use-auto,
;;   performance-*,
;;   readability-*,
;;   -readability-magic-numbers

;; # Treat warnings as errors (optional, leave empty to just warn)
;; WarningsAsErrors: ''

;; # Only analyze headers in your project (prevents noise from system headers)
;; HeaderFilterRegex: '.*'

;; # Extra configurations for specific checks
;; CheckOptions:
;;   - key:             readability-identifier-naming.VariableCase
;;     value:           camelBack


;; *** clangd configuration notes ***
;; **********************************
;; To specify a different compiler/toolchain for clangd (other than
;; what is in the .compile-commands.json file, add a configuration
;; file in /home/<username>/.config/clangd/config.yaml. Example:

;; *** clangd config.yaml example ***
;; **********************************
;; If:
;;   # This regex applies these rules to ANY file inside a folder named "code"
;;   PathMatch: /imsar/.*

;; CompileFlags:
;;   Compiler: /opt/gcc-13/bin/g++
;;   Add: ["--gcc-toolchain=/opt/gcc/gcc-13", -Wall, -Wextra, -std=c++17]


