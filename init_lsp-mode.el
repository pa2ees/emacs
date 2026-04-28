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
(setq lsp-ui-doc-position 'at-point) ;; make the tooltip pop up at point
(setq lsp-ui-doc-show-with-mouse nil) ;; make tooltip not pop up with mouse hover
(setq lsp-ui-doc-show-with-cursor nil) ;; make tooltip not pop up automatically
(setq lsp-ui-doc-always-hide t) ;; does nothing???
(add-hook 'pre-command-hook #'lsp-ui-doc-hide) ;; force the tooltip to go away 


