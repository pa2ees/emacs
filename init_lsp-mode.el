(setq lsp-keymap-prefix "M-l")
(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (c-mode . lsp))
;;         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

