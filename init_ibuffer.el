;; ibuffer - an enhanced buffer switching tool
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("C/C++" (or
                         (mode . c++-mode)
                         (mode . c-mode)))
               ("perl" (mode . cperl-mode))
               ("python" (or
                          (mode . python-mode)
                          (mode . inferior-python-mode)))
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
               ("helm" (or
                        (name . "\*helm-\*")))
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
              (push "helm" ibuffer-hidden-filter-groups)
              (push "magit" ibuffer-hidden-filter-groups)
              (setq ibuffer-current-format 1)
              (ibuffer-update-format))))

(setq ibuffer-show-empty-filter-groups nil)
