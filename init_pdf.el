;; (pdf-tools-install)

;; (add-hook 'pdf-view-mode-hook (lambda () (progn
;;                                            (linum-mode -1)
;;                                            (local-set-key (kbd "C-p") (lambda () (interactive) (pdf-view-previous-line-or-previous-page 6)))
;;                                            (local-set-key (kbd "C-n") (lambda () (interactive) (pdf-view-next-line-or-next-page 6))))))

;; (setq-default pdf-view-display-size 'fit-page)
;; (setq-default pdf-view-continuous nil)


;; (pdf-view-next-line-or-next-page 6)
