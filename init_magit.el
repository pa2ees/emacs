(defun evz/git-message-add-jira-ticket()
  (let* ((branch-name (magit-get-current-branch))
         (pat "^\\(\\(NES\\|NANO\\)-[0-9]+\\)")
         (pos (string-match pat branch-name)))
    (if pos
        (save-excursion
          (save-match-data
            (if (search-forward "billed to" nil t)
                (beginning-of-line)
              (if (search-forward "#" nil t)
                  (beginning-of-line)
                (insert "\n\n"))))
            
          (open-line 1)
          (insert "JIRA Ticket: ")
          (insert (match-string 1 branch-name))))))
      
(add-hook 'git-commit-setup-hook #'evz/git-message-add-jira-ticket)

(defun evz/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun evz/add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'evz/ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook #'evz/add-d-to-ediff-mode-map)
