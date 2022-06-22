(defun evz/git-message-add-jira-ticket()
  (let* ((branch-name (magit-get-current-branch))
         (pat "^\\(NES-[0-9]+\\)")
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
