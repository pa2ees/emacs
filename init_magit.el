(defun evz/git-message-add-jira-ticket()
  (let* ((branch-name (magit-get-current-branch))
         (rem-url (magit-get (concat "remote." (magit-get-current-remote) ".url")))
         (nes-pat "^\\(\\(NES\\|NANO\\)-[0-9]+\\)")
         (rem-pat "\\(.*@gitlabee.imsar.us\\)")
         (rem-pos (string-match rem-pat rem-url))
         (nes-pos (string-match nes-pat branch-name)))
    (if rem-pos
        (save-excursion
          (save-match-data
            (if (search-forward "#" nil t)
                (beginning-of-line)
              (insert "\n\n")))
          (if nes-pos
              (progn
                (open-line 1)
                (insert "JIRA Ticket: ")
                (insert (match-string 1 branch-name))
                (insert "\n")))
          (insert-file-contents "~/.magit_message")))))
      
(add-hook 'git-commit-setup-hook #'evz/git-message-add-jira-ticket)

(defun evz/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun evz/add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'evz/ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook #'evz/add-d-to-ediff-mode-map)
