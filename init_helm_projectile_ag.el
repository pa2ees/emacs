(defvar evz/helm-ag--extra-options '()
  "List of extra options to add to helm-ag-base-command")

(defvar evz/helm-ag--before-extra-options-base-command ""
  "State of base command before extra options added")

(defun evz/helm-ag--toggle-extra-option (option)
  "Add option if it's not present in evz/helm-ag--extra-options, otherwise remove it"
  (if (member option evz/helm-ag--extra-options)
      (setq evz/helm-ag--extra-options (remove option evz/helm-ag--extra-options))
    (push option evz/helm-ag--extra-options)))

(defun evz/helm-ag--add-extra-option (option)
  (unless (member option evz/helm-ag--extra-options)
    (push option evz/helm-ag--extra-options)))
   
(defun evz/helm-ag--remove-extra-option (option)
  (if (member option evz/helm-ag--extra-options)
      (setq evz/helm-ag--extra-options (remove option evz/helm-ag--extra-options))))
   
(defun evz/helm-ag--get-extra-options-as-str ()
  (mapconcat #'identity evz/helm-ag--extra-options " "))

(defun evz/helm-ag--rerun-for-extra-options ()
  (setq evz/helm-ag--current-refine-pattern helm-pattern)
  (helm-run-after-exit
   (lambda()
     (let* ((helm-ag-base-command (concat evz/helm-ag--before-extra-options-base-command (evz/helm-ag--get-extra-options-as-str))))
       (helm :sources '(helm-ag-source) :buffer "*helm-ag*" :keymap helm-ag-map
             :history 'helm-ag--helm-history :input evz/helm-ag--current-refine-pattern)))))

(defun evz/helm-ag-toggle-hidden ()
  (interactive)
  (evz/helm-ag--toggle-extra-option "--hidden")
  (evz/helm-ag--rerun-for-extra-options))

(defun evz/helm-ag-toggle-case-sensitivity ()
  (interactive)
  (evz/helm-ag--toggle-extra-option "-s")
  (evz/helm-ag--rerun-for-extra-options))


;; I think this can just be done by putting !path/to/dir as a pattern
;; (defun evz/helm-ag-exclude-dir ()
;;   (interactive)
;;   (let* ((candidates (helm-get-candidates (helm-get-current-source)))
;;          (dirs (delete-dups
;;                 (mapcar (lambda (candidate)
;;                           (let ((path (if (consp candidate) (cdr candidate) candidate)))
;;                             (file-name-directory path)))
;;                         candidates))))
;;     (setq blarf dirs)
;;     (setq blarf2 candidates)))
;; (cadr blarf)
;; (when (string-match "\\`\\([^:]+\\):" (cdar blarf2))
;;   (insert (match-string 1 (cdar blarf2))))

;; (car (split-string (cdar blarf2) ":"))
;; (insert (cdar blarf2))

(defun evz/helm-ag--setup (base-command)
  (setq evz/helm-ag--before-extra-options-base-command base-command)
  (setq evz/helm-ag--extra-options '()))

;; helm *r*efine
;;  - *h*idden (toggle)
;;  - exclude *d*ir
;;  - refine excluded *D*irs
(with-eval-after-load 'helm-ag
  (define-key helm-ag-map (kbd "C-c r h") #'evz/helm-ag-toggle-hidden)
  (define-key helm-ag-map (kbd "C-c r c") #'evz/helm-ag-toggle-case-sensitivity)
  ;; (define-key helm-ag-map (kbd "C-c r d") #'evz/helm-ag-exclude-dir)
  ;; (define-key helm-ag-map (kbd "C-c r D") #'evz/helm-ag--refine-exclude-dirs)
  )

(defun evz/helm-projectile-ag (&optional options)
  "Helm version of `projectile-ag'.
OPTIONS explicit command line arguments to ag"
  (interactive (if current-prefix-arg (list (helm-read-string "option: " "" 'helm-ag--extra-options-history))))
  (if (require 'helm-ag nil t)
      (if (projectile-project-p)
          (progn
            (when (helm-projectile--projectile-ignore-strategy)
              (setq grep-find-ignored-files (helm-projectile--ignored-files)
                    grep-find-ignored-directories (helm-projectile--ignored-directories)))
            (let* ((ignored (when (helm-projectile--projectile-ignore-strategy)
                              (mapconcat (lambda (i)
                                           (concat "--ignore " i))
                                         (append grep-find-ignored-files
                                                 grep-find-ignored-directories
                                                 (cadr (projectile-parse-dirconfig-file)))
                                         " ")))
                   (helm-ag-base-command (concat helm-ag-base-command
                                                 (when ignored (concat " " ignored))
                                                 " " options))
                   (current-prefix-arg nil))
              (evz/helm-ag--setup helm-ag-base-command)
              (helm-ag (projectile-project-root) (car (projectile-parse-dirconfig-file)))))
        (error "You're not in a project"))
    (when (yes-or-no-p "`helm-ag' is not installed. Install? ")
      (condition-case nil
          (progn
            (package-install 'helm-ag)
            (helm-projectile-ag options))
        (error (error "`helm-ag' is not available.  Is MELPA in your `package-archives'?"))))))

(defun evz/helm-select-project--helm-projectile-ag (&rest args)
  (interactive "P")
  (let ((default-directory (or (evz/helm-select-project "Select Project for Ag") default-directory)))
    (apply #'evz/helm-projectile-ag args)))

(defun evz/helm-select-project--helm-projectile-find-file (&rest args)
  (interactive "P")
  (let ((default-directory (or (evz/helm-select-project "Select Project for Ag") default-directory)))
    (apply #'helm-projectile-find-file args)))
