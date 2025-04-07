(defun evz/insert-pybind-arg (&optional arg defval)
  "Prompt the user for an argument name and optional default value, then insert the appropriate pybind11 argument."
  (interactive)
  (let* ((arg-name (or arg (read-string "Argument name: ")))
         (default-value (if arg
                            (or defval "")
                          (read-string "Default value (leave empty if none): "))))
    (if (string-empty-p default-value)
        (insert (format ", pybind::arg(\"%s\")" arg-name))
      (insert (format ", pybind::arg(\"%s\") = %s" arg-name default-value)))))

(defun evz/insert-pybind-args-from-param-list ()
  (interactive)
  (if evz/param-list
      (progn
        (dolist (param evz/param-list)
          (evz/insert-pybind-arg (car param) (cdr param)))
        (setq evz/param-list nil))
    (message "param-list is empty")))

(defun evz/extract-cpp-parameters ()
  "Extract parameter names and default values from a C++ function signature under point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'line))
           (line (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (params-str (progn (string-match "(\\(.*\\))" line)
                               (match-string 1 line)))
           (params (split-string params-str "," t "[ \t]+"))
           (param-list '()))
      (dolist (param params)
        (setq param (replace-regexp-in-string "[*&]+" "" param))
        (if (string-match "\\([^ ]*\\) *= *\\(.*\\)" param)
            (push (cons (match-string 1 param) (match-string 2 param)) param-list)
          (if (string-match "\\([a-zA-Z0-9_]*\\) *$" param)
              (push (cons (match-string 1 param) nil) param-list))))
      (setq evz/param-list (reverse param-list))
      (message "%S" (reverse param-list)))))

;; (defun evz/extract-cpp-parameters ()
;;   "Extract parameter names and default values from a C++ function signature under point."
;;   (interactive)
;;   (save-excursion
;;     (let* ((bounds (bounds-of-thing-at-point 'line))
;;            (line (buffer-substring-no-properties (car bounds) (cdr bounds)))
;;            (params-str (progn (string-match "(\\(.*\\))" line)
;;                                (match-string 1 line)))
;;            (params (split-string params-str "," t "[ \t]+"))
;;            (param-list '()))
;;       (dolist (param params)
;;         (setq param (replace-regexp-in-string "[*&]+" "" param))
;;         (if (string-match "[a-zA-Z0-9_<>\\-:]+\\([a-zA-Z0-9_]+\\)*=*" param)
;;             (push (cons (match-string 1 param) (match-string 2 param)) param-list)
;;           (if (string-match "[a-zA-Z0-9_<>\\-:]+\\([a-zA-Z0-9_]+\\)" param)
;;               (push (cons (match-string 1 param) nil) param-list))))
;;       (setq evz/param-list (reverse param-list))
;;       (message "%S" (reverse param-list)))))

(global-set-key (kbd "C-c b i") 'evz/insert-pybind-args-from-param-list)
(global-set-key (kbd "C-c b I") 'evz/insert-pybind-arg)
(global-set-key (kbd "C-c b x") 'evz/extract-cpp-parameters)


;; "[^ ]* \\([^ ]*\\) ?= ?\\(.*\\)"
