(defvar evz/original-tab-binding nil
  "Store the original function 'TAB' was mapped to in C modes.")

(defvar evz/original-colon-binding nil
  "Store the original function ':' was mapped to in C modes.")

(defvar evz/original-dot-binding nil
  "Store the original function '.' was mapped to in C modes.")

(defun evz/c-mode-tab-press ()
  "Do default TAB behavior.  If pressed twice in a row, activate completion"
  (interactive)
  (if (eq last-command 'evz/c-mode-tab-press)
      (company-complete)
    (call-interactively evz/original-tab-binding)))    

(defun evz/c-mode-colon-press ()
  "Insert colon.  If pressed twice in a row, insert colon and activate completion"
  (interactive)
  (if (eq last-command evz/original-colon-binding)
      ;; if last-command was whatever a colon was originally bound to
      (progn
        ;; leave this-command set to evz/c-mode-colon-press
        ;; this will make completions pop up (after delay) if they should
        (call-interactively evz/original-colon-binding))
    (progn
      (call-interactively evz/original-colon-binding) ;; call original colon function
      ;; set this-command to whatever colon was originally bound to
      (setq this-command evz/original-colon-binding))))

(defun evz/c-mode-dot-press ()
  "Insert dot."
  (interactive)
  (call-interactively evz/original-dot-binding))

(defun evz/store-original-tab-behavior ()
  (setq evz/original-tab-binding (key-binding (kbd "TAB"))))

(defun evz/store-original-colon-behavior()
  (setq evz/original-colon-binding (key-binding (kbd ":"))))

(defun evz/store-original-dot-behavior()
  (setq evz/original-dot-binding (key-binding (kbd "."))))

(defun evz/setup-c-mode-completion-behavior ()
  ;; setup "TAB" behavior
  (evz/store-original-tab-behavior)
  (local-set-key (kbd "TAB") 'evz/c-mode-tab-press) 
  ;; setup ":" behavior
  (evz/store-original-colon-behavior)
  (local-set-key (kbd ":") 'evz/c-mode-colon-press)
  ;; setup "." behavior
  (evz/store-original-dot-behavior)
  (local-set-key (kbd ".") 'evz/c-mode-dot-press)
  ;; delay completion popup
  (setq company-idle-delay .5)
  (setq company-begin-commands '(c-scope-operator c-electric-lt-gt c-electric-slash evz/c-mode-colon-press evz/c-mode-dot-press)))

(defun evz/toggle-folding ()
  (interactive)
  (progn
    (beginning-of-line)
    (c-end-of-defun)
    (c-beginning-of-defun)
    (c-syntactic-re-search-forward "{" nil 'eob)
    (backward-char)
    (hs-toggle-hiding)))

(defun evz/beginning-of-next-defun ()
  (interactive)
  (c-beginning-of-defun -1))

(add-hook 'c-initialization-hook
          (lambda ()
            (evz/setup-c-mode-completion-behavior)
            (local-set-key (kbd "C-M-p") 'c-beginning-of-defun)
            (local-set-key (kbd "C-M-n") 'evz/beginning-of-next-defun)
            (local-set-key (kbd "C-M-N") 'c-end-of-defun)
            (local-set-key (kbd "C-;") 'evz/toggle-folding)
            (local-set-key (kbd "C-c f t") 'evz/toggle-folding) ;; f(old) t(oggle)
            (local-set-key (kbd "C-c f s") 'hs-show-all) ;; f(old) s(how all)
            (local-set-key (kbd "C-c f h") 'hs-hide-all) ;; f(old) h(ide all)
            ))

(add-hook 'c-mode-common-hook
          (lambda ()
            (hs-minor-mode) ;; enable code folding
          ))

