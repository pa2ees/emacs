;; keyboard shortcut: C-x (p)roject (b)lame
;; needed to remove the projectile key-binding for this (see init_projectile.el)
(global-set-key (kbd "C-x p b") 'git-messenger:popup-message)
(setq git-messenger:show-detail t)
(setq git-messenger:use-magit-popup t)

;; This is meant to be run upon throwing to 'git-messenger-loop and then schedule
;; a the popup-message to come up immediately on that new line
(defun evz/git-messenger-move-line-popup-message (go-forward)
  (forward-line (if go-forward 1 -1))
  (run-at-time 0 nil 'git-messenger:popup-message)
  t)

(defun evz/git-messenger-next-line ()
  "Move to the next line and show git-messenger popup"
  (interactive)
  (throw 'git-messenger-loop (evz/git-messenger-move-line-popup-message t)))

(defun evz/git-messenger-prev-line ()
  "Move to the next line and show git-messenger popup"
  (interactive)
  (throw 'git-messenger-loop (evz/git-messenger-move-line-popup-message nil)))

;; map the conventional next-line and previous-line key bindings 
(with-eval-after-load 'git-messenger
  (define-key git-messenger-map (kbd "C-n") #'evz/git-messenger-next-line)
  (define-key git-messenger-map (kbd "C-p") #'evz/git-messenger-prev-line))

