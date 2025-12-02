;; Org mode stuff
(require 'org)
(require 'helm-org)
(require 'org-download)


;; ----- KEYBOARD BINDINGS -----
;; Define C-c o as the Org prefix
(define-prefix-command 'my-org-prefix)
(global-set-key (kbd "C-c o") 'my-org-prefix)

;; Org mode key bindings under C-c o
(define-key my-org-prefix (kbd "a") 'org-agenda)          ;; Agenda
(define-key my-org-prefix (kbd "A") 'org-archive-subtree) ;; Archive
(define-key my-org-prefix (kbd "c") 'org-capture)         ;; Capture
(define-key my-org-prefix (kbd "l") 'org-store-link)      ;; Store link
(define-key my-org-prefix (kbd "L") 'org-insert-link)     ;; Insert link
(define-key my-org-prefix (kbd "t") 'org-todo)            ;; Change TODO state
(define-key my-org-prefix (kbd "p") 'org-set-property)    ;; Set property
(define-key my-org-prefix (kbd "b") 'org-switchb)         ;; Switch between org buffers
(define-key my-org-prefix (kbd "f") 'org-footnote-action) ;; Footnotes
(define-key my-org-prefix (kbd "s") 'org-schedule)        ;; Schedule task
(define-key my-org-prefix (kbd "d") 'org-deadline)        ;; Set deadline
(define-key my-org-prefix (kbd "r") 'org-refile)          ;; Refile heading
(define-key my-org-prefix (kbd "i") (lambda ()             ;; Open inbox
                                      (interactive)
                                      (find-file org-inbox-file)))
(define-key my-org-prefix (kbd "n") (lambda ()             ;; Open main notes file
                                      (interactive)
                                      (find-file org-notes-file)))
(define-key my-org-prefix (kbd "k") 'org-cut-subtree)      ;; Cut subtree
(define-key my-org-prefix (kbd "y") 'org-copy-subtree)     ;; Copy subtree
(define-key my-org-prefix (kbd "q") 'org-set-tags-command) ;;

;; ----- CUSTOM FUNCTIONS -----
(defun evz/org-book-titles ()
  "Return a list of TITLEs for all books in notes.org."
  (with-current-buffer (find-file-noselect "~/notes/notes.org")
    (org-map-entries
     (lambda ()
       (org-entry-get (point) "TITLE"))
     ;; match :book: tag or any tag you choose
     "+book")))

;; ----- BEHAVIOR CHANGES -----
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'org-download-enable)
;; show just an overview when startup
(setq org-startup-folded 'content)

;; make links use ids, and create id if it doesn't exist
(setq org-id-link-to-org-use-id t)

;; This fixes org-capture using helm to capture tags
(define-advice helm--completion-in-region (:around (helm-fun origfun start end collection &optional predicate) temporary-helm-crm-separator-for-tags)
    (setq tcrmds helm-crm-default-separator)
    ;; If the last command was any of these values, we're looking at tags most likely
    (when (or (member last-command '(org-capture org-ctrl-c-ctrl-c org-set-tags org-set-tags-command))
            ;;This is a workaround for completions when you've already started typing.
            (and (eq this-command 'crm-complete)
              (eq major-mode 'org-mode))
            ;; This is probably the only thing we really need, but it doesn't handle custom "Tags" prompts
            (and (active-minibuffer-window)
              (eq "Tags: " (minibuffer-prompt))))
      (setq helm-crm-default-separator ":"))
    ;; Call the original Helm Completion function with all the original arguments
    (funcall helm-fun  origfun start end collection predicate)
    (setq helm-crm-default-separator tcrmds))

(add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
(add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))

;; ----- LOOK CHANGES -----
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
;; set to t to not display the '+' for +strikethrough+ or '*' for *bold*
(setq org-hide-emphasis-markers nil) 
;; set to display /alpha as the unicode character alpha (only for display)
(setq org-pretty-entities nil)
;; replace asterisks with bullets. 
(setq org-modern-star 'replace)

;; ----- ORG FILE LOCATIONS -----
(setq org-notes-folder "~/notes/")
(defun notes-path (f) (concat org-notes-folder f))
(setq org-archive-location (concat org-notes-folder "archive.org::* From %s"))

(setq org-mobile-inbox-file (notes-path "mobile_inbox.org"))
(setq org-inbox-file        (notes-path "inbox.org"))
(setq org-notes-file        (notes-path "notes.org"))
(setq org-todo-file         (notes-path "todo.org"))
(setq org-archive-file      (notes-path "archive.org"))

(setq org-agenda-files (list org-mobile-inbox-file
                             org-inbox-file
                             org-notes-file
                             org-todo-file))
(setq org-refile-targets '((org-todo-file :maxlevel . 1)
                           (org-notes-file :maxlevel . 2)))

;; ----- ORG DOWNLOAD SETTINGS -----
(setq org-download-image-dir "~/notes/images/")
(setq org-download-method 'directory)


;; ----- CAPTURE TEMPLATES -----
(setq org-capture-templates
      '(

        ;; -- Notes Section (prefix: n) --
        ("n" "Notes")

        ;; General note
        ("nn" "General Note" entry
         (file+headline org-inbox-file "Notes")
         "* %^{Title} :%^G:\n%?")
         
        ;; Speaker -- NEEDS UPDATE
        ("ns" "Speaker Note" entry
         (file+headline org-inbox-file "Notes")
         "* %^{Title} :speaker:%^G:\n- Event type: %^{Board Plan|Second Look|Team Training|Other}\n- Speaker: %^{Speaker}\n- Date: %U\n%?")

        ;; Audio -- NEEDS UPDATE
        ("na" "Audio Note" entry
         (file+headline org-inbox-file "Notes")
         "* %^{Title} :audio:%^G:\n- Reference: %^{Audio Source} %^{Chapter/Time}\n- Date: %U\n%?")

        ;; Book
        ("nb" "Book Note" entry
         (file+headline org-inbox-file "Notes")
         "* %^{Book Note Title} :reading:\n:PROPERTIES:\n:BOOK: %^{Book|%(string-join (evz/org-book-titles) \"|\")}\n:PAGE: %^{Page}\n:END:\n%?")

        ;; Scripture (spiritual) -- NEEDS UPDATE
        ("nS" "Scripture Note" entry
         (file+headline org-inbox-file "Notes")
         "* %^{Title} :scripture:%^G:\n- Reference: %^{Scripture/Audio}\n- Date: %U\n%?")

        ;; -- End Notes Section --

        ;; TODO
        ("t" "Todo" entry
         (file+headline org-inbox-file "Tasks")
         "* TODO %^{Task} :%^G:\n- Date: %U\n%?")

        ("T" "Linked Todo" entry
         (file+headline org-inbox-file "Tasks")
         "* TODO %^{Task} :%^G:\n- Date: %U\nLink: %a\n%?")

        ;; Journal
        ("j" "Journal" entry
         (file+headline org-inbox-file "Journal")
         "* %U :journal:%^G:\n%?")

        ;; -- Add Entries Section (prefix: e) --
        ("e" "Add Entry")

        ;; Book Entry
        ("eb" "Book entry" entry
         (file+headline org-notes-file "Book Notes")
         "* %^{Title} :book:\n:PROPERTIES:\n:AUTHOR:   %^{Author}\n:TITLE:    %\\1\n:END:\n%?" :empty-lines 1)

        ;; -- End Add Entries Section --
        ))
