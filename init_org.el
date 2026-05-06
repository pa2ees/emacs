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
(setq org-hide-emphasis-markers t) 
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
(setq org-journal-path      (notes-path "journal/"))

(setq org-agenda-files (list org-mobile-inbox-file
                             org-inbox-file
                             org-notes-file
                             org-todo-file))
(setq org-refile-targets '((org-todo-file :maxlevel . 1)
                           (org-notes-file :maxlevel . 2)))

;; ----- ORG DOWNLOAD SETTINGS -----
(setq org-download-image-dir "~/notes/images/")
(setq org-download-method 'directory)

;; ----- LARGE CAPTURE TEMPLATE STRINGS -----
(defvar daily-journal-template
  "#+TITLE: Daily Journal
#+DATE: %<%Y-%b-%d>

* Metrics
** Sleep Quality: %?*/10
- Bed Time:
- Snooze Time: 
- Get Up Time: 
- Notes:
** Energy: */10
** Mood: */10
** Focus: */10
- Minutes Unfocused: 0

* Morning Routine [/]
- [ ] Make bed
- [ ] Exercise (audio) ([[id:699babac-f415-41cd-8961-9e386455cbff][Workout]]) 
- [ ] Shower
- [ ] Breakfast
- [ ] Vitamins
- [ ] Meditation
- [ ] Prayer
- [ ] Scripture Reading
- [ ] Scripture Memorization
- [ ] Review Daily Mastery/Focus ([[id:57eed00c-06d6-45f3-8142-77f97bf2e610][link]]) 

* Daily Todo [/]
- [ ] Reading
- [ ] Topics
- [ ] Audio
- [ ] DreamStream
- [ ] Share my Story
- [ ] Coach upline

* Evening Routine [/]
- [ ] Brush Teeth
- [ ] Prep for tomorrow
- [ ] Beauty Before Bed ([[id:f2330f95-8cae-4ae4-acfd-32c3d6b70da4][source]]) [/]
  - [ ] Review your day
  - [ ] Recall three beautiful moments [/]
    - [ ] 
    - [ ] 
    - [ ] 
  - [ ] Reprogram the mistakes and let go of the day
  - [ ] Preview/Visualize tomorrow

* Journal Entry

")

;; ----- CAPTURE TEMPLATES -----
(setq org-capture-templates
      `(
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
        ("j" "Journal" plain
         (file (lambda ()
                 (concat org-journal-path
                         (format-time-string "%Y-%m-%d")
                         ".org")))
         ;; "* Hello")
         ,daily-journal-template
         :immediate-finish t
         :jump-to-captured t)

        ;; -- Add Entries Section (prefix: e) --
        ("e" "Add Entry")

        ;; Book Entry
        ("eb" "Book entry" entry
         (file+headline org-notes-file "Book Notes")
         "* %^{Title} :book:\n:PROPERTIES:\n:AUTHOR:   %^{Author}\n:TITLE:    %\\1\n:END:\n%?" :empty-lines 1)

        ;; -- End Add Entries Section --
        ))


;; TEST GET METRICS ;;
;; (require 'cl-lib)
;; (require 'org-element)

;; (defun evz/get-org-keyword (key)
;;   " Extract the value of a specific #+PROPERTY or keyword."
;;   (org-element-map (org-element-parse-buffer) 'keyword
;;     (lambda (k)
;;       (when (string-equal (org-element-property :key k) key)
;;         (org-element-property :value k)))
;;     nil t))

;; (defun evz/parse-fraction (str)
;;   "Convert [n/m] or n/10 to a float percentage. Returns nil for '*/10' or '[/]'."
;;   (cond
;;    ((or (null str) (string-match-p "\\*/" str) (string-match-p "\[/\]" str)) nil)
;;    ((string-match "\\([0-9]+\\)/\\([0-9]+\\)" str)
;;     (let ((n (string-to-number (match-string 1 str)))
;;           (d (string-to-number (match-string 2 str))))
;;       (if (zerop d) 0.0 (/ (float n) d))))
;;    (t nil)))

;; (defun evz/extract-journal-metrics (file)
;;   "Parse a single journal FILE and return an alist of metrics."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (org-mode)
;;     (let* ((ast (org-element-parse-buffer))
;;            (date (evz/get-org-keyword "DATE"))
;;            (results `(("date" . ,date))))
      
;;       ;; Parse headings for percentages [n/m]
;;       (org-element-map ast 'headline
;;         (lambda (hl)
;;           (let ((title (org-element-property :raw-value hl)))
;;             (cond
;;              ((string-match "Morning Routine" title)
;;               (push (cons "morning_pct" (evz/parse-fraction title)) results))
;;              ((string-match "Daily Todo" title)
;;               (push (cons "todo_pct" (evz/parse-fraction title)) results))
;;              ((string-match "Evening Routine" title)
;;               (push (cons "evening_pct" (evz/parse-fraction title)) results))))))

;;       ;; Parse list items for key-value metrics
;;       (org-element-map ast 'item
;;         (lambda (it)
;;           (let ((plain-text (org-element-interpret-data (org-element-contents it))))
;;             (cond
;;              ((string-match "Sleep Quality: \\([0-9/\\*]+\\)" plain-text)
;;               (push (cons "sleep_qual" (evz/parse-fraction (match-string 1 plain-text))) results))
;;              ((string-match "Bed Time: \\([0-9:]+\\)" plain-text)
;;               (push (cons "bed_time" (match-string 1 plain-text)) results))
;;              ((string-match "Snooze Time: \\([0-9]+\\)" plain-text)
;;               (push (cons "snooze" (match-string 1 plain-text)) results))
;;              ((string-match "Get Up Time: \\([0-9:]+\\)" plain-text)
;;               (push (cons "wake_time" (match-string 1 plain-text)) results))
;;              ((string-match "Energy: \\([0-9/\\*]+\\)" plain-text)
;;               (push (cons "energy" (evz/parse-fraction (match-string 1 plain-text))) results))
;;              ((string-match "Mood: \\([0-9/\\*]+\\)" plain-text)
;;               (push (cons "mood" (evz/parse-fraction (match-string 1 plain-text))) results))
;;              ((string-match "Focus: \\([0-9/\\*]+\\)" plain-text)
;;               (push (cons "focus" (evz/parse-fraction (match-string 1 plain-text))) results))
;;              ((string-match "Minutes Unfocused: \\([0-9]+\\)" plain-text)
;;               (push (cons "unfocused_min" (match-string 1 plain-text)) results))))))
;;       results)))

;; (defun evz/export-journal-stats (directory)
;;   "Parse all org files in DIRECTORY and output CSV to a buffer."
;;   (interactive "DJournal Directory: ")
;;   (let ((files (directory-files directory t "\\.org$"))
;;         (out-buf (get-buffer-create "*Journal Export*")))
;;     (with-current-buffer out-buf
;;       (erase-buffer)
;;       (insert "date,sleep_qual,bed_time,wake_time,snooze,energy,mood,focus,unfocused_min,morning_pct,todo_pct,evening_pct\n")
;;       (dolist (file files)
;;         (let ((data (evz/extract-journal-metrics file)))
;;           (insert (format "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n"
;;                           (cdr (assoc "date" data))
;;                           (or (cdr (assoc "sleep_qual" data)) "")
;;                           (or (cdr (assoc "bed_time" data)) "")
;;                           (or (cdr (assoc "wake_time" data)) "")
;;                           (or (cdr (assoc "snooze" data)) "")
;;                           (or (cdr (assoc "energy" data)) "")
;;                           (or (cdr (assoc "mood" data)) "")
;;                           (or (cdr (assoc "focus" data)) "")
;;                           (or (cdr (assoc "unfocused_min" data)) "")
;;                           (or (cdr (assoc "morning_pct" data)) "")
;;                           (or (cdr (assoc "todo_pct" data)) "")
;;                           (or (cdr (assoc "evening_pct" data)) "")))))
;;       (switch-to-buffer out-buf))))

;; ;;; -*- lexical-binding: t -*-

;; (defun evz/test-get-keyword (key)
;;   "Interactively test `evz/get-org-keyword` for KEY in the current buffer."
;;   (interactive "sEnter Org Keyword (e.g. DATE): ")
;;   (let ((val (evz/get-org-keyword key)))
;;     (message "Value for #+ %s: %s" key (or val "NOT FOUND"))))

;; (defun evz/test-parse-fraction (input)
;;   "Interactively test `evz/parse-fraction` with a string INPUT."
;;   (interactive "sEnter fraction string (e.g. 9/10, */10, [1/3]): ")
;;   (let ((val (evz/parse-fraction input)))
;;     (message "Parsed Value: %s (Type: %s)" 
;;              (or val "nil") 
;;              (type-of val))))

;; (defun evz/test-extract-current-buffer ()
;;   "Run the full extraction logic on the current buffer and display the alist."
;;   (interactive)
;;   (if (derived-mode-p 'org-mode)
;;       (let ((file (buffer-file-name)))
;;         (if file
;;             (message "Extracted Data: %S" (evz/extract-journal-metrics file))
;;           (user-error "Buffer must be visited from a file")))
;;     (user-error "Not in an Org-mode buffer")))

;; (defun evz/get-item (item-name)
;;   (interactive "sEnter Name: ")
;;   (org-element-map ast 'item
;;     (lambda (it)
;;       (let ((plain-text (org-element-interpret-data (org-element-contents it))))
;;         (cond
;;          ((string-match (concat item-name "\\(.*\\)") plain-text)
;;           (match-string 1 plain-text)))))))

;; (defun evz/collect-headline-names ()
;;   "Collect the title of every headline in the current buffer and message the list."
;;   (interactive)
;;   (let (headlines)
;;     (org-map-entries
;;      (lambda ()
;;        (push (org-get-heading t t t t) headlines))
;;      t 'buffer) ; 'buffer' scopes it to the current file only
;;     (let ((result (nreverse headlines)))
;;       (message "Headlines found: %S" result)
;;       result)))

;; (defun evz/collect-raw-headline-lines (item-name)
;;   (org-map-entries ; FUNC &optional MATCH SCOPE
;;    (lambda ()
;;      (let ((line-text (org-get-heading t t t t)))
;;        (if (string-match item-name line-text)
;;            (match-string 1 line-text)
;;          nil)))
;;    t 'file))

;; (defun evz/get-metric-by-pattern (item-name)
;;   (let ((all-results (org-map-entries
;;                       (lambda ()
;;                         (let ((line-text (org-get-heading t t t t)))
;;                           (when (string-match item-name line-text)
;;                             (match-string 1 line-text))))
;;                       t 'file)))
;;     (cl-find-if #'identity all-results)))

;; (setq res "")
;; (message "%s" (cadr res))

;; (defun evz/collect-all-metrics ()
;;   (interactive)
;;   (let* ((targets '(("sleep_quality" . "Sleep Quality: \\([0-9/\\*]+\\)")
;;                     ("energy" . "Energy: \\([0-9/\\*]+\\)")))
;;          (results (cl-loop for (key . pattern) in targets
;;                            collect (cons key (evz/parse-fraction (evz/get-metric-by-pattern pattern))))))
;;     (setq res results)))

;; (setq hello-str "hello there")
;; (string-match "hello \\(.*\\)" hello-str)
;; (match-string 1 hello-str)

;; (defun evz/parse-fraction (str)
;;   (let* ((match (string-match "\\([0-9]+\\)/\\([0-9]+\\)" str))
;;          (n (match-string 1 str))
;;          (d (match-string 2 str))
;;          (bad-match (or (not n) (not d) (zerop (string-to-number d)))))
;;     (if bad-match
;;         nil
;;       (/ (float (string-to-number n)) (string-to-number d)))))

;; (float "4")
;; (evz/parse-fraction "[1/10]")
;; (setq str "[/]")
;; (string-match "\\([0-9]+\\)/\\([0-9]+\\)" str)
;; (string-to-number (match-string 2 str))
;; (if d (message "howdy") (message "nope"))
;; (setq d nil)
;; (setq n 5)
;; (or (not d) (zerop d))
;; (/ (float 3) nil)
;; (string-to-number "*")
    
