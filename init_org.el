;;(add-hook 'org-mode-hook
;;            (lambda () (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags-command))))


;; need to create templates for
;;   *done* confluence talk notes
;;   *done* scripture notes
;;   promptings

;; TODO: make links to web scriptures
;; TODO: make emacs understand how to render web scripture pages

(setq notes_file_folder "~/projects/notes/")
(setq work_notes_file (concat notes_file_folder "work.org"))
(setq notes_file (concat notes_file_folder "notes.org"))
(setq spiritual_notes_file (concat notes_file_folder "spiritual.org"))
(setq conference_notes_file (concat notes_file_folder "conference.org"))
(setq scripture_notes_file (concat notes_file_folder "scripture.org"))

(setq scripture_url "https://www.churchofjesuschrist.org/study/scriptures/%s/%s/%s?lang=eng")
(setq bofm_books '(("First Nephi" . "1-ne")
                   ("Second Nephi" . "2-ne")
                   ("Jacob" . "jacob")
                   ("Enos" . "enos")
                   ("Jarom" . "jarom")
                   ("Omni" . "omni")
                   ("Words of Mormon" . "w-of-m")
                   ("Mosiah" . "mosiah")
                   ("Alma" . "alma")
                   ("Helaman" . "hel")
                   ("Third Nephi" . "3-ne")
                   ("Fourth Nephi" . "4-ne")
                   ("Mormon" . "morm")
                   ("Ether" . "ether")
                   ("Moroni" . "moro")))

;; format is (cons "title of volume" (list "Full name of volume" "Abbrev of volume" "url form of volume" volume_book_list))
(setq volumes (list (cons "Old Testament" "ot")
                    (cons "New Testament" "nt")
                    (cons "Book of Mormon" (list "Book of Mormon" "BofM" "bofm" bofm_books))
                    (cons "Doctrine and Covenants" "dc-testament")))

(defun my/test ()
  (interactive)
  (let* ((volume (cdr (assoc (completing-read "Book: " (mapcar 'car volumes)) volumes)))
         (book (assoc (completing-read "Book: " (mapcar 'car (nth 3 volume))) (nth 3 volume)))
         (chapter (completing-read "Chapter: " nil)))
    (insert (format "[[%s][%s]]"
                    (format scripture_url
                            (nth 2 volume) ; get url form of volume; (cdr (assoc (completing-read "Book: " (mapcar 'car scripture_books)) scripture_books))
                            (cdr book) ; get url vorm of book (cdr (assoc (completing-read "Book: " (mapcar 'car bofm_books)) bofm_books))
                            chapter) ; get chapter (completing-read "Chapter: " nil))
                    "Description"))))

;;[[https://www.churchofjesuschrist.org/study/scriptures/bofm/2-ne/30?lang=eng][Description]]
                         


(setq org-capture-templates
      '(("c" "Conference Talk Note" entry (file+headline spiritual_notes_file "Conference Notes")
         "* %T %^g%^{Year}p%^{Month}p%^{Session}p%^{Speaker}p%^{Title}p\n%?")
        ("s" "Scripture Note" entry (file+headline spiritual_notes_file "Scripture Notes")
         "* %T %^g%^{Book}p%^{Chapter}p%^{Verse}p\n%?")
        ("e" "Meeting Note" entry (file+headline spiritual_notes_file "Meeting Notes")
         "* %T %^g\n%?")
        ("g" "General Note" entry (file+headline spiritual_notes_file "General Notes")
         "* %T %^g\n%?")
        ("m" "Mission Templates")
        ("mp" "Mission Plan" entry (file+olp spiritual_notes_file "Mission" "Plan")
         "* %^{Topic} %^g\n%?")
        ("mt" "Mission TODO" entry (file+olp spiritual_notes_file "Mission" "Todo")
         "* TODO %? %^g")
        
        ("mn" "Mission Note Templates")
        ("mng" "General Mission Note" entry (file+olp spiritual_notes_file "Mission" "Notes" "General")
         "* %T %^g\n%?")
        ("mns" "Spiritual Mission Note" entry (file+olp spiritual_notes_file "Mission" "Notes" "Spiritual")
         "* %T %^g\n%?")
        ("mnt" "Temporal Mission Note" entry (file+olp spiritual_notes_file "Mission" "Notes" "Temporal")
         "* %T %^g\n%?")))
        


(defun aj/org-completing-read-tags (prompt coll pred req initial hist def inh)
  ;; (if (not (or t (string= "Tags: " prompt)))
  ;;     ;; Not a tags prompt.  Use normal completion by calling
  ;;     ;; `org-completing-read' again without this function in
  ;;     ;; `helm-completing-read-handlers-alist'
  ;;     (let ((helm-completing-read-handlers-alist (rassq-delete-all
  ;;                                                 'aj/org-completing-read-tags
  ;;                                                 helm-completing-read-handlers-alist)))
  ;;       (org-completing-read prompt coll pred req initial hist def inh))
    ;; Tags prompt
  (let* ((initial (and (stringp initial)
                       (not (string= initial ""))
                       initial))
         (curr (when initial
                 (org-split-string initial ":")))
         (table (org-uniquify
                 (mapcar 'car org-last-tags-completion-table)))
         (table (if curr
                    ;; Remove current tags from list
                    (cl-delete-if (lambda (x)
                                    (member x curr))
                                  table)
                  table))
         (prompt (if initial
                     (concat "Tags " initial)
                   prompt)))
    (concat initial (mapconcat 'identity
                               (nreverse (aj/helm-completing-read-multiple
                                          prompt table pred nil nil hist def
                                          t "Org tags" "*Helm org tags*" ":"))
                               ":"))))

(defun aj/helm-completing-read-multiple (prompt choices
                                                &optional predicate require-match initial-input hist def
                                                inherit-input-method name buffer sentinel)
  "Read multiple items with `helm-completing-read-default-1'. Reading stops
when the user enters SENTINEL. By default, SENTINEL is
\"*done*\". SENTINEL is disambiguated with clashing completions
by appending _ to SENTINEL until it becomes unique. So if there
are multiple values that look like SENTINEL, the one with the
most _ at the end is the actual sentinel value. See
documentation for `ido-completing-read' for details on the
other parameters."
  (let ((sentinel (or sentinel "*done*"))
        this-choice res done-reading)
    ;; Uniquify the SENTINEL value
    (while (cl-find sentinel choices)
      (setq sentinel (concat sentinel "_")))
    (setq choices (cons sentinel choices))
    ;; Read choices
    (while (not done-reading)
      (setq this-choice (helm-completing-read-default-1 prompt choices
                                                        predicate require-match initial-input hist def
                                                        inherit-input-method name buffer nil t))
      (if (equal this-choice sentinel)
          (setq done-reading t)
        (setq res (cons this-choice res))
        (setq prompt (concat prompt this-choice ":"))))
    res))


;; **** I think these are now obsolete because I changed things over to use org properties
;; **** But they might come in handy if I want to create some sort of custom search thing

;; change the below to capture other things in the file like
;; dates, speakers and talk titles
;; (defconst my/org-conference-speaker-re
;;   "^[[:blank:]]*Speaker:[[:blank:]]*\\([[:alnum:]\.]+\\( *[[:alnum:]\.]+\\)*\\)[[:blank:]]*$"
;;   "Regexp matching speakers in conference notes, stored in match group 1")

;; (defconst my/org-conference-talk-title-re
;;   "^[[:blank:]]*Title:[[:blank:]]*\\([[:alnum:]\.]+\\( *[[:alnum:]\.]+\\)*\\)[[:blank:]]*$"
;;   "Regexp matching talk titles in conference notes, stored in match group 1")

;; (defconst my/org-scripture-book-re
;; ;  "^[[:blank:]]*Scripture:[[:blank:]]*\\([[:alnum:]\.]+\\( *[[:alnum:]\.]+\\)*\\)[[:blank:]]*$"
;;   "^[[:blank:]]*Scripture:[[:blank:]]*\\([[:alnum:]\.&]+\\(?: *[[:alnum:]\.&]+\\)*\\)[[:blank:]]*\|.*$"
;;   "Regexp matching talk titles in conference notes, stored in match group 1")
;; ;
;; (defun my/org-get-conference-items (item-re)
;;   "Get a list of items from the conference notes"
;;   (org-with-point-at 1
;;     (let ((conference-items)
;;           (sort-fold-case t))      
;;       (while (re-search-forward item-re nil t)
;; 	(push (match-string-no-properties 1) conference-items))
;;       (cl-sort conference-items 'string-lessp :key 'downcase))))


(defun my/org-capture-fill-template (&optional template initial annotation)
  "Fill a template and return the filled template as a string.
The template may still contain \"%?\" for cursor positioning."
  (let* ((template (or template (org-capture-get :template)))
	 (buffer (org-capture-get :buffer))
	 (file (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
	 (time (let* ((c (or (org-capture-get :default-time) (current-time)))
		      (d (decode-time c)))
		 (if (< (nth 2 d) org-extend-today-until)
		     (encode-time 0 59 23 (1- (nth 3 d)) (nth 4 d) (nth 5 d))
		   c)))
	 (v-t (format-time-string (org-time-stamp-format nil) time))
	 (v-T (format-time-string (org-time-stamp-format t) time))
	 (v-u (format-time-string (org-time-stamp-format nil t) time))
	 (v-U (format-time-string (org-time-stamp-format t t) time))
	 (v-c (and kill-ring (current-kill 0)))
	 (v-x (or (org-get-x-clipboard 'PRIMARY)
		  (org-get-x-clipboard 'CLIPBOARD)
		  (org-get-x-clipboard 'SECONDARY)
		  ""))			;ensure it is a string
	 ;; `initial' and `annotation' might have been passed.  But if
	 ;; the property list has them, we prefer those values.
	 (v-i (or (plist-get org-store-link-plist :initial)
		  (and (stringp initial) (org-no-properties initial))
		  (org-capture-get :initial)
		  ""))
	 (v-a
	  (let ((a (or (plist-get org-store-link-plist :annotation)
		       annotation
		       (org-capture-get :annotation)
		       "")))
	    ;; Is the link empty?  Then we do not want it...
	    (if (equal a "[[]]") "" a)))
	 (l-re "\\[\\[\\(.*?\\)\\]\\(\\[.*?\\]\\)?\\]")
	 (v-A (if (and v-a (string-match l-re v-a))
		  (replace-match "[[\\1][%^{Link description}]]" nil nil v-a)
		v-a))
	 (v-l (if (and v-a (string-match l-re v-a))
		  (replace-match "[[\\1]]" nil nil v-a)
		v-a))
	 (v-n user-full-name)
	 (v-k (if (marker-buffer org-clock-marker)
		  (org-no-properties org-clock-heading)
		""))
	 (v-K (if (marker-buffer org-clock-marker)
		  (org-link-make-string
		   (format "%s::*%s"
			   (buffer-file-name (marker-buffer org-clock-marker))
			   v-k)
		   v-k)
		""))
	 (v-f (or (org-capture-get :original-file-nondirectory) ""))
	 (v-F (or (org-capture-get :original-file) ""))
	 (org-capture--clipboards
	  (delq nil
		(list v-i
		      (org-get-x-clipboard 'PRIMARY)
		      (org-get-x-clipboard 'CLIPBOARD)
		      (org-get-x-clipboard 'SECONDARY)
		      v-c))))
    (setq org-store-link-plist (plist-put org-store-link-plist :annotation v-a))
    (setq org-store-link-plist (plist-put org-store-link-plist :initial v-i))
    (unless template
      (setq template "")
      (message "no template") (ding)
      (sit-for 1))
    (save-window-excursion
      (org-switch-to-buffer-other-window (get-buffer-create "*Capture*"))
      (erase-buffer)
      (setq buffer-file-name nil)
      (setq mark-active nil)
      (insert template)
      (goto-char (point-min))
      ;; %[] insert contents of a file.
      (save-excursion
	(while (re-search-forward "%\\[\\(.+\\)\\]" nil t)
	  (let ((filename (expand-file-name (match-string 1)))
		(beg (copy-marker (match-beginning 0)))
		(end (copy-marker (match-end 0))))
	    (unless (org-capture-escaped-%)
	      (delete-region beg end)
	      (set-marker beg nil)
	      (set-marker end nil)
	      (condition-case error
		  (insert-file-contents filename)
		(error
		 (insert (format "%%![couldn not insert %s: %s]"
				 filename
				 error))))))))
      ;; Mark %() embedded elisp for later evaluation.
      (org-capture-expand-embedded-elisp 'mark)
      ;; Expand non-interactive templates.
      (let ((regexp "%\\(:[-A-Za-z]+\\|<\\([^>\n]+\\)>\\|[aAcfFikKlntTuUx]\\)"))
	(save-excursion
	  (while (re-search-forward regexp nil t)
	    ;; `org-capture-escaped-%' may modify buffer and cripple
	    ;; match-data.  Use markers instead.  Ditto for other
	    ;; templates.
	    (let ((pos (copy-marker (match-beginning 0)))
		  (end (copy-marker (match-end 0)))
		  (value (match-string 1))
		  (time-string (match-string 2)))
	      (unless (org-capture-escaped-%)
		(delete-region pos end)
		(set-marker pos nil)
		(set-marker end nil)
		(let* ((inside-sexp? (org-capture-inside-embedded-elisp-p))
		       (replacement
			(pcase (string-to-char value)
			  (?< (format-time-string time-string time))
			  (?:
			   (or (plist-get org-store-link-plist (intern value))
			       ""))
			  (?i
			   (if inside-sexp? v-i
			     ;; Outside embedded Lisp, repeat leading
			     ;; characters before initial place holder
			     ;; every line.
			     (let ((lead (concat "\n"
						 (org-current-line-string t))))
			       (replace-regexp-in-string "\n" lead v-i nil t))))
			  (?a v-a)
			  (?A v-A)
			  (?c v-c)
			  (?f v-f)
			  (?F v-F)
			  (?k v-k)
			  (?K v-K)
			  (?l v-l)
			  (?n v-n)
			  (?t v-t)
			  (?T v-T)
			  (?u v-u)
			  (?U v-U)
			  (?x v-x))))
		  (insert
		   (if inside-sexp?
		       ;; Escape sensitive characters.
		       (replace-regexp-in-string "[\\\"]" "\\\\\\&" replacement)
		     replacement))))))))
      ;; Expand %() embedded Elisp.  Limit to Sexp originally marked.
      (org-capture-expand-embedded-elisp)
      ;; Expand interactive templates.  This is the last step so that
      ;; template is mostly expanded when prompting happens.  Turn on
      ;; Org mode and set local variables.  This is to support
      ;; completion in interactive prompts.
      (let ((org-inhibit-startup t)) (org-mode))
      (org-clone-local-variables buffer "\\`org-")
      (let (strings)			; Stores interactive answers.
	(save-excursion
	  (let ((regexp "%\\^\\(?:{\\([^}]*\\)}\\)?\\([bCgGiLpstTuU]\\)?"))
	    (while (re-search-forward regexp nil t)
	      (let* ((items (and (match-end 1)
				 (save-match-data
				   (split-string (match-string-no-properties 1)
						 "|"))))
		     (key (match-string 2))
		     (beg (copy-marker (match-beginning 0)))
		     (end (copy-marker (match-end 0)))
		     (prompt (nth 0 items))
		     (default (nth 1 items))
		     (completions (nthcdr 2 items)))
		(unless (org-capture-escaped-%)
		  (delete-region beg end)
		  (set-marker beg nil)
		  (set-marker end nil)
		  (pcase key
		    ((or "G" "g")
		     (let* ((org-last-tags-completion-table
			     (org-global-tags-completion-table
			      (cond ((equal key "G") (org-agenda-files))
				    (file (list file))
				    (t nil))))
			    (org-add-colon-after-tag-completion t)
			    (ins (mapconcat
				  #'identity
				  (org-split-string
				   ;;(completing-read
                                   (aj/org-completing-read-tags
				    (if prompt (concat prompt ": ") "Tags: ")
				    'org-tags-completion-function nil nil nil
				    'org-tags-history nil nil)
				   "[^[:alnum:]_@#%]+")
				  ":")))
		       (when (org-string-nw-p ins)
			 (unless (eq (char-before) ?:) (insert ":"))
			 (insert ins)
			 (unless (eq (char-after) ?:) (insert ":"))
			 (when (org-at-heading-p) (org-align-tags)))))
                    ((or "s" "i" "b") ;Conference notes specific stuff.
                     (insert
                      (completing-read
                       (cond ((equal key "s") "Speaker: ")
                             ((equal key "i") "Talk Title: ")
                             ((equal key "b") "Book: "))
                       (save-excursion
                         (set-buffer (find-file-noselect spiritual_notes_file))
                         (my/org-get-conference-items (cond ((equal key "s") my/org-conference-speaker-re)
                                                            ((equal key "i") my/org-conference-talk-title-re)
                                                            ((equal key "b") my/org-scripture-book-re)))))))
                    
		    ((or "C" "L")
		     (let ((insert-fun (if (equal key "C") #'insert
					 (lambda (s) (org-insert-link 0 s)))))
		       (pcase org-capture--clipboards
			 (`nil nil)
			 (`(,value) (funcall insert-fun value))
			 (`(,first-value . ,_)
			  (funcall insert-fun
				   (read-string "Clipboard/kill value: "
						first-value
						'org-capture--clipboards
						first-value)))
			 (_ (error "Invalid `org-capture--clipboards' value: %S"
				   org-capture--clipboards)))))
		    ("p"
		     ;; We remove file properties inherited from
		     ;; target buffer so `org-read-property-value' has
		     ;; a chance to find allowed values in sub-trees
		     ;; from the target buffer.
		     (setq-local org-file-properties nil)
		     (let* ((origin (set-marker (make-marker)
						(org-capture-get :pos)
						(org-capture-get :buffer)))
			    ;; Find location from where to get allowed
			    ;; values.  If `:target-entry-p' is
			    ;; non-nil, the current headline in the
			    ;; target buffer is going to be a parent
			    ;; headline, so location is fine.
			    ;; Otherwise, find the parent headline in
			    ;; the target buffer.
			    (pom (if (org-capture-get :target-entry-p) origin
				   (let ((level (progn
						  (while (org-up-heading-safe))
						  (org-current-level))))
				     (org-with-point-at origin
				       (let ((l (if (org-at-heading-p)
						    (org-current-level)
						  most-positive-fixnum)))
					 (while (and l (>= l level))
					   (setq l (org-up-heading-safe)))
					 (if l (point-marker)
					   (point-min-marker)))))))
			    (value (org-read-property-value prompt pom)))
		       (org-set-property prompt value)))
		    ((or "t" "T" "u" "U")
		     ;; These are the date/time related ones.
		     (let* ((upcase? (equal (upcase key) key))
			    (org-end-time-was-given nil)
			    (time (org-read-date upcase? t nil prompt)))
		       (org-insert-time-stamp
			time (or org-time-was-given upcase?)
			(member key '("u" "U"))
			nil nil (list org-end-time-was-given))))
		    (`nil
		     ;; Load history list for current prompt.
		     (setq org-capture--prompt-history
			   (gethash prompt org-capture--prompt-history-table))
		     (push (org-completing-read
			    (concat (or prompt "Enter string")
				    (and default (format " [%s]" default))
				    ": ")
			    completions
			    nil nil nil 'org-capture--prompt-history default)
			   strings)
		     (insert (car strings))
		     ;; Save updated history list for current prompt.
		     (puthash prompt org-capture--prompt-history
			      org-capture--prompt-history-table))
		    (_
		     (error "Unknown template placeholder: \"%%^%s\""
			    key))))))))
	;; Replace %n escapes with nth %^{...} string.
	(setq strings (nreverse strings))
	(save-excursion
	  (while (re-search-forward "%\\\\\\([1-9][0-9]*\\)" nil t)
	    (unless (org-capture-escaped-%)
	      (replace-match
	       (nth (1- (string-to-number (match-string 1))) strings)
	       nil t)))))
      ;; Make sure there are no empty lines before the text, and that
      ;; it ends with a newline character or it is empty.
      (skip-chars-forward " \t\n")
      (delete-region (point-min) (line-beginning-position))
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (if (bobp) (delete-region (point) (line-end-position))
	(end-of-line)
	(delete-region (point) (point-max))
	(insert "\n"))
      ;; Return the expanded template and kill the capture buffer.
      (untabify (point-min) (point-max))
      (set-buffer-modified-p nil)
      (prog1 (buffer-substring-no-properties (point-min) (point-max))
	(kill-buffer (current-buffer))))))


;; override existing function, as mine is overridden when org-capture is run
;; and the hook doesn't run until after this function is called
(advice-add 'org-capture-fill-template :override #'my/org-capture-fill-template)


(defun my/org-set-tags-command (&optional arg)
  "Set the tags for the current visible entry.

When called with `\\[universal-argument]' prefix argument ARG, \
realign all tags
in the current buffer.

When called with `\\[universal-argument] \\[universal-argument]' prefix argument, \
unconditionally do not
offer the fast tag selection interface.

If a region is active, set tags in the region according to the
setting of `org-loop-over-headlines-in-active-region'.

This function is for interactive use only;
in Lisp code use `org-set-tags' instead."
  (interactive "P")
  (let ((org-use-fast-tag-selection
	 (unless (equal '(16) arg) org-use-fast-tag-selection)))
    (cond
     ((equal '(4) arg) (org-align-tags t))
     ((and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let (org-loop-over-headlines-in-active-region) ;  hint: infinite recursion.
	(org-map-entries
	 #'my/org-set-tags-command;org-set-tags-command
	 nil
	 (if (eq org-loop-over-headlines-in-active-region 'start-level)
	     'region-start-level
	   'region)
	 (lambda () (when (org-invisible-p) (org-end-of-subtree nil t))))))
     (t
      (save-excursion
	(org-back-to-heading)
	(let* ((all-tags (org-get-tags))
	       (table (setq org-last-tags-completion-table
			    (org--tag-add-to-alist
			     (and org-complete-tags-always-offer-all-agenda-tags
				  (org-global-tags-completion-table
				   (org-agenda-files)))
			     (or org-current-tag-alist (org-get-buffer-tags)))))
	       (current-tags
		(cl-remove-if (lambda (tag) (get-text-property 0 'inherited tag))
			      all-tags))
	       (inherited-tags
		(cl-remove-if-not (lambda (tag) (get-text-property 0 'inherited tag))
				  all-tags))
	       (tags
		(replace-regexp-in-string
		 ;; Ignore all forbidden characters in tags.
		 "[^[:alnum:]_@#%]+" ":"
		 (if (or (eq t org-use-fast-tag-selection)
			 (and org-use-fast-tag-selection
			      (delq nil (mapcar #'cdr table))))
		     (org-fast-tag-selection
		      current-tags
		      inherited-tags
		      table
		      (and org-fast-tag-selection-include-todo org-todo-key-alist))
		   (let ((org-add-colon-after-tag-completion (< 1 (length table))))
		     (org-trim (aj/org-completing-read-tags ;completing-read
				"Tags: "
				#'org-tags-completion-function
				nil nil (org-make-tag-string current-tags)
				'org-tags-history nil nil)))))))
	  (org-set-tags tags)))))))

(add-hook 'org-mode-hook
          (progn
            (lambda () (advice-add 'org-set-tags-command :override 'my/org-set-tags-command))
            (lambda () (push '(tags-tree . local) org-show-context-detail))))

;; opens the entire note (not just the heading) when using "C-c /" (org-sparse-tree)



