;;; agent-shell-context-inject.el --- Simple context injection for agent-shell -*- lexical-binding: t; -*-

;; Simple system to inject context files into agent-shell sessions.
;; Looks for .md files in ~/.config/claude/context-presets/ (or future LLM dirs)

;;; Code:

(require 'agent-shell)

;; ============================================================================
;; Configuration
;; ============================================================================

(defvar evz/agent-shell-context-base-dir "~/.config/"
  "Base directory for LLM context presets.")

(defvar evz/agent-shell-current-llm "claude"
  "Current LLM being used (for finding context directory).")

(defun evz/agent-shell-context--get-preset-dir ()
  "Get the context preset directory for current LLM."
  (expand-file-name
   (concat evz/agent-shell-current-llm "/context-presets/")
   evz/agent-shell-context-base-dir))

(defun evz/agent-shell-context--list-presets ()
  "Return list of available context presets (without .md extension)."
  (let ((preset-dir (evz/agent-shell-context--get-preset-dir)))
    (when (file-directory-p preset-dir)
      (mapcar (lambda (f) (file-name-sans-extension f))
              (directory-files preset-dir nil "\\.md$")))))

(defun evz/agent-shell-context--build-inject-message (preset-names)
  "Build message to inject context for PRESET-NAMES."
  (let* ((preset-dir (evz/agent-shell-context--get-preset-dir))
         (file-paths (mapcar (lambda (name)
                              (expand-file-name (concat name ".md") preset-dir))
                            preset-names)))
    (concat "Read the following context files:\n"
            (mapconcat (lambda (path) (format "- %s" path))
                      file-paths
                      "\n")
            "\n\nUse this context for our session.")))

;; ============================================================================
;; Interactive Command
;; ============================================================================

(defun evz/agent-shell-inject-context ()
  "Inject context preset into current agent-shell session.
Presents menu of available .md files from context-presets directory.
Call multiple times to inject multiple contexts."
  (interactive)

  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))

  (let* ((presets (evz/agent-shell-context--list-presets))
         ;; Create alist with display format "preset-name (llm)"
         (candidates (mapcar (lambda (preset)
                              (cons (format "%s (%s)" preset evz/agent-shell-current-llm)
                                    preset))
                            presets))
         (selected (if candidates
                      (completing-read
                       "Apply context: "
                       candidates
                       nil t)
                    (user-error "No context presets found in %s"
                              (evz/agent-shell-context--get-preset-dir))))
         ;; Extract just the preset name from the selection
         (preset-name (cdr (assoc selected candidates))))

    (when preset-name
      (let ((message (evz/agent-shell-context--build-inject-message (list preset-name))))
        (goto-char (point-max))
        (insert message)
        (message "Injected context: %s (%s)" preset-name evz/agent-shell-current-llm)))))

;; ============================================================================
;; Management Commands
;; ============================================================================

(defun evz/agent-shell-context-open-presets-dir ()
  "Open the context presets directory in dired."
  (interactive)
  (let ((dir (evz/agent-shell-context--get-preset-dir)))
    (if (file-directory-p dir)
        (dired dir)
      (when (y-or-n-p (format "Directory %s doesn't exist. Create it? " dir))
        (make-directory dir t)
        (dired dir)))))

(defun evz/agent-shell-context-edit-preset ()
  "Edit a context preset file."
  (interactive)
  (let* ((presets (evz/agent-shell-context--list-presets))
         (selected (if presets
                      (completing-read "Edit preset: " presets nil t)
                    (user-error "No presets found")))
         (file-path (expand-file-name
                    (concat selected ".md")
                    (evz/agent-shell-context--get-preset-dir))))
    (find-file file-path)))

(defun evz/agent-shell-context-new-preset ()
  "Create a new context preset."
  (interactive)
  (let* ((name (read-string "Preset name (without .md): "))
         (file-path (expand-file-name
                    (concat name ".md")
                    (evz/agent-shell-context--get-preset-dir))))

    (when (string-match-p "[^a-zA-Z0-9_-]" name)
      (user-error "Preset name should only contain letters, numbers, hyphens, and underscores"))

    (when (file-exists-p file-path)
      (user-error "Preset '%s' already exists" name))

    (find-file file-path)
    (insert (format "# %s Context\n\n" (capitalize name)))
    (insert "<!-- Description of what this context provides -->\n\n")
    (save-buffer)
    (message "Created new preset: %s" name)))

(defun evz/agent-shell-context-list-presets ()
  "List available context presets."
  (interactive)
  (let ((presets (evz/agent-shell-context--list-presets))
        (dir (evz/agent-shell-context--get-preset-dir)))

    (with-help-window "*Agent Shell Context Presets*"
      (princ (format "Context Presets for: %s\n" evz/agent-shell-current-llm))
      (princ (format "Location: %s\n\n" dir))
      (princ "════════════════════════════════════════════\n\n")

      (if presets
          (progn
            (princ "Available presets:\n\n")
            (dolist (preset presets)
              (princ (format "  • %s\n" preset))))
        (princ "No presets found.\n\n")
        (princ (format "Create presets in: %s\n" dir)))

      (princ "\n════════════════════════════════════════════\n")
      (princ "\nCommands:\n")
      (princ "  C-c a C          - Inject context (in agent-shell buffer)\n")
      (princ "  M-x evz/agent-shell-context-list-presets     - This list\n")
      (princ "  M-x evz/agent-shell-context-edit-preset      - Edit preset\n")
      (princ "  M-x evz/agent-shell-context-new-preset       - Create preset\n")
      (princ "  M-x evz/agent-shell-context-open-presets-dir - Open in dired\n"))))

(provide 'agent-shell-context-inject)
;;; agent-shell-context-inject.el ends here
