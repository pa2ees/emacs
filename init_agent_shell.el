;;; init_agent_shell.el --- Agent Shell MCP Server Configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; Package Configuration
;; ============================================================================

(use-package agent-shell
  :ensure t
  :bind (("C-c a a" . evz/agent-shell-auto)
         ("C-c a u" . agent-shell-show-usage)
         ("C-c a s" . evz/emacs-mcp-server-start)
         ("C-c a S" . evz/emacs-mcp-server-stop)
         ("C-c a b" . evz/agent-shell-bridge-start)
         ("C-c a B" . evz/agent-shell-bridge-stop)
         ("C-c a r" . evz/agent-shell-helpers-restart-all)
         ("C-c a l" . evz/aws-sso-login)
         ("C-c a d" . evz/agent-shell-bridge-toggle-debug))
  :hook (agent-shell-mode . evz/agent-shell-setup-cleanup-hook)
  :config
  (setq agent-shell-anthropic-claude-acp-command
        '("~/.config/claude/claude-acp-engine.sh")))

(use-package emacs-mcp-server
  :vc (:url "https://github.com/rhblind/emacs-mcp-server")
  :defer t)

;; ============================================================================
;; MCP Bridge - HTTP-to-Unix-Socket Bridge (raw forwarding)
;; ============================================================================

(defvar evz/agent-shell-bridge-sock-path nil
  "Path to the MCP Unix socket.")

(defvar evz/agent-shell-bridge-port nil
  "HTTP server port.")

(defvar evz/agent-shell-bridge-server-process nil
  "The HTTP server process.")

(defvar evz/agent-shell-bridge-debug nil
  "When non-nil, output debug messages to the bridge buffer.")

(defvar evz/agent-shell-bridge-buffer-name "*MCP Bridge*"
  "Name of the buffer for bridge messages.")

(defvar evz/agent-shell-bridge-url nil
  "The HTTP URL for this instance's MCP bridge.")

(defun evz/agent-shell-bridge--log (format-string &rest args)
  "Log a message to the bridge buffer."
  (with-current-buffer (get-buffer-create evz/agent-shell-bridge-buffer-name)
    (goto-char (point-max))
    (insert (apply #'format format-string args))
    (insert "\n")))

(defun evz/agent-shell-bridge--debug (format-string &rest args)
  "Log a debug message to the bridge buffer if evz/agent-shell-bridge-debug is non-nil."
  (when evz/agent-shell-bridge-debug
    (apply #'evz/agent-shell-bridge--log format-string args)))

(defun evz/agent-shell-bridge--generate-socket-path ()
  "Generate a unique socket path based on Emacs PID."
  (format "/tmp/emacs-mcp-server-emacs-mcp-%d.sock" (emacs-pid)))

(defun evz/agent-shell-bridge--find-free-port (&optional start-port)
  "Find a free port starting from START-PORT (default 8126)."
  (let ((port (or start-port 8126))
        (max-attempts 100)
        (found nil))
    (while (and (not found) (< (- port (or start-port 8126)) max-attempts))
      (condition-case nil
          (let ((test-proc (make-network-process
                           :name "port-test"
                           :server t
                           :host "127.0.0.1"
                           :service port
                           :family 'ipv4)))
            (delete-process test-proc)
            (setq found port))
        (error
         (setq port (1+ port)))))
    (or found (error "Could not find free port after %d attempts" max-attempts))))

(defun evz/agent-shell-bridge--init-paths ()
  "Initialize socket path and port if not already set."
  (unless evz/agent-shell-bridge-sock-path
    (setq evz/agent-shell-bridge-sock-path (evz/agent-shell-bridge--generate-socket-path)))
  (unless evz/agent-shell-bridge-port
    (setq evz/agent-shell-bridge-port (evz/agent-shell-bridge--find-free-port))))

(defun evz/agent-shell-bridge--read-ndjson-raw (process)
  "Read NDJSON lines as raw strings from PROCESS without parsing."
  (let ((lines '())
        (output ""))
    (with-current-buffer (process-buffer process)
      (setq output (buffer-string)))

    (evz/agent-shell-bridge--debug "[SOCK] reading NDJSON stream...")
    (evz/agent-shell-bridge--debug "[SOCK RAW OUTPUT] %s" (substring output 0 (min 200 (length output))))

    (dolist (line (split-string output "\n" t))
      (let ((trimmed (string-trim line)))
        (when (> (length trimmed) 0)
          (evz/agent-shell-bridge--debug "[SOCK LINE] %s..." (substring trimmed 0 (min 100 (length trimmed))))
          (push trimmed lines))))

    (setq lines (nreverse lines))
    (evz/agent-shell-bridge--debug "[SOCK] total messages received: %d" (length lines))
    lines))

(defun evz/agent-shell-bridge--handle-request (client-proc request-data)
  "Handle HTTP request from CLIENT-PROC with REQUEST-DATA."
  (let ((start-time (float-time)))

    (evz/agent-shell-bridge--log "\n==> REQUEST")
    (evz/agent-shell-bridge--debug "[HTTP] Body: %s..." (substring request-data 0 (min 200 (length request-data))))

    ;; Ensure newline termination for MCP
    (unless (string-suffix-p "\n" request-data)
      (setq request-data (concat request-data "\n")))

    (evz/agent-shell-bridge--debug "[SOCK] connecting to %s" evz/agent-shell-bridge-sock-path)

    ;; Create Unix socket connection
    (let* ((sock-buffer (generate-new-buffer " *mcp-bridge-sock*"))
           (sock-proc (make-network-process
                       :name "mcp-bridge-sock"
                       :buffer sock-buffer
                       :family 'local
                       :service evz/agent-shell-bridge-sock-path
                       :sentinel #'ignore)))

      (evz/agent-shell-bridge--debug "[SOCK] sending request")

      ;; Send request
      (process-send-string sock-proc request-data)

      ;; Signal end-of-request
      (process-send-eof sock-proc)

      ;; Wait for response (blocking, but simple)
      (while (process-live-p sock-proc)
        (accept-process-output sock-proc 0.1))

      ;; Read NDJSON response as raw strings
      (let* ((lines (evz/agent-shell-bridge--read-ndjson-raw sock-proc))
             (response-str (if lines
                              (car (last lines))
                            "{\"error\": \"no response from MCP server\"}"))
             (elapsed-ms (* 1000 (- (float-time) start-time))))

        (evz/agent-shell-bridge--debug "[FINAL RESPONSE] %s..." (substring response-str 0 (min 200 (length response-str))))
        (evz/agent-shell-bridge--log "<== RESPONSE | %.1fms" elapsed-ms)

        ;; Send HTTP response (raw string, no parsing/encoding)
        (let ((response-bytes (encode-coding-string response-str 'utf-8)))
          (process-send-string
           client-proc
           (format "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
                   (length response-bytes)
                   response-str))
          ;; Close the connection gracefully - sentinel will clean up buffer
          (delete-process client-proc))

        (kill-buffer sock-buffer)))))

(defun evz/agent-shell-bridge--parse-http-request (data)
  "Parse HTTP request DATA and return the body."
  (when (string-match "\r\n\r\n\\(.*\\)" data)
    (match-string 1 data)))

(defun evz/agent-shell-bridge--server-filter (proc data)
  "Filter function for HTTP server PROC receiving DATA."
  (evz/agent-shell-bridge--debug "[FILTER] received %d bytes" (length data))

  ;; Accumulate data in process buffer
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert data)

    ;; Check if we have complete HTTP request (ends with \r\n\r\n)
    (goto-char (point-min))
    (when (search-forward "\r\n\r\n" nil t)
      (let* ((request (buffer-string))
             (body (evz/agent-shell-bridge--parse-http-request request)))
        (evz/agent-shell-bridge--debug "[FILTER] parsed body: %s..." (when body (substring body 0 (min 100 (length body)))))
        (if body
            (progn
              (erase-buffer)  ; Clear buffer for next request
              (evz/agent-shell-bridge--handle-request proc body))
          (evz/agent-shell-bridge--log "[FILTER] WARNING: no body found in request"))))))

(defun evz/agent-shell-bridge--server-sentinel (proc event)
  "Sentinel for server PROC handling EVENT."
  (evz/agent-shell-bridge--debug "[SENTINEL] %s event=%s" (process-name proc) (string-trim event))
  (unless (process-live-p proc)
    (let ((buf (process-buffer proc)))
      (evz/agent-shell-bridge--debug "[SENTINEL] cleaning up buffer: %s" (when buf (buffer-name buf)))
      (when (and buf (buffer-live-p buf))
        (kill-buffer buf)))))

(defun evz/agent-shell-bridge--accept-connection (server client message)
  "Accept new CLIENT connection on SERVER with MESSAGE."
  (evz/agent-shell-bridge--debug "[SERVER] new connection from %s" message)
  ;; Use the auto-created buffer, we'll clean it up in the sentinel
  (set-process-filter client #'evz/agent-shell-bridge--server-filter)
  (set-process-sentinel client #'evz/agent-shell-bridge--server-sentinel))

;; ============================================================================
;; User Commands
;; ============================================================================

(defun evz/emacs-mcp-server-running-p ()
  "Return non-nil if MCP server is running."
  (and (boundp 'mcp-server-transport-unix--server-process)
       mcp-server-transport-unix--server-process
       (process-live-p mcp-server-transport-unix--server-process)))

(defun evz/agent-shell-bridge-running-p ()
  "Return non-nil if bridge is running."
  (and evz/agent-shell-bridge-server-process
       (process-live-p evz/agent-shell-bridge-server-process)))

(defun evz/emacs-mcp-server-start ()
  "Start the MCP server with PID-based socket path."
  (interactive)
  (when (evz/emacs-mcp-server-running-p)
    (message "MCP server already running")
    (return))

  (require 'mcp-server)
  (setq mcp-server-security-prompt-for-permissions t)
  (setq mcp-server-socket-directory "/tmp/")
  (setq mcp-server-socket-name (format "emacs-mcp-%d" (emacs-pid)))
  (mcp-server-start-unix)
  (message "MCP server started: %s" (mcp-server-get-socket-path)))

(defun evz/emacs-mcp-server-stop ()
  "Stop the MCP server."
  (interactive)
  (unless (evz/emacs-mcp-server-running-p)
    (message "MCP server not running")
    (return))

  (mcp-server-stop)
  (message "MCP server stopped"))

(defun evz/agent-shell-bridge-start ()
  "Start the MCP HTTP bridge with dynamic socket and port."
  (interactive)
  (when (evz/agent-shell-bridge-running-p)
    (message "Bridge already running on port %d" evz/agent-shell-bridge-port)
    (return))

  (unless (evz/emacs-mcp-server-running-p)
    (user-error "MCP server is not running. Start it first with evz/emacs-mcp-server-start"))

  ;; Initialize paths if needed
  (evz/agent-shell-bridge--init-paths)

  ;; Use the same socket path that MCP server is using
  (setq evz/agent-shell-bridge-sock-path (mcp-server-get-socket-path))

  (setq evz/agent-shell-bridge-server-process
        (make-network-process
         :name "mcp-http-bridge"
         :server t
         :host "127.0.0.1"
         :service evz/agent-shell-bridge-port
         :family 'ipv4
         :filter-multibyte nil
         :coding 'binary
         :log #'evz/agent-shell-bridge--accept-connection))

  ;; Store the URL for agent-shell configuration
  (setq evz/agent-shell-bridge-url (format "http://127.0.0.1:%d" evz/agent-shell-bridge-port))

  ;; Configure agent-shell to use this bridge
  (setq agent-shell-mcp-servers
        `(((name . "emacs")
           (type . "http")
           (url . ,evz/agent-shell-bridge-url)
           (headers . []))))

  (evz/agent-shell-bridge--log "🚀 MCP HTTP bridge listening on http://127.0.0.1:%d" evz/agent-shell-bridge-port)
  (evz/agent-shell-bridge--log "🔌 forwarding to unix socket: %s" evz/agent-shell-bridge-sock-path)
  (message "Bridge started: socket=%s port=%d url=%s"
           evz/agent-shell-bridge-sock-path evz/agent-shell-bridge-port evz/agent-shell-bridge-url))

(defun evz/agent-shell-bridge-stop ()
  "Stop the MCP HTTP bridge server."
  (interactive)
  (unless (evz/agent-shell-bridge-running-p)
    (message "Bridge not running")
    (return))

  (delete-process evz/agent-shell-bridge-server-process)
  (setq evz/agent-shell-bridge-server-process nil)
  (evz/agent-shell-bridge--log "Bridge server stopped.")
  (message "Bridge stopped"))

(defun evz/agent-shell-bridge-toggle-debug ()
  "Toggle debug output for the bridge."
  (interactive)
  (setq evz/agent-shell-bridge-debug (not evz/agent-shell-bridge-debug))
  (message "Bridge debug %s" (if evz/agent-shell-bridge-debug "enabled" "disabled")))

(defun evz/aws-sso-login ()
  "Run the AWS SSO Bedrock login script asynchronously."
  (interactive)
  (let ((script-path (expand-file-name "~/.config/claude/claude-login.sh")))
    (async-shell-command script-path "*AWS SSO Login*")))

(defun evz/agent-shell-helpers-restart-all ()
  "Restart both MCP server and bridge."
  (interactive)
  (when (evz/agent-shell-bridge-running-p)
    (evz/agent-shell-bridge-stop))
  (when (evz/emacs-mcp-server-running-p)
    (evz/emacs-mcp-server-stop))
  (sit-for 0.5)
  (evz/emacs-mcp-server-start)
  (sit-for 0.5)
  (evz/agent-shell-bridge-start))

(defun evz/agent-shell-cleanup ()
  "Cleanup function to stop bridge and MCP server when agent-shell buffer is killed."
  (when (evz/agent-shell-bridge-running-p)
    (message "Stopping bridge...")
    (evz/agent-shell-bridge-stop))
  (when (evz/emacs-mcp-server-running-p)
    (message "Stopping MCP server...")
    (evz/emacs-mcp-server-stop)))

(defun evz/agent-shell-setup-cleanup-hook ()
  "Add cleanup hook to agent-shell-mode buffers."
  (when (derived-mode-p 'agent-shell-mode)
    (add-hook 'kill-buffer-hook #'evz/agent-shell-cleanup nil t)))

(defun evz/agent-shell-auto ()
  "Start agent-shell, ensuring MCP server and bridge are running."
  (interactive)

  ;; Check if in project
  (unless (projectile-project-root)
    (unless (y-or-n-p "Not in a project. Really start agent-shell? ")
      (user-error "Cancelled")))

  ;; Ensure MCP server is running
  (unless (evz/emacs-mcp-server-running-p)
    (message "Starting MCP server...")
    (evz/emacs-mcp-server-start)
    (sit-for 0.5))

  ;; Ensure bridge is running
  (unless (evz/agent-shell-bridge-running-p)
    (message "Starting bridge...")
    (evz/agent-shell-bridge-start)
    (sit-for 0.5))

  ;; Start agent-shell
  (call-interactively 'agent-shell)

  ;; Add cleanup hook to the current buffer if it's agent-shell
  (evz/agent-shell-setup-cleanup-hook))

;; ============================================================================
;; Status Display
;; ============================================================================

(defun evz/agent-shell-helpers-show-status ()
  "Show status of MCP server and bridge."
  (interactive)
  (let ((mcp-status (if (evz/emacs-mcp-server-running-p) "RUNNING" "STOPPED"))
        (bridge-status (if (evz/agent-shell-bridge-running-p)
                          (format "RUNNING on port %d" evz/agent-shell-bridge-port)
                        "STOPPED"))
        (project-status (if (projectile-project-root) "YES" "NO")))
    (message "MCP Server: %s | Bridge: %s | In Project: %s"
             mcp-status bridge-status project-status)))

(provide 'init-agent-shell)
;;; init_agent_shell.el ends here
