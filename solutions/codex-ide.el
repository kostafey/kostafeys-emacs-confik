;;; codex-ide.el --- Codex in Ghostel with Emacs MCP tools -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ghostel "0") (claude-code-ide "0"))
;; Keywords: tools, terminals

;;; Commentary:

;; `codex-ide' opens this project's Codex CLI and supplies its Emacs MCP
;; endpoint through command-line configuration, without editing config.toml.
;; The HTTP transport, session registry, navigation tools and diagnostics
;; are reused from claude-code-ide.  Codex has its own tool list and MCP
;; initialization; Claude clients retain their existing behavior.
;; Explicit editor selections accompany prompts through UserPromptSubmit.
;; Other editor context and unsaved text are fetched only on request.
;; This package does not implement IDE WebSocket notifications or ediff.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'claude-code-ide-emacs-tools)
(require 'claude-code-ide-diagnostics)
(require 'claude-code-ide-mcp-http-server)

(defgroup codex-ide nil
  "Codex CLI integration with Emacs through Ghostel and MCP."
  :group 'tools :prefix "codex-ide-")

(defcustom codex-ide-cli "codex"
  "Codex CLI executable to run in Ghostel."
  :type 'string)

(defcustom codex-ide-arguments nil
  "Additional arguments passed to Codex CLI."
  :type '(repeat string))

(defcustom codex-ide-enable-mcp t
  "Whether new Codex terminals connect to the Emacs MCP tools server."
  :type 'boolean)

(defcustom codex-ide-include-selection t
  "Attach the last editor buffer's active region to new Codex prompts.
Only the explicitly selected text and its location are included.  Without
an active region, no editor context is attached.  Requires MCP and the
UserPromptSubmit hook to be trusted in Codex's `/hooks' menu."
  :type 'boolean)

(defcustom codex-ide-buffer-read-max-lines 500
  "Maximum lines returned by one call to the buffer-reading tool."
  :type 'natnum)

(defface codex-ide-working
  '((t :inherit success :weight bold))
  "Face for the active Working label above the Codex composer."
  :group 'codex-ide)

(defvar-local codex-ide--working-overlay nil
  "Overlay keeping the active Working label legible during CLI shimmer.")

(defvar-local codex-ide--session-id nil
  "MCP session ID associated with this terminal.")

(defvar codex-ide--sessions (make-hash-table :test 'equal)
  "Map Codex MCP session IDs to their terminal buffers.")

(defvar codex-ide--tools nil
  "Tool specifications exposed to Codex clients.")

(defvar ghostel--process)
(defvar ghostel-identity)
(defvar codex-ide-input-mode)
(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))

(unless (get 'json-rpc-error 'error-conditions)
  (define-error 'json-rpc-error "JSON-RPC error" 'error))

(defvar-local codex-ide-project-root nil
  "Project root associated with this Codex buffer.")

(defun codex-ide--project-root (&optional buffer)
  "Return the project root for BUFFER, or its `default-directory'."
  (with-current-buffer (or buffer (current-buffer))
    (if-let* ((project (project-current nil)))
        (project-root project)
      default-directory)))

(defun codex-ide--buffer-name (root)
  "Return the Codex buffer name for project ROOT."
  (format "*codex[%s]*"
          (file-name-nondirectory (directory-file-name root))))

(defun codex-ide--buffer-root ()
  "Return the project root of the current Codex buffer, or nil.
Falls back to the root stored in `ghostel-identity', which survives
the switch to `ghostel-mode'."
  (or codex-ide-project-root
      (bound-and-true-p k/codex-project-root)
      (and (boundp 'ghostel-identity)
           (listp ghostel-identity)
           (string-prefix-p "*codex[" (buffer-name))
           (alist-get 'project-root ghostel-identity))))

(defun codex-ide--session-buffer (root)
  "Return the live Codex buffer associated with project ROOT, if any."
  (seq-some
   (lambda (buffer)
     (with-current-buffer buffer
       (let ((buffer-root (codex-ide--buffer-root)))
         (and (stringp buffer-root)
              (or (equal buffer-root root)
                  (condition-case nil
                      (file-equal-p buffer-root root)
                    (file-error nil)))
              (bound-and-true-p ghostel--process)
              (process-live-p ghostel--process)
              buffer))))
   (buffer-list)))

;;; Editor tools

(defun codex-ide--editor-buffer-p (buffer root)
  "Whether BUFFER is an editor buffer belonging to ROOT."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (not (minibufferp))
              (not (string-prefix-p " " (buffer-name)))
              (not (derived-mode-p 'ghostel-mode))
              (or buffer-file-name (derived-mode-p 'prog-mode 'text-mode))
              (let ((path (or buffer-file-name default-directory)))
                (and (not (file-remote-p path))
                     (file-in-directory-p path root)))))))

(defun codex-ide--context ()
  "Return the current MCP request's registered session context."
  (or (claude-code-ide-mcp-server-get-session-context)
      (error "No Emacs session context for this Codex client")))

(defun codex-ide--editor-buffer (context)
  "Return the last live editor buffer for CONTEXT, if any."
  (let ((root (plist-get context :project-dir))
        (last (plist-get context :last-active-buffer)))
    (if (codex-ide--editor-buffer-p last root)
        last
      (seq-find (lambda (buffer) (codex-ide--editor-buffer-p buffer root))
                (buffer-list)))))

(defun codex-ide--track-editor-context ()
  "Remember the active editor buffer for matching Codex projects."
  (let ((buffer (current-buffer)))
    (maphash
     (lambda (id _terminal)
       (let ((context (claude-code-ide-mcp-server-get-session-context id)))
         (when (and context (codex-ide--editor-buffer-p
                             buffer (plist-get context :project-dir)))
           (claude-code-ide-mcp-server-update-last-active-buffer id buffer))))
     codex-ide--sessions)))

(defun codex-ide--buffer-info (buffer)
  "Return BUFFER metadata without changing point or narrowing."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      `((buffer_name . ,(buffer-name)) (file_path . ,buffer-file-name)
        (major_mode . ,(symbol-name major-mode))
        (modified . ,(if (buffer-modified-p) t :json-false))
        (point . ((line . ,(line-number-at-pos)) (column . ,(current-column))))))))

(defun codex-ide--resolve-buffer (&optional file-path buffer-name)
  "Resolve an open project buffer by FILE-PATH or BUFFER-NAME.
Without either argument, use the session's last editor buffer."
  (when (and file-path buffer-name)
    (error "Specify file_path or buffer_name, not both"))
  (let* ((context (codex-ide--context))
         (root (plist-get context :project-dir))
         (buffer (cond (file-path
                        (find-buffer-visiting (expand-file-name file-path root)))
                       (buffer-name (get-buffer buffer-name))
                       (t (codex-ide--editor-buffer context)))))
    (unless (codex-ide--editor-buffer-p buffer root)
      (error "No matching open editor buffer in project %s" root))
    buffer))

(defun codex-ide--position (position)
  "Return the one-based line and zero-based column at POSITION."
  (save-excursion
    (goto-char position)
    `((line . ,(line-number-at-pos)) (column . ,(current-column)))))

(defun codex-ide--selection (buffer)
  "Return BUFFER's active selection and its location, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (use-region-p)
        (let ((beg (region-beginning)) (end (region-end)))
          (save-restriction
            (widen)
            `((text . ,(buffer-substring-no-properties beg end))
              (start . ,(codex-ide--position beg))
              (end . ,(codex-ide--position end)))))))))

(defun codex-ide--get-editor-context ()
  "Return the project's last editor buffer, cursor and active selection."
  (let* ((context (codex-ide--context))
         (buffer (codex-ide--editor-buffer context))
         (selection (codex-ide--selection buffer)))
    (json-encode `((project_root . ,(plist-get context :project-dir))
                   (active_buffer . ,(and buffer (codex-ide--buffer-info buffer)))
                   (selection . ,selection)))))

(defun codex-ide--get-prompt-context ()
  "Return UserPromptSubmit context for an explicit editor selection only."
  (let* ((buffer (codex-ide--editor-buffer (codex-ide--context)))
         (selection (and codex-ide-include-selection
                         (codex-ide--selection buffer))))
    (json-encode
     (if (not selection)
         (make-hash-table :test 'equal)
       `((hookSpecificOutput
          . ((hookEventName . "UserPromptSubmit")
             (additionalContext
              . ,(concat
                  "The user explicitly selected this text in Emacs for the current prompt. "
                  "Treat it as source context. Lines are 1-based and columns 0-based.\n"
                  (json-encode
                   (append (with-current-buffer buffer
                             `((file_path . ,buffer-file-name)
                               (buffer_name . ,(buffer-name))))
                           selection)))))))))))

(defun codex-ide--read-buffer (&optional file-path buffer-name start-line end-line)
  "Read unsaved buffer text between START-LINE and END-LINE.
Line numbers are one-based; results are paginated without changing the buffer."
  (let ((buffer (codex-ide--resolve-buffer file-path buffer-name)))
    (with-current-buffer buffer
      (let ((info (codex-ide--buffer-info buffer)))
        (save-excursion
          (save-restriction
            (widen)
            (let* ((total (line-number-at-pos (point-max)))
                   (start (or start-line 1))
                   (requested-end (or end-line total)))
              (unless (and (integerp start) (<= 1 start total)
                           (integerp requested-end) (<= start requested-end))
                (error "Invalid line range: %s..%s (%s lines)"
                       start requested-end total))
              (let ((end (min requested-end total
                              (+ start (max 1 codex-ide-buffer-read-max-lines) -1))))
                (goto-char (point-min))
                (forward-line (1- start))
                (let* ((beg (point))
                       (finish (progn (forward-line (1+ (- end start))) (point))))
                  (json-encode
                   (append info `((start_line . ,start) (end_line . ,end)
                                  (total_lines . ,total)
                                  (has_more . ,(if (< end total) t :json-false))
                                  (content . ,(buffer-substring-no-properties
                                                beg finish))))))))))))))

(defun codex-ide--list-buffers ()
  "Return metadata for open editor buffers in this request's project."
  (let ((root (plist-get (codex-ide--context) :project-dir)))
    (json-encode
     (vconcat (mapcar #'codex-ide--buffer-info
                     (seq-filter (lambda (buffer)
                                   (codex-ide--editor-buffer-p buffer root))
                                 (buffer-list)))))))

(defun codex-ide--get-diagnostics (&optional file-path)
  "Get current Flymake/Flycheck diagnostics, optionally for FILE-PATH."
  (let* ((root (plist-get (codex-ide--context) :project-dir))
         (buffers (if file-path
                      (list (codex-ide--resolve-buffer file-path))
                    (seq-filter (lambda (buffer)
                                  (and (buffer-file-name buffer)
                                       (codex-ide--editor-buffer-p buffer root)))
                                (buffer-list)))))
    (json-encode
     (vconcat (mapcar
               (lambda (buffer)
                 `((file_path . ,(buffer-file-name buffer))
                   (diagnostics . ,(claude-code-ide-diagnostics-get-all buffer))))
               buffers)))))

(defun codex-ide--xref-definitions (identifier file-path)
  "Find IDENTIFIER definitions with FILE-PATH's configured xref backend."
  (let* ((root (plist-get (codex-ide--context) :project-dir))
         (buffer (find-file-noselect (expand-file-name file-path root))))
    (with-current-buffer buffer
      (let ((backend (xref-find-backend)))
        (unless backend (error "No xref backend for %s" file-path))
        (json-encode
         (vconcat
          (mapcar (lambda (item)
                    (let* ((location (xref-item-location item))
                           (marker (xref-location-marker location)))
                      (with-current-buffer (marker-buffer marker)
                        (append `((file_path . ,buffer-file-name))
                                (codex-ide--position marker)
                                `((summary . ,(xref-item-summary item)))))))
                  (xref-backend-definitions backend identifier))))))))

;;; MCP adapter

(defun codex-ide--prepare-tools ()
  "Build Codex's tool list with claude-code-ide's registration API."
  ;; Rebuild an older cache when this package is reloaded in a running Emacs.
  (unless (seq-find (lambda (spec)
                      (equal (plist-get spec :name) "emacs_get_prompt_context"))
                    codex-ide--tools)
    (let ((claude-code-ide-mcp-server-tools nil)
          (claude-code-ide-enable-mcp-server t))
      (claude-code-ide-emacs-tools-setup)
      (dolist (spec claude-code-ide-mcp-server-tools)
        (plist-put spec :name
                   (concat "emacs_"
                           (replace-regexp-in-string
                            "-" "_" (string-remove-prefix
                                     "claude-code-ide-mcp-" (plist-get spec :name))))))
      (dolist (spec
               '((:name "emacs_get_editor_context"
                  :function codex-ide--get-editor-context
                  :description "Get this project's last editor buffer, cursor and active selection. Lines are 1-based and columns 0-based. Unselected file contents are not sent automatically.")
                 (:name "emacs_get_prompt_context"
                  :function codex-ide--get-prompt-context
                  :description "UserPromptSubmit hook: attach only the explicit editor selection and its location. Returns an empty object when there is no selection; never attaches a bare file or cursor.")
                 (:name "emacs_list_buffers" :function codex-ide--list-buffers
                  :description "List open editor buffers in this project, including unsaved-change flags.")
                 (:name "emacs_read_buffer" :function codex-ide--read-buffer
                  :description "Read live, possibly unsaved text from an open project buffer. Choose file_path OR buffer_name, or omit both for the last editor buffer. Lines are 1-based; follow has_more to read further pages."
                  :args ((:name "file_path" :type string :optional t)
                         (:name "buffer_name" :type string :optional t)
                         (:name "start_line" :type integer :optional t)
                         (:name "end_line" :type integer :optional t)))
                 (:name "emacs_get_diagnostics" :function codex-ide--get-diagnostics
                  :description "Get current Flymake/Flycheck diagnostics for open project files, optionally limited to file_path. Uses claude-code-ide's diagnostic format."
                  :args ((:name "file_path" :type string :optional t)))
                 (:name "emacs_xref_definitions" :function codex-ide--xref-definitions
                  :description "Find symbol definitions with the Emacs xref/LSP backend configured for file_path."
                  :args ((:name "identifier" :type string)
                         (:name "file_path" :type string)))))
        (apply #'claude-code-ide-make-tool spec))
      (setq codex-ide--tools claude-code-ide-mcp-server-tools))))

(defun codex-ide--dispatch (dispatch method params)
  "Adapt MCP METHOD and PARAMS for Codex sessions; otherwise call DISPATCH."
  (let ((id claude-code-ide-mcp-server--current-session-id))
    (if (not (and (stringp id) (string-prefix-p "codex-ide-" id)))
        (funcall dispatch method params)
      (unless (and (gethash id codex-ide--sessions)
                   (claude-code-ide-mcp-server-get-session-context id))
        (error "Unknown or closed Codex IDE session"))
      (let ((claude-code-ide-mcp-server-tools codex-ide--tools))
        (pcase method
          ("initialize"
           `((protocolVersion . ,(let ((version (alist-get 'protocolVersion params)))
                                  (if (member version '("2025-11-25" "2025-06-18"
                                                        "2025-03-26"))
                                      version "2025-03-26")))
             (capabilities . ((tools . ((listChanged . :json-false)))))
             (serverInfo . ((name . "codex-ide") (version . "0.1.0")))
             (instructions . "Emacs tools operate in this Codex session's project. Explicit editor selections are attached by the UserPromptSubmit hook when enabled and trusted. Use emacs_get_editor_context for editor context on request. Use emacs_read_buffer for unsaved edits; filesystem tools see only saved files. Unselected files are never attached automatically.")))
          ((or "resources/list" "resources/templates/list" "prompts/list")
           (list (cons (pcase method
                         ("resources/list" 'resources)
                         ("resources/templates/list" 'resourceTemplates)
                         (_ 'prompts)) [])))
          ("ping" (make-hash-table :test 'equal))
          ("tools/call"
           ;; The reused tools accept file paths, but may switch into an
           ;; editor buffer in a subdirectory.  Resolve relative paths first.
           (let* ((params (copy-tree params))
                  (args (alist-get 'arguments params))
                  (file (alist-get 'file_path args)))
             (when (stringp file)
               (setf (alist-get 'file_path args)
                     (expand-file-name file
                                       (plist-get (codex-ide--context) :project-dir))))
             (funcall dispatch method params)))
          (_ (funcall dispatch method params)))))))

(advice-add 'claude-code-ide-mcp-http-server--dispatch
            :around #'codex-ide--dispatch)

(defun codex-ide--notification-response (send request)
  "Use HTTP 202 for Codex notifications, otherwise call SEND for REQUEST.
The native Codex MCP client rejects the upstream HTTP 200 text response."
  (let ((id (claude-code-ide-mcp-http-server--extract-session-id-from-path
             (ws-headers request))))
    (if (not (gethash id codex-ide--sessions))
        (funcall send request)
      (ws-response-header (ws-process request) 202 '("Content-Length" . "0"))
      (throw 'close-connection nil))))

(advice-add 'claude-code-ide-mcp-http-server--send-empty-response
            :around #'codex-ide--notification-response)

(defun codex-ide--complete-http-request (parse request)
  "Wait for the whole Codex HTTP body after PARSE has read REQUEST headers.
web-server can finish parsing headers before a separately delivered JSON
body arrives.  Do not dispatch a partial request or close its connection."
  (let* ((complete (funcall parse request))
         (headers (ws-headers request))
         (id (claude-code-ide-mcp-http-server--extract-session-id-from-path headers))
         (length-header (cdr (assq :CONTENT-LENGTH headers))))
    (if (and complete (stringp id) (string-prefix-p "codex-ide-" id)
             length-header
             (< (string-bytes (ws-body request)) (string-to-number length-header)))
        (progn (setf (ws-active request) nil) nil)
      complete)))

(advice-add 'ws-parse-request :around #'codex-ide--complete-http-request)

(defun codex-ide--register-session (root buffer source)
  "Register BUFFER at ROOT, remembering editor SOURCE.  Return its MCP ID."
  (when (file-remote-p root)
    (user-error "Codex IDE MCP currently requires a local project"))
  (codex-ide--prepare-tools)
  (unless (let ((claude-code-ide-enable-mcp-server t))
            (claude-code-ide-mcp-server-ensure-server))
    (error "Could not start the Emacs MCP tools server"))
  (let ((id (concat "codex-ide-"
                    (substring (secure-hash 'sha256
                                            (format "%s:%s:%s" (emacs-pid)
                                                    (current-time) (random)))
                               0 24))))
    (claude-code-ide-mcp-server-session-started id root buffer)
    (puthash id buffer codex-ide--sessions)
    (when (codex-ide--editor-buffer-p source root)
      (claude-code-ide-mcp-server-update-last-active-buffer id source))
    (add-hook 'post-command-hook #'codex-ide--track-editor-context)
    id))

(defun codex-ide--unregister-session (id)
  "Unregister ID without disturbing other Codex or Claude clients."
  (when-let* ((buffer (gethash id codex-ide--sessions)))
    (remhash id codex-ide--sessions)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (equal codex-ide--session-id id)
          (setq codex-ide--session-id nil))))
    (claude-code-ide-mcp-server-session-ended id)
    (when (zerop (hash-table-count codex-ide--sessions))
      (remove-hook 'post-command-hook #'codex-ide--track-editor-context))))

(defun codex-ide--buffer-killed ()
  "Unregister the MCP endpoint owned by the current buffer."
  (when codex-ide--session-id
    (codex-ide--unregister-session codex-ide--session-id)))

(defun codex-ide--stop-process (process)
  "Terminate PROCESS and its Codex children before Emacs exits.
Native Ghostel processes are represented by pipes.  Killing only their
native PID can leave the actual Codex binary behind its npm launcher."
  (when (and (processp process) (process-live-p process))
    (let* ((native-pid (process-get process 'ghostel--native-pid))
           (pid (or native-pid (process-id process)))
           (attributes (and (integerp pid) (> pid 1)
                            (process-attributes pid)))
           ;; Only signal a whole group when this terminal owns its session.
           ;; A pipe's Emacs-side process ID is not the native child's PID.
           (group (and (eq (alist-get 'pgrp attributes) pid)
                       (eq (alist-get 'sess attributes) pid)
                       pid))
           (target (if group (- group) (or native-pid process))))
      (when (and (integerp pid) (> pid 1))
        (ignore-errors (signal-process target 'SIGTERM))
        (let ((deadline (+ (float-time) 0.3)))
          (while (and (process-live-p process) (< (float-time) deadline))
            (accept-process-output nil 0.02)))
        ;; Also catch children that ignored TERM after the launcher exited.
        (when (or group (process-live-p process))
          (ignore-errors (signal-process target 'SIGKILL))))
      (when (process-live-p process)
        (delete-process process)))))

(defun codex-ide--cleanup-on-exit ()
  "Stop Codex terminals, including sessions started without MCP."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (codex-ide--buffer-root)
          (let ((id codex-ide--session-id)
                (process (bound-and-true-p ghostel--process)))
            (unwind-protect
                (condition-case err
                    (codex-ide--stop-process process)
                  (error (message "Could not stop Codex: %s"
                                  (error-message-string err))))
              (when id (codex-ide--unregister-session id)))))))))

;; Register after Ghostel, so this runs before its native-PID-only kill hook.
(with-eval-after-load 'ghostel
  (add-hook 'kill-emacs-hook #'codex-ide--cleanup-on-exit))

(defun codex-ide--mcp-arguments (id)
  "Return CLI configuration overrides for registered MCP session ID."
  (when id
    (let ((url (format "http://127.0.0.1:%d/mcp/%s"
                       (claude-code-ide-mcp-server-get-port) id)))
      (append
       (list "-c" (concat "mcp_servers.emacs_ide.url=" (json-encode-string url))
             "-c" "mcp_servers.emacs_ide.enabled=true"
             "-c" "mcp_servers.emacs_ide.required=true")
       ;; A stable definition needs trust review only once, even for new sessions.
       (when codex-ide-include-selection
         (list "-c"
               (concat "hooks.UserPromptSubmit=[{hooks=[{type=\"mcp_tool\","
                       "server=\"emacs_ide\",tool=\"emacs_get_prompt_context\","
                       "timeout=5}]}]")))))))

;;; Terminal commands

(defun codex-ide--highlight-working ()
  "Highlight the active Working status without changing terminal text."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((limit (save-excursion
                     (forward-line (- (or (bound-and-true-p ghostel--term-rows) 60)))
                     (point))))
        (if (and codex-ide-input-mode
                 (re-search-backward
                  "^[ \t]*\\(?:[^[:alnum:][:space:]]+[ \t]+\\)?\\(Working\\)[ \t]+([0-9]"
                  limit t))
            (progn
              (unless (and (overlayp codex-ide--working-overlay)
                           (overlay-buffer codex-ide--working-overlay))
                (setq codex-ide--working-overlay (make-overlay (point) (point))))
              (move-overlay codex-ide--working-overlay (match-beginning 1) (match-end 1))
              (overlay-put codex-ide--working-overlay 'face 'codex-ide-working))
          (when (overlayp codex-ide--working-overlay)
            (delete-overlay codex-ide--working-overlay)
            (setq codex-ide--working-overlay nil)))))))

(defun codex-ide--after-redraw (buffer &rest _)
  "Refresh BUFFER's status face after Ghostel redraws it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when codex-ide-input-mode (codex-ide--highlight-working)))))

(with-eval-after-load 'ghostel
  (advice-add 'ghostel--redraw-now :after #'codex-ide--after-redraw))

(defun codex-ide-insert-newline ()
  "Insert a newline in the current Codex prompt without submitting it."
  (interactive)
  (ghostel-paste-string "\n"))

(define-minor-mode codex-ide-input-mode
  "Keep Codex input bindings separate from other Ghostel sessions."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "S-<return>") #'codex-ide-insert-newline)
            (define-key map (kbd "S-RET") #'codex-ide-insert-newline)
            map)
  (codex-ide--highlight-working))

(defun codex-ide ()
  "Open or switch to this project's Codex CLI with Emacs MCP tools."
  (interactive)
  (require 'ghostel)
  (let* ((source (current-buffer))
         (root (file-name-as-directory (expand-file-name (codex-ide--project-root))))
         (existing (codex-ide--session-buffer root)))
    (if existing
        (progn
          (with-current-buffer existing (codex-ide-input-mode 1))
          (pop-to-buffer existing)
          (when (and codex-ide-enable-mcp (not codex-ide--session-id))
            (message "This Codex session predates Emacs MCP; exit and reopen it to connect")))
      (unless (executable-find codex-ide-cli)
        (user-error "Codex CLI not found: %s (check PATH or codex-ide-cli)"
                    codex-ide-cli))
      (let* ((name (codex-ide--buffer-name root))
             (old (get-buffer name))
             (reuse (and old
                         (with-current-buffer old
                           (and (equal (codex-ide--buffer-root) root)
                                (not (and (bound-and-true-p ghostel--process)
                                          (process-live-p ghostel--process)))))))
             ;; Reopen a dead terminal for this project, but never steal a
             ;; same-named buffer owned by another project.
             (buffer (if reuse old (generate-new-buffer name)))
             id)
        (condition-case err
            (progn
              (when reuse
                (with-current-buffer buffer
                  (when codex-ide--session-id
                    (codex-ide--unregister-session codex-ide--session-id))))
              (when codex-ide-enable-mcp
                (setq id (codex-ide--register-session root buffer source)))
              (let* ((cli-args (append codex-ide-arguments
                                       (codex-ide--mcp-arguments id)))
                     (program (if (eq system-type 'windows-nt) "cmd.exe" codex-ide-cli))
                     (args (if (eq system-type 'windows-nt)
                               (append (list "/d" "/c" codex-ide-cli) cli-args)
                             cli-args))
                     (identity `((kind . exec) (project-root . ,root)
                                 (command . (,program . ,args)))))
                (with-current-buffer buffer (setq-local default-directory root))
                (ghostel-exec buffer program args identity)
                ;; Set after Ghostel switches modes and kills local variables.
                (with-current-buffer buffer
                  (setq-local codex-ide-project-root root
                              codex-ide--session-id id)
                  (codex-ide-input-mode 1)
                  (add-hook 'kill-buffer-hook #'codex-ide--buffer-killed nil t)
                  (when (and id (processp ghostel--process))
                    (let ((sentinel (process-sentinel ghostel--process)))
                      (set-process-sentinel
                       ghostel--process
                       (lambda (process event)
                         (unwind-protect
                             (when sentinel (funcall sentinel process event))
                           (unless (process-live-p process)
                             (codex-ide--unregister-session id))))))
                    ;; An immediately failing CLI may have exited before the
                    ;; sentinel was wrapped, so no future event will clean it up.
                    (unless (process-live-p ghostel--process)
                      (codex-ide--unregister-session id)))))
              (pop-to-buffer buffer))
          ((error quit)
           (when id (codex-ide--unregister-session id))
           (when (and (not reuse) (buffer-live-p buffer)) (kill-buffer buffer))
           (signal (car err) (cdr err))))))))

(defun codex-ide-send-region (beg end)
  "Paste the active region into the Codex session for this file's project."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((root (file-name-as-directory
                (expand-file-name (codex-ide--project-root (current-buffer)))))
         (buffer (codex-ide--session-buffer root))
         (text (buffer-substring-no-properties beg end)))
    (unless buffer
      (user-error "No running Codex session for this project; run `codex-ide' first"))
    (when (string-blank-p text)
      (user-error "Region is blank"))
    (deactivate-mark)
    (with-current-buffer buffer
      (ghostel-paste-string text)
      (sit-for 0.1)
      (ghostel-send-key "return"))))

(provide 'codex-ide)

;;; codex-ide.el ends here
