;;; agent-ide.el --- Agent CLIs in Ghostel with Emacs MCP tools -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ghostel "0") (claude-code-ide "0"))
;; Keywords: tools, terminals

;;; Commentary:

;; The shared core of `codex-ide' and `pi-ide'.  It runs an agent CLI for
;; the current project in Ghostel and serves that terminal its own Emacs
;; MCP endpoint.  The HTTP transport, session registry, navigation tools
;; and diagnostics are reused from claude-code-ide; Claude clients retain
;; their existing behavior.
;;
;; Each CLI is an `agent-ide-client': how its command line receives the
;; endpoint, how its tools are named, how the explicit editor selection
;; reaches its prompts, and the input mode of its terminals.  Other
;; editor context and unsaved text are fetched only on request.

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

(defgroup agent-ide nil
  "Agent CLIs in Ghostel with Emacs MCP tools."
  :group 'tools :prefix "agent-ide-")

(defcustom agent-ide-buffer-read-max-lines 500
  "Maximum lines returned by one call to the buffer-reading tool."
  :type 'natnum)

(defface agent-ide-selection
  '((t :inherit link :underline nil))
  "Face for the selected-lines indicator above an agent terminal.")

(cl-defstruct (agent-ide-client (:constructor agent-ide-client-create)
                                (:copier nil))
  "An agent CLI run in Ghostel by `agent-ide-open'.
Slots naming variables hold their symbols, so customizations take effect
in sessions started afterwards."
  (name nil :documentation "Display name, such as \"Codex\".")
  (command nil :documentation "Command that opens the client's terminal.")
  (buffer-prefix nil :documentation "Terminals are named *PREFIX[project]*.")
  (id-prefix nil :documentation "Prefix of the client's MCP session IDs.")
  (cli-variable nil :documentation "Variable holding the executable.")
  (arguments-variable nil :documentation "Variable holding extra arguments.")
  (enable-mcp-variable nil :documentation "Variable enabling MCP.")
  (include-selection-variable
   nil :documentation "Variable enabling the prompt selection.")
  (root-variables
   nil :documentation "Buffer-local variables holding a terminal's project
root.  New terminals set the first one.")
  (input-mode nil :documentation "Minor mode enabled in the terminals.")
  (mcp-arguments
   nil :documentation "Function of the endpoint URL returning the CLI
arguments that connect the client to it.")
  (tool-prefix "" :documentation "Prefix of the client's tool names.")
  (prompt-context
   nil :documentation "Function of the editor buffer, its selection and
the session context returning the prompt context for the selection.")
  (prompt-context-description
   nil :documentation "Description of the prompt context tool.")
  (server-name nil :documentation "MCP server name sent on initialize.")
  (instructions nil :documentation "MCP server instructions.")
  (tools nil :documentation "Cached tool specifications."))

(defvar agent-ide--clients nil
  "Clients defined with `agent-ide-define-client'.")

(defvar agent-ide--sessions (make-hash-table :test 'equal)
  "Map MCP session IDs to their terminal buffers.")

(defvar-local agent-ide--session-id nil
  "MCP session ID associated with this terminal.")

(defvar-local agent-ide--selection-state nil
  "Editor buffer, region bounds and modification tick shown in the header.")

(defvar-local agent-ide--saved-header-line nil
  "Saved (LOCAL-P . FORMAT) while the selection indicator is displayed.")

(defvar ghostel--process)
(defvar ghostel-identity)
(declare-function ghostel-exec "ghostel"
                  (buffer program &optional args identity))
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))

(unless (get 'json-rpc-error 'error-conditions)
  (define-error 'json-rpc-error "JSON-RPC error" 'error))

(defun agent-ide-define-client (client)
  "Register CLIENT, replacing a client with the same name, and return it."
  (setq agent-ide--clients
        (cons client
              (seq-remove (lambda (other)
                            (equal (agent-ide-client-name other)
                                   (agent-ide-client-name client)))
                          agent-ide--clients)))
  client)

(defun agent-ide--id-client (id)
  "Return the client owning MCP session ID, or nil."
  (and (stringp id)
       (seq-find (lambda (client)
                   (string-prefix-p (agent-ide-client-id-prefix client) id))
                 agent-ide--clients)))

(defun agent-ide--value (client slot)
  "Return the value of the variable that CLIENT names in SLOT."
  (symbol-value (cl-struct-slot-value 'agent-ide-client slot client)))

(defun agent-ide--project-root (&optional buffer)
  "Return the project root for BUFFER, or its `default-directory'."
  (with-current-buffer (or buffer (current-buffer))
    (if-let* ((project (project-current nil)))
        (project-root project)
      default-directory)))

(defun agent-ide--buffer-name (client root)
  "Return CLIENT's terminal buffer name for project ROOT."
  (format "*%s[%s]*" (agent-ide-client-buffer-prefix client)
          (file-name-nondirectory (directory-file-name root))))

(defun agent-ide--buffer-root (client)
  "Return the project root of the current buffer as CLIENT's terminal.
Falls back to the root stored in `ghostel-identity', which survives
the switch to `ghostel-mode'."
  (or (seq-some (lambda (variable)
                  (and (boundp variable) (symbol-value variable)))
                (agent-ide-client-root-variables client))
      (and (boundp 'ghostel-identity)
           (listp ghostel-identity)
           (string-prefix-p
            (format "*%s[" (agent-ide-client-buffer-prefix client))
            (buffer-name))
           (alist-get 'project-root ghostel-identity))))

(defun agent-ide--live-terminal-p ()
  "Whether the current buffer runs a live Ghostel process."
  (and (bound-and-true-p ghostel--process)
       (process-live-p ghostel--process)))

(defun agent-ide--session-buffer (client root)
  "Return CLIENT's live terminal associated with project ROOT, if any."
  (seq-some
   (lambda (buffer)
     (with-current-buffer buffer
       (let ((buffer-root (agent-ide--buffer-root client)))
         (and (stringp buffer-root)
              (or (equal buffer-root root)
                  (condition-case nil
                      (file-equal-p buffer-root root)
                    (file-error nil)))
              (agent-ide--live-terminal-p)
              buffer))))
   (buffer-list)))

;;; Editor tools

(defun agent-ide--editor-buffer-p (buffer root)
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

(defun agent-ide--context ()
  "Return the current MCP request's registered session context."
  (or (claude-code-ide-mcp-server-get-session-context)
      (error "No Emacs session context for this agent client")))

(defun agent-ide--editor-buffer (context)
  "Return the last live editor buffer for CONTEXT, if any."
  (let ((root (plist-get context :project-dir))
        (last (plist-get context :last-active-buffer)))
    (if (agent-ide--editor-buffer-p last root)
        last
      (seq-find (lambda (buffer) (agent-ide--editor-buffer-p buffer root))
                (buffer-list)))))

(defun agent-ide--set-selection-header (text)
  "Show TEXT in this terminal's header, or restore its header when nil."
  (if text
      (progn
        (unless agent-ide--saved-header-line
          (setq agent-ide--saved-header-line
                (cons (local-variable-p 'header-line-format)
                      header-line-format)))
        (setq-local header-line-format
                    (list (propertize text 'face 'agent-ide-selection)
                          (when (cdr agent-ide--saved-header-line) "  ")
                          (cdr agent-ide--saved-header-line))))
    (when agent-ide--saved-header-line
      (if (car agent-ide--saved-header-line)
          (setq-local header-line-format (cdr agent-ide--saved-header-line))
        (kill-local-variable 'header-line-format))
      (setq agent-ide--saved-header-line nil)))
  (force-mode-line-update))

(defun agent-ide--update-selection-header (client context)
  "Update this terminal's indicator from CONTEXT's explicit selection.
CLIENT is the agent running in the terminal.  Reuse the count until the
region or buffer text changes.  This runs from editor commands,
independently of terminal redraws, with no timer."
  (let* ((source (and (symbol-value (agent-ide-client-input-mode client))
                      (agent-ide--value client 'include-selection-variable)
                      context
                      (agent-ide--editor-buffer context)))
         (state (and source
                     (with-current-buffer source
                       (when (use-region-p)
                         (list source (region-beginning) (region-end)
                               (buffer-chars-modified-tick)))))))
    (unless (equal state agent-ide--selection-state)
      (setq agent-ide--selection-state state)
      (agent-ide--set-selection-header
       (when state
         (let ((lines (with-current-buffer source
                        (save-restriction
                          (widen)
                          (count-lines (nth 1 state) (nth 2 state))))))
           (format "  [%d %s selected]"
                   lines (if (= lines 1) "line" "lines"))))))))

(defun agent-ide-input-mode-changed (client)
  "Show or remove the selection indicator after CLIENT's input mode toggles.
Call this from the body of the client's input mode."
  (if (symbol-value (agent-ide-client-input-mode client))
      (agent-ide--update-selection-header
       client (and agent-ide--session-id
                   (claude-code-ide-mcp-server-get-session-context
                    agent-ide--session-id)))
    (setq agent-ide--selection-state nil)
    (agent-ide--set-selection-header nil)))

(defun agent-ide--track-editor-context ()
  "Remember the active editor buffer for matching agent projects."
  (let ((buffer (current-buffer)))
    (maphash
     (lambda (id terminal)
       (let ((context (claude-code-ide-mcp-server-get-session-context id))
             (client (agent-ide--id-client id)))
         (when (and context (agent-ide--editor-buffer-p
                             buffer (plist-get context :project-dir)))
           (claude-code-ide-mcp-server-update-last-active-buffer id buffer))
         (when (and context client (buffer-live-p terminal))
           (with-current-buffer terminal
             (agent-ide--update-selection-header
              client (claude-code-ide-mcp-server-get-session-context id))))))
     agent-ide--sessions)))

(defun agent-ide--buffer-info (buffer)
  "Return BUFFER metadata without changing point or narrowing."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      `((buffer_name . ,(buffer-name)) (file_path . ,buffer-file-name)
        (major_mode . ,(symbol-name major-mode))
        (modified . ,(if (buffer-modified-p) t :json-false))
        (point . ((line . ,(line-number-at-pos))
                  (column . ,(current-column))))))))

(defun agent-ide--resolve-buffer (&optional file-path buffer-name)
  "Resolve an open project buffer by FILE-PATH or BUFFER-NAME.
Without either argument, use the session's last editor buffer."
  (when (and file-path buffer-name)
    (error "Specify file_path or buffer_name, not both"))
  (let* ((context (agent-ide--context))
         (root (plist-get context :project-dir))
         (buffer (cond (file-path
                        (find-buffer-visiting
                         (expand-file-name file-path root)))
                       (buffer-name (get-buffer buffer-name))
                       (t (agent-ide--editor-buffer context)))))
    (unless (agent-ide--editor-buffer-p buffer root)
      (error "No matching open editor buffer in project %s" root))
    buffer))

(defun agent-ide--position (position)
  "Return the one-based line and zero-based column at POSITION."
  (save-excursion
    (goto-char position)
    `((line . ,(line-number-at-pos)) (column . ,(current-column)))))

(defun agent-ide--selection (buffer)
  "Return BUFFER's active selection and its location, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (use-region-p)
        (let ((beg (region-beginning)) (end (region-end)))
          (save-restriction
            (widen)
            `((text . ,(buffer-substring-no-properties beg end))
              (start . ,(agent-ide--position beg))
              (end . ,(agent-ide--position end)))))))))

(defun agent-ide--get-editor-context ()
  "Return the project's last editor buffer, cursor and active selection."
  (let* ((context (agent-ide--context))
         (buffer (agent-ide--editor-buffer context))
         (selection (agent-ide--selection buffer)))
    (json-encode
     `((project_root . ,(plist-get context :project-dir))
       (active_buffer . ,(and buffer (agent-ide--buffer-info buffer)))
       (selection . ,selection)))))

(defun agent-ide--get-prompt-context (client)
  "Return CLIENT's prompt context for an explicit editor selection only.
Without a selection, or with the selection disabled for CLIENT, return
an empty JSON object; never attach a bare file or cursor."
  (let* ((context (agent-ide--context))
         (buffer (agent-ide--editor-buffer context))
         (selection (and (agent-ide--value client 'include-selection-variable)
                         (agent-ide--selection buffer))))
    (json-encode
     (if selection
         (funcall (agent-ide-client-prompt-context client)
                  buffer selection context)
       (make-hash-table :test 'equal)))))

(defun agent-ide--read-buffer (&optional file-path buffer-name
                                         start-line end-line)
  "Read unsaved buffer text between START-LINE and END-LINE.
Line numbers are one-based; results are paginated without changing the
buffer.  FILE-PATH or BUFFER-NAME selects the buffer."
  (let ((buffer (agent-ide--resolve-buffer file-path buffer-name)))
    (with-current-buffer buffer
      (let ((info (agent-ide--buffer-info buffer)))
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
                              (+ start (max 1 agent-ide-buffer-read-max-lines)
                                 -1))))
                (goto-char (point-min))
                (forward-line (1- start))
                (let* ((beg (point))
                       (finish (progn (forward-line (1+ (- end start)))
                                      (point))))
                  (json-encode
                   (append info
                           `((start_line . ,start) (end_line . ,end)
                             (total_lines . ,total)
                             (has_more . ,(if (< end total) t :json-false))
                             (content . ,(buffer-substring-no-properties
                                          beg finish))))))))))))))

(defun agent-ide--list-buffers ()
  "Return metadata for open editor buffers in this request's project."
  (let ((root (plist-get (agent-ide--context) :project-dir)))
    (json-encode
     (vconcat (mapcar #'agent-ide--buffer-info
                      (seq-filter (lambda (buffer)
                                    (agent-ide--editor-buffer-p buffer root))
                                  (buffer-list)))))))

(defun agent-ide--get-diagnostics (&optional file-path)
  "Get current Flymake/Flycheck diagnostics, optionally for FILE-PATH."
  (let* ((root (plist-get (agent-ide--context) :project-dir))
         (buffers (if file-path
                      (list (agent-ide--resolve-buffer file-path))
                    (seq-filter
                     (lambda (buffer)
                       (and (buffer-file-name buffer)
                            (agent-ide--editor-buffer-p buffer root)))
                     (buffer-list)))))
    (json-encode
     (vconcat (mapcar
               (lambda (buffer)
                 `((file_path . ,(buffer-file-name buffer))
                   (diagnostics
                    . ,(claude-code-ide-diagnostics-get-all buffer))))
               buffers)))))

(defun agent-ide--xref-definitions (identifier file-path)
  "Find IDENTIFIER definitions with FILE-PATH's configured xref backend."
  (let* ((root (plist-get (agent-ide--context) :project-dir))
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
                                (agent-ide--position marker)
                                `((summary . ,(xref-item-summary item)))))))
                  (xref-backend-definitions backend identifier))))))))

;;; MCP adapter

(defun agent-ide--tools (client)
  "Return CLIENT's tool list, built with claude-code-ide's registration API."
  (or (agent-ide-client-tools client)
      (let ((claude-code-ide-mcp-server-tools nil)
            (claude-code-ide-enable-mcp-server t)
            (prefix (agent-ide-client-tool-prefix client)))
        (claude-code-ide-emacs-tools-setup)
        (dolist (spec claude-code-ide-mcp-server-tools)
          (plist-put spec :name
                     (concat prefix
                             (replace-regexp-in-string
                              "-" "_" (string-remove-prefix
                                       "claude-code-ide-mcp-"
                                       (plist-get spec :name))))))
        (dolist (spec
                 `((:name "get_editor_context"
                    :function agent-ide--get-editor-context
                    :description
                    ,(concat "Get this project's last editor buffer, cursor "
                             "and active selection. Lines are 1-based and "
                             "columns 0-based. Unselected file contents "
                             "are not sent automatically."))
                   (:name "get_prompt_context"
                    :function ,(lambda ()
                                 (agent-ide--get-prompt-context client))
                    :description
                    ,(agent-ide-client-prompt-context-description client))
                   (:name "list_buffers" :function agent-ide--list-buffers
                    :description
                    ,(concat "List open editor buffers in this project, "
                             "including unsaved-change flags."))
                   (:name "read_buffer" :function agent-ide--read-buffer
                    :description
                    ,(concat "Read live, possibly unsaved text from an open "
                             "project buffer. Choose file_path OR "
                             "buffer_name, or omit both for the last editor "
                             "buffer. Lines are 1-based; follow has_more "
                             "to read further pages.")
                    :args ((:name "file_path" :type string :optional t)
                           (:name "buffer_name" :type string :optional t)
                           (:name "start_line" :type integer :optional t)
                           (:name "end_line" :type integer :optional t)))
                   (:name "get_diagnostics"
                    :function agent-ide--get-diagnostics
                    :description
                    ,(concat "Get current Flymake/Flycheck diagnostics for "
                             "open project files, optionally limited to "
                             "file_path. Uses claude-code-ide's diagnostic "
                             "format.")
                    :args ((:name "file_path" :type string :optional t)))
                   (:name "xref_definitions"
                    :function agent-ide--xref-definitions
                    :description
                    ,(concat "Find symbol definitions with the Emacs "
                             "xref/LSP backend configured for file_path.")
                    :args ((:name "identifier" :type string)
                           (:name "file_path" :type string)))))
          (apply #'claude-code-ide-make-tool
                 (plist-put (copy-sequence spec) :name
                            (concat prefix (plist-get spec :name)))))
        (setf (agent-ide-client-tools client)
              claude-code-ide-mcp-server-tools))))

(defun agent-ide--dispatch (dispatch method params)
  "Adapt MCP METHOD and PARAMS for agent sessions; otherwise call DISPATCH."
  (let* ((id claude-code-ide-mcp-server--current-session-id)
         (client (agent-ide--id-client id)))
    (if (not client)
        (funcall dispatch method params)
      (unless (and (gethash id agent-ide--sessions)
                   (claude-code-ide-mcp-server-get-session-context id))
        (error "Unknown or closed %s IDE session"
               (agent-ide-client-name client)))
      (let ((claude-code-ide-mcp-server-tools (agent-ide--tools client)))
        (pcase method
          ("initialize"
           `((protocolVersion
              . ,(let ((version (alist-get 'protocolVersion params)))
                   (if (member version '("2025-11-25" "2025-06-18"
                                         "2025-03-26"))
                       version "2025-03-26")))
             (capabilities . ((tools . ((listChanged . :json-false)))))
             (serverInfo . ((name . ,(agent-ide-client-server-name client))
                            (version . "0.1.0")))
             (instructions . ,(agent-ide-client-instructions client))))
          ((or "resources/list" "resources/templates/list" "prompts/list")
           (list (cons (pcase method
                         ("resources/list" 'resources)
                         ("resources/templates/list" 'resourceTemplates)
                         (_ 'prompts))
                       [])))
          ("ping" (make-hash-table :test 'equal))
          ("tools/call"
           ;; The reused tools accept file paths, but may switch into an
           ;; editor buffer in a subdirectory.  Resolve relative paths first.
           (let* ((params (copy-tree params))
                  (args (alist-get 'arguments params))
                  (file (alist-get 'file_path args)))
             (when (stringp file)
               (setf (alist-get 'file_path args)
                     (expand-file-name
                      file (plist-get (agent-ide--context) :project-dir))))
             (funcall dispatch method params)))
          (_ (funcall dispatch method params)))))))

(advice-add 'claude-code-ide-mcp-http-server--dispatch
            :around #'agent-ide--dispatch)

(defun agent-ide--notification-response (send request)
  "Use HTTP 202 for agent notifications, otherwise call SEND for REQUEST.
Streamable HTTP acknowledges a notification with 202 and no body; the
native Codex MCP client rejects the upstream HTTP 200 text response."
  (let ((id (claude-code-ide-mcp-http-server--extract-session-id-from-path
             (ws-headers request))))
    (if (not (gethash id agent-ide--sessions))
        (funcall send request)
      (ws-response-header (ws-process request) 202 '("Content-Length" . "0"))
      (throw 'close-connection nil))))

(advice-add 'claude-code-ide-mcp-http-server--send-empty-response
            :around #'agent-ide--notification-response)

(defun agent-ide--complete-http-request (parse request)
  "Wait for the whole agent HTTP body after PARSE has read REQUEST headers.
web-server can finish parsing headers before a separately delivered JSON
body arrives.  Do not dispatch a partial request or close its connection."
  (let* ((complete (funcall parse request))
         (headers (ws-headers request))
         (id (claude-code-ide-mcp-http-server--extract-session-id-from-path
              headers))
         (length-header (cdr (assq :CONTENT-LENGTH headers))))
    (if (and complete (agent-ide--id-client id)
             length-header
             (< (string-bytes (ws-body request))
                (string-to-number length-header)))
        (progn (setf (ws-active request) nil) nil)
      complete)))

(advice-add 'ws-parse-request :around #'agent-ide--complete-http-request)

(defun agent-ide--register-session (client root buffer source)
  "Register CLIENT's BUFFER at ROOT, remembering editor SOURCE.
Return the new MCP session ID."
  (when (file-remote-p root)
    (user-error "%s IDE MCP currently requires a local project"
                (agent-ide-client-name client)))
  (agent-ide--tools client)
  (unless (let ((claude-code-ide-enable-mcp-server t))
            (claude-code-ide-mcp-server-ensure-server))
    (error "Could not start the Emacs MCP tools server"))
  (let ((id (concat (agent-ide-client-id-prefix client)
                    (substring (secure-hash 'sha256
                                            (format "%s:%s:%s" (emacs-pid)
                                                    (current-time) (random)))
                               0 24))))
    (claude-code-ide-mcp-server-session-started id root buffer)
    (puthash id buffer agent-ide--sessions)
    (when (agent-ide--editor-buffer-p source root)
      (claude-code-ide-mcp-server-update-last-active-buffer id source))
    (add-hook 'post-command-hook #'agent-ide--track-editor-context)
    id))

(defun agent-ide--unregister-session (id)
  "Unregister ID without disturbing other agent or Claude clients."
  (when-let* ((buffer (gethash id agent-ide--sessions)))
    (remhash id agent-ide--sessions)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (equal agent-ide--session-id id)
          (setq agent-ide--session-id nil
                agent-ide--selection-state nil)
          (agent-ide--set-selection-header nil))))
    (claude-code-ide-mcp-server-session-ended id)
    (when (zerop (hash-table-count agent-ide--sessions))
      (remove-hook 'post-command-hook #'agent-ide--track-editor-context))))

(defun agent-ide--buffer-killed ()
  "Unregister the MCP endpoint owned by the current buffer."
  (when agent-ide--session-id
    (agent-ide--unregister-session agent-ide--session-id)))

(defun agent-ide--stop-process (process)
  "Terminate PROCESS and its children before Emacs exits.
Native Ghostel processes are represented by pipes.  Killing only their
native PID can leave the actual CLI behind its launcher, such as the
Codex binary behind npm or Pi's Node process behind its shell script."
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

(defun agent-ide--cleanup-on-exit ()
  "Stop agent terminals, including sessions started without MCP."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when-let* ((client (seq-find #'agent-ide--buffer-root
                                      agent-ide--clients)))
          (let ((id agent-ide--session-id)
                (process (bound-and-true-p ghostel--process)))
            (unwind-protect
                (condition-case err
                    (agent-ide--stop-process process)
                  (error (message "Could not stop %s: %s"
                                  (agent-ide-client-name client)
                                  (error-message-string err))))
              (when id (agent-ide--unregister-session id)))))))))

;; Register after Ghostel, so this runs before its native-PID-only kill hook.
(with-eval-after-load 'ghostel
  (add-hook 'kill-emacs-hook #'agent-ide--cleanup-on-exit))

(defun agent-ide--mcp-url (id)
  "Return the Emacs MCP endpoint URL of session ID."
  (format "http://127.0.0.1:%d/mcp/%s"
          (claude-code-ide-mcp-server-get-port) id))

;;; Terminal commands

(defun agent-ide-open (client)
  "Open or switch to the current project's CLIENT terminal."
  (require 'ghostel)
  (let* ((source (current-buffer))
         (name (agent-ide-client-name client))
         (cli (agent-ide--value client 'cli-variable))
         (mode (agent-ide-client-input-mode client))
         (root (file-name-as-directory
                (expand-file-name (agent-ide--project-root))))
         (existing (agent-ide--session-buffer client root)))
    (if existing
        (progn
          (with-current-buffer existing (funcall mode 1))
          (pop-to-buffer existing)
          (when (and (agent-ide--value client 'enable-mcp-variable)
                     (not (buffer-local-value 'agent-ide--session-id
                                              existing)))
            (message (concat "This %s session runs without Emacs MCP; "
                             "exit and reopen it to connect")
                     name)))
      (unless (executable-find cli)
        (user-error "%s not found: %s (check PATH or %s)"
                    name cli (agent-ide-client-cli-variable client)))
      (let* ((buffer-name (agent-ide--buffer-name client root))
             (old (get-buffer buffer-name))
             (reuse (and old
                         (with-current-buffer old
                           (and (equal (agent-ide--buffer-root client) root)
                                (not (agent-ide--live-terminal-p))))))
             ;; Reopen a dead terminal for this project, but never steal a
             ;; same-named buffer owned by another project.
             (buffer (if reuse old (generate-new-buffer buffer-name)))
             id)
        (condition-case err
            (progn
              (when reuse
                (with-current-buffer buffer
                  (when agent-ide--session-id
                    (agent-ide--unregister-session agent-ide--session-id))))
              (when (agent-ide--value client 'enable-mcp-variable)
                (setq id (agent-ide--register-session
                          client root buffer source)))
              (let* ((cli-args
                      (append (agent-ide--value client 'arguments-variable)
                              (and id (funcall
                                       (agent-ide-client-mcp-arguments client)
                                       (agent-ide--mcp-url id)))))
                     (program (if (eq system-type 'windows-nt) "cmd.exe" cli))
                     (args (if (eq system-type 'windows-nt)
                               (append (list "/d" "/c" cli) cli-args)
                             cli-args))
                     (identity `((kind . exec) (project-root . ,root)
                                 (command . (,program . ,args)))))
                (with-current-buffer buffer
                  (setq-local default-directory root))
                (ghostel-exec buffer program args identity)
                ;; Set after Ghostel switches modes and kills local variables.
                (with-current-buffer buffer
                  (set (make-local-variable
                        (car (agent-ide-client-root-variables client)))
                       root)
                  (setq-local agent-ide--session-id id)
                  (funcall mode 1)
                  (add-hook 'kill-buffer-hook #'agent-ide--buffer-killed nil t)
                  (when (and id (processp ghostel--process))
                    (let ((sentinel (process-sentinel ghostel--process)))
                      (set-process-sentinel
                       ghostel--process
                       (lambda (process event)
                         (unwind-protect
                             (when sentinel (funcall sentinel process event))
                           (unless (process-live-p process)
                             (agent-ide--unregister-session id))))))
                    ;; An immediately failing CLI may have exited before the
                    ;; sentinel was wrapped, so no future event will clean
                    ;; it up.
                    (unless (process-live-p ghostel--process)
                      (agent-ide--unregister-session id)))))
              (pop-to-buffer buffer))
          ((error quit)
           (when id (agent-ide--unregister-session id))
           (when (and (not reuse) (buffer-live-p buffer)) (kill-buffer buffer))
           (signal (car err) (cdr err))))))))

(defun agent-ide-send-region (client beg end)
  "Paste the region between BEG and END into this project's CLIENT session."
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((root (file-name-as-directory
                (expand-file-name (agent-ide--project-root (current-buffer)))))
         (buffer (agent-ide--session-buffer client root))
         (text (buffer-substring-no-properties beg end)))
    (unless buffer
      (user-error "No running %s session for this project; run `%s' first"
                  (agent-ide-client-name client)
                  (agent-ide-client-command client)))
    (when (string-blank-p text)
      (user-error "Region is blank"))
    (deactivate-mark)
    (with-current-buffer buffer
      (ghostel-paste-string text)
      (sit-for 0.1)
      (ghostel-send-key "return"))))

(provide 'agent-ide)

;;; agent-ide.el ends here
