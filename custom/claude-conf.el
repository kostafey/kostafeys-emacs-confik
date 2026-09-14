;;; claude-conf.el

;; `web-server' is a hard dependency of claude-code-ide, but straight names
;; local repo directories after the repo basename alone, and two unrelated
;; projects share the name "emacs-web-server": eschulte's (this package) and
;; skeeto's (which is simple-httpd).  Whichever is cloned first wins the
;; directory, and here it was skeeto's, so straight built a web-server with
;; no web-server.el in it, `ws-start' stayed undefined and the MCP server
;; failed to start.  A distinct `:local-repo' keeps the two apart.
;;
;; No `:after' tie-up in the other direction: claude-code-ide requires
;; web-server itself when its HTTP server first loads, and this declaration is
;; only here to put the right one on `load-path'.
(use-package web-server
  :straight `(web-server
              :type git :host github
              :repo "eschulte/emacs-web-server"
              :local-repo "eschulte-emacs-web-server"))

(defun k/claude-code-ide--live-session-p (project-dir)
  "Non-nil when PROJECT-DIR has a Claude Code instance with a live buffer."
  (seq-some (lambda (session)
              (buffer-live-p (claude-code-ide-mcp-session-buffer session)))
            (claude-code-ide-mcp--sessions-for-project project-dir)))

;; `/exit' does not end the conversation any more: the CLI keeps it as a
;; background session, and it is still there after the terminal that ran it is
;; gone.  `claude agents --json' lists what is running -- around 150 ms, and
;; only on the path that is about to spawn a terminal anyway -- and
;; `claude attach ID' opens one of them here.
(defun k/claude-code-ide--background-session (project-dir)
  "Return the id of the newest CLI session running in PROJECT-DIR, or nil.
`--cwd' scopes the listing to PROJECT-DIR and below, so a session started
in a subdirectory of the project counts as well.  Runs through
`process-file', which puts the query on the same host as a TRAMP project."
  (with-temp-buffer
    (let ((default-directory project-dir))
      (when (eq 0 (process-file claude-code-ide-cli-path nil t nil
                                "agents" "--json" "--cwd"
                                (file-local-name (directory-file-name project-dir))))
        (goto-char (point-min))
        (when-let* ((sessions (ignore-errors
                                (json-parse-buffer :object-type 'alist
                                                   :array-type 'list))))
          (alist-get 'id
                     (car (sort sessions
                                (lambda (a b)
                                  (> (or (alist-get 'startedAt a) 0)
                                     (or (alist-get 'startedAt b) 0)))))))))))

(defvar k/claude-code-ide--attach-id nil
  "Background session id to attach to, bound around the spawn.")

(defun k/claude-code-ide--attach-command (build &rest args)
  "Return a `claude attach' command while attaching, else BUILD with ARGS.
None of what BUILD assembles applies to an attach: the session is already
running, carrying the MCP config and system prompt it was started with,
and `attach' is a subcommand that would reject those flags anyway."
  (if k/claude-code-ide--attach-id
      (concat claude-code-ide-cli-path " attach "
              (shell-quote-argument k/claude-code-ide--attach-id))
    (apply build args)))

(with-eval-after-load 'claude-code-ide
  (advice-add 'claude-code-ide--build-claude-command
              :around #'k/claude-code-ide--attach-command))

(defun k/claude-code-ide ()
  "Bring up this project's Claude Code, whatever state it is in.

An instance running in Emacs is switched to; a conversation the CLI still
has running in the background is attached to -- that is the one true
continuation, since the process never died -- and anything else starts a
fresh session.

Deliberately no `claude -c': continuing the last conversation of a
directory means resurrecting whatever was last discussed there, whole,
at a cost that grows with it -- the transcripts here run to megabytes.
`/resume' picks a conversation from inside a session instead, by name and
on purpose.

Instances are looked up in the registry by project root, so named and
numbered ones count too, not just the plain `*claude-code[PROJECT]*'
buffer.  With a prefix argument and several running,
`claude-code-ide-switch-to-buffer' asks which one to switch to."
  (interactive)
  ;; The keybinding fires before the package is loaded -- `:bind' autoloads
  ;; only the commands it binds, and everything below is internal.
  (require 'claude-code-ide)
  ;; A session outlives the terminal whose process has died; left in the
  ;; registry it would pass for a live one and send us to a dead buffer.
  (claude-code-ide--cleanup-dead-sessions)
  (let* ((project-dir (claude-code-ide--get-working-directory))
         (live (k/claude-code-ide--live-session-p project-dir))
         (background (unless live
                       (k/claude-code-ide--background-session project-dir))))
    (cond
     (live
      (call-interactively #'claude-code-ide-switch-to-buffer))
     (background
      (let ((k/claude-code-ide--attach-id background))
        (claude-code-ide)))
     (t
      (call-interactively #'claude-code-ide)))))

(use-package claude-code-ide
  :straight `(claude-code-ide
              :type git :host nil
              :repo "https://github.com/manzaltu/claude-code-ide.el"
              :branch "main")
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("C-M-a j" . k/claude-code-ide))
  :config
  ;; Of the three backends (vterm, eat, ghostel) this one renders the Claude
  ;; Code TUI with the fewest artifacts.  It wants the bundled xterm-ghostty
  ;; terminfo that `shell-conf' arranges for ghostel to ship.
  (setq claude-code-ide-terminal-backend 'ghostel)
  ;; The CLI draws its "In <file>" label inside the prompt row and takes the
  ;; columns for it out of the input field -- there is no layout in it that
  ;; would put the label on a line of its own, so the only way to type across
  ;; the full width is not to hand it a file.  A region still reaches Claude
  ;; while it is active, and a file can always be named in the prompt.
  (setq claude-code-ide-share-opened-file nil)
  ;; Exposes xref, imenu, project and diagnostics back to the agent, and flips
  ;; `claude-code-ide-enable-mcp-server' so the session claude-code-ide starts
  ;; brings that server up with it.
  (claude-code-ide-emacs-tools-setup))

;; The MCP tools server answers `initialize', `tools/list' and `tools/call',
;; and signals `json-rpc-error' for every other method -- a symbol the package
;; never passes to `define-error'.  Carrying no `error-conditions', it slips
;; through the `condition-case' in
;; `claude-code-ide-mcp-http-server--handle-post' and dies in the process
;; filter as "peculiar error: -32601, Method not found".  The noise in
;; *Messages* is the lesser half: the reply is never sent either, so whoever
;; asked waits for a response that will not come.  Defining the symbol hands
;; the handler back its own error path.
(define-error 'json-rpc-error "JSON-RPC error" 'error)

;; What trips it are the capability probes a client sends after `initialize'.
;; Answer them as a server holding neither resources nor prompts should, so
;; the common case does not have to travel the error path at all.
(defun k/claude-code-ide-mcp-dispatch-probes (dispatch method params)
  "Answer the MCP capability probes, leaving the rest to DISPATCH.
METHOD and PARAMS are passed through untouched."
  (pcase method
    ("resources/list" '((resources . [])))
    ("resources/templates/list" '((resourceTemplates . [])))
    ("prompts/list" '((prompts . [])))
    ;; An empty JSON object, which an empty alist would encode as `null'.
    ("ping" (make-hash-table :test 'equal))
    (_ (funcall dispatch method params))))

(with-eval-after-load 'claude-code-ide-mcp-http-server
  (advice-add 'claude-code-ide-mcp-http-server--dispatch
              :around #'k/claude-code-ide-mcp-dispatch-probes))

(provide 'claude-conf)
