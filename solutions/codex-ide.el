;;; codex-ide.el --- Codex in Ghostel with Emacs MCP tools -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ghostel "0") (claude-code-ide "0"))
;; Keywords: tools, terminals

;;; Commentary:

;; `codex-ide' opens this project's Codex CLI and supplies its Emacs MCP
;; endpoint through command-line configuration, without editing config.toml.
;; Terminals, the MCP endpoint and the editor tools come from `agent-ide'.
;; Explicit editor selections accompany prompts through UserPromptSubmit.
;; Other editor context and unsaved text are fetched only on request.
;; This package does not implement IDE WebSocket notifications or ediff.

;;; Code:

(require 'agent-ide)

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

(defface codex-ide-working
  '((t :inherit success :weight bold))
  "Face for the active Working label above the Codex composer."
  :group 'codex-ide)

(defvar-local codex-ide-project-root nil
  "Project root associated with this Codex buffer.")

(defvar-local codex-ide--working-overlay nil
  "Overlay keeping the active Working label legible during CLI shimmer.")

(defvar codex-ide-input-mode)
(defvar ghostel--term-rows)
(declare-function ghostel-paste-string "ghostel" (string))

(defun codex-ide--prompt-context (buffer selection _context)
  "Return UserPromptSubmit output for SELECTION in editor BUFFER."
  `((hookSpecificOutput
     . ((hookEventName . "UserPromptSubmit")
        (additionalContext
         . ,(concat
             "The user explicitly selected this text in Emacs for the "
             "current prompt. Treat it as source context. Lines are "
             "1-based and columns 0-based.\n"
             (json-encode
              (append (with-current-buffer buffer
                        `((file_path . ,buffer-file-name)
                          (buffer_name . ,(buffer-name))))
                      selection))))))))

(defun codex-ide--mcp-arguments (url)
  "Return CLI configuration overrides connecting Codex to URL."
  (append
   (list "-c" (concat "mcp_servers.emacs_ide.url=" (json-encode-string url))
         "-c" "mcp_servers.emacs_ide.enabled=true"
         "-c" "mcp_servers.emacs_ide.required=true")
   ;; A stable definition needs trust review only once, even for new sessions.
   (when codex-ide-include-selection
     (list "-c"
           (concat "hooks.UserPromptSubmit=[{hooks=[{type=\"mcp_tool\","
                   "server=\"emacs_ide\",tool=\"emacs_get_prompt_context\","
                   "timeout=5}]}]")))))

(defvar codex-ide--client nil
  "The Codex client of `agent-ide'.")

;; Redefined on every load, so reloading the file updates the client.
(setq codex-ide--client
      (agent-ide-define-client
       (agent-ide-client-create
        :name "Codex"
        :command 'codex-ide
        :buffer-prefix "codex"
        :id-prefix "codex-ide-"
        :cli-variable 'codex-ide-cli
        :arguments-variable 'codex-ide-arguments
        :enable-mcp-variable 'codex-ide-enable-mcp
        :include-selection-variable 'codex-ide-include-selection
        ;; `k/codex-project-root' marks terminals of the personal command
        ;; that preceded this package.
        :root-variables '(codex-ide-project-root k/codex-project-root)
        :input-mode 'codex-ide-input-mode
        :mcp-arguments #'codex-ide--mcp-arguments
        :tool-prefix "emacs_"
        :prompt-context #'codex-ide--prompt-context
        :prompt-context-description
        (concat "UserPromptSubmit hook: attach only the explicit editor "
                "selection and its location. Returns an empty object when "
                "there is no selection; never attaches a bare file or cursor.")
        :server-name "codex-ide"
        :instructions
        (concat "Emacs tools operate in this Codex session's project. "
                "Explicit editor selections are attached by the "
                "UserPromptSubmit hook when enabled and trusted. Use "
                "emacs_get_editor_context for editor context on request. Use "
                "emacs_read_buffer for unsaved edits; filesystem tools see "
                "only saved files. Unselected files are never attached "
                "automatically."))))

;;; Terminal commands

(defun codex-ide--highlight-working ()
  "Highlight the active Working status without changing terminal text."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((limit (save-excursion
                     (forward-line
                      (- (or (bound-and-true-p ghostel--term-rows) 60)))
                     (point))))
        (if (and codex-ide-input-mode
                 (re-search-backward
                  (concat "^[ \t]*\\(?:[^[:alnum:][:space:]]+[ \t]+\\)?"
                          "\\(Working\\)[ \t]+([0-9]")
                  limit t))
            (progn
              (unless (and (overlayp codex-ide--working-overlay)
                           (overlay-buffer codex-ide--working-overlay))
                (setq codex-ide--working-overlay
                      (make-overlay (point) (point))))
              (move-overlay codex-ide--working-overlay
                            (match-beginning 1) (match-end 1))
              (overlay-put codex-ide--working-overlay
                           'face 'codex-ide-working))
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
  (codex-ide--highlight-working)
  (agent-ide-input-mode-changed codex-ide--client))

(defun codex-ide ()
  "Open or switch to this project's Codex CLI with Emacs MCP tools."
  (interactive)
  (agent-ide-open codex-ide--client))

(defun codex-ide-send-region (beg end)
  "Paste the region between BEG and END into this project's Codex session."
  (interactive "r")
  (agent-ide-send-region codex-ide--client beg end))

(defun codex-ide-session-buffer ()
  "Return the live Codex terminal of the current buffer's project, or nil."
  (agent-ide-session-buffer codex-ide--client))

(defun codex-ide-send-string (text)
  "Submit TEXT as a prompt to this project's Codex session."
  (agent-ide-send-string codex-ide--client text))

(provide 'codex-ide)

;;; codex-ide.el ends here
