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

;; `claude-code-ide-continue' always starts a *new* instance -- a second one
;; forks the same conversation into a second terminal -- so it is only the
;; right entry point while this project has no session at all.  Once one is
;; running, the same key should just take us back to it.
(defun k/claude-code-ide ()
  "Go to this project's Claude Code buffer, or continue its last conversation.
Sessions are looked up by project root, so named and numbered instances
count too, not just the plain `*claude-code[PROJECT]*' buffer.  With a
prefix argument and several instances running,
`claude-code-ide-switch-to-buffer' asks which one to switch to."
  (interactive)
  ;; The keybinding fires before the package is loaded -- `:bind' autoloads
  ;; only the commands it binds, and everything below is internal.
  (require 'claude-code-ide)
  ;; A session outlives the terminal whose process has died; left in the
  ;; registry it would pass for a live one and send us to a dead buffer.
  (claude-code-ide--cleanup-dead-sessions)
  (if (seq-some (lambda (session)
                  (buffer-live-p (claude-code-ide-mcp-session-buffer session)))
                (claude-code-ide-mcp--sessions-for-project
                 (claude-code-ide--get-working-directory)))
      (call-interactively #'claude-code-ide-switch-to-buffer)
    (call-interactively #'claude-code-ide-continue)))

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
  ;; Exposes xref, imenu, project and diagnostics back to the agent, and flips
  ;; `claude-code-ide-enable-mcp-server' so the session claude-code-ide starts
  ;; brings that server up with it.
  (claude-code-ide-emacs-tools-setup))

(provide 'claude-conf)
