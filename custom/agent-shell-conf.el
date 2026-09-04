;;; agent-shell-conf.el

;; Install acp for `calude' to use it in the agent-shell:
;; npm install -g @agentclientprotocol/claude-agent-acp

(require 'pixel-scroll)
(require 'mwheel)

;; `agent-shell' dependencies: the underlying shell interface and the
;; Agent Client Protocol layer (neither is available in ELPA).
(use-package shell-maker
  :straight `(shell-maker
              :type git :host github
              :repo "xenodium/shell-maker"
              :branch "main"))

(use-package acp
  :straight `(acp
              :type git :host github
              :repo "xenodium/acp.el"
              :branch "main"))

;; `comint-mode-map' (inherited via `shell-maker-mode-map') takes C-<arrow>
;; for input history, shadowing the global line scrolling from `basic-keys'.
;; Keep both: history while composing at the prompt, scrolling in the
;; read-only transcript above it.  `shell-maker-point-at-last-prompt-p' is
;; what agent-shell itself uses to tell the two regions apart.
;;
;; The scrolling half has to go by pixels rather than by lines.  agent-shell
;; draws prompts and separators as overlays whose `before-string' spans
;; several screen lines, and those lines carry no buffer position, so
;; line-based `scroll-up' cannot put `window-start' inside one: a one- or
;; two-line scroll collapses back to where it started and only a step large
;; enough to clear the whole overlay moves the view.  Pixel scrolling
;; vscrolls through such an element instead.  Mind the inverted naming in
;; pixel-scroll.el: its "up" goes towards the beginning of the buffer.
(defvar-local k/agent-shell--pixel-scrolled nil
  "Non-nil when the command now finishing scrolled this buffer by pixels.
Read and cleared by `k/agent-shell--tolerate-partial-line'.")

(defun k/agent-shell-pixel-scroll (direction pixels)
  "Scroll PIXELS towards the buffer's beginning if DIRECTION is `up', else end.
Records the scroll so the redisplay that follows tolerates a partially
visible line -- see `k/agent-shell--tolerate-partial-line'."
  (setq k/agent-shell--pixel-scrolled t)
  (if (eq direction 'up)
      (pixel-scroll-precision-scroll-up pixels)
    (pixel-scroll-precision-scroll-down pixels)))

(defun k/agent-shell-previous-input-or-scroll-down ()
  "Go to previous input at the prompt, scroll one line back elsewhere."
  (interactive)
  (if (shell-maker-point-at-last-prompt-p)
      (call-interactively #'comint-previous-input)
    (k/agent-shell-pixel-scroll 'up (default-line-height))))

(defun k/agent-shell-next-input-or-scroll-up ()
  "Go to next input at the prompt, scroll one line forward elsewhere."
  (interactive)
  (if (shell-maker-point-at-last-prompt-p)
      (call-interactively #'comint-next-input)
    (k/agent-shell-pixel-scroll 'down (default-line-height))))

;; The mouse wheel walks into the same wall.  `mouse-wheel-scroll-amount' is 1
;; and `mouse-wheel-progressive-speed' is nil, so every click is exactly
;; `(scroll-up 1)' — the one step that cannot cross such an overlay, and never
;; a larger one that would get past it by luck.  Route the wheel through pixel
;; scrolling as well, keeping the configured lines per click.  Only the bare
;; events are rebound; modified ones (text scaling, hscroll) stay with mwheel.
(defun k/agent-shell-wheel-lines ()
  "Lines per wheel click, following `mouse-wheel-scroll-amount'."
  (let ((amount (car mouse-wheel-scroll-amount)))
    (if (numberp amount) amount 1)))

(defun k/agent-shell-wheel-scroll-up (event)
  "Scroll the window under EVENT one step towards the beginning of the buffer."
  (interactive "e")
  (with-selected-window (or (mwheel-event-window event) (selected-window))
    (ignore-errors
      (k/agent-shell-pixel-scroll
       'up (* (k/agent-shell-wheel-lines) (default-line-height))))))

(defun k/agent-shell-wheel-scroll-down (event)
  "Scroll the window under EVENT one step towards the end of the buffer."
  (interactive "e")
  (with-selected-window (or (mwheel-event-window event) (selected-window))
    (ignore-errors
      (k/agent-shell-pixel-scroll
       'down (* (k/agent-shell-wheel-lines) (default-line-height))))))

(defun k/agent-shell-paste-dwim (&optional arg)
  "Paste a clipboard image as file context, or paste text as usual.

agent-shell wires `agent-shell-yank-dwim' up by remapping `yank', which
never fires under cua-mode: cua remaps `yank' to `cua-paste' from a map
that outranks the major mode one, and Emacs applies a remapping only
once, so agent-shell's own `[remap yank]' is never consulted.  Dispatch
by hand instead.

Text keeps going through `cua-paste' so rectangle pasting still works.
ARG is passed on to it."
  (interactive "*P")
  (let ((targets (and (window-system) (gui-get-selection 'CLIPBOARD 'TARGETS))))
    (if (and (vectorp targets)
             (not (seq-contains-p targets 'image/png)))
        (cua-paste arg)
      (agent-shell-yank-dwim arg))))

;; Taking C-v back from cua-mode needs more than a major mode binding.  cua
;; keeps its CUA keys in `cua--cua-keys-keymap', published through
;; `emulation-mode-map-alists', and those maps outrank the major mode map — so
;; C-v resolves to `yank' there and remaps to `cua-paste' before
;; `agent-shell-mode-map' is ever consulted.  (S-<insert> is absent from that
;; keymap, which is why only C-v was affected.)  Join the same mechanism, in
;; front of cua's entry, switched on per buffer.
(defvar-local k/agent-shell-paste-override nil
  "Non-nil where `k/agent-shell-paste-override-map' should take effect.")

(defvar k/agent-shell-paste-override-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-v") #'k/agent-shell-paste-dwim)
    map)
  "Keymap reclaiming the paste key from cua-mode inside agent-shell.")

(add-to-list 'emulation-mode-map-alists
             `((k/agent-shell-paste-override . ,k/agent-shell-paste-override-map)))

(defun k/agent-shell-enable-paste-override ()
  "Let `k/agent-shell-paste-override-map' win over cua-mode here."
  (setq-local k/agent-shell-paste-override t))

(defun k/agent-shell--tolerate-partial-line ()
  "Allow a partially visible cursor line, but only after a pixel scroll.

`make-cursor-line-fully-visible' has to be nil for a pixel-sized scroll
to survive: otherwise redisplay scrolls back to show the line point is on
in full, undoing it.  Left nil for the whole buffer, though, it also
stops the window scrolling when point simply moves onto the partly
visible last line -- the one-line scroll every other buffer does.

Redisplay reads the flag after `post-command-hook' runs, so it can be
decided per command from here: nil for the redisplay following a scroll,
t for every other one."
  (setq-local make-cursor-line-fully-visible (not k/agent-shell--pixel-scrolled))
  (setq k/agent-shell--pixel-scrolled nil))

(defun k/agent-shell-manage-partial-lines ()
  "Arrange for `k/agent-shell--tolerate-partial-line' to run in this buffer."
  (add-hook 'post-command-hook #'k/agent-shell--tolerate-partial-line nil t))

;; agent-shell — the agent speaks ACP to a native Emacs buffer, so this is
;; not a terminal: the CLI's own slash commands (`/exit' & co.) don't apply.
;; Requires the Claude ACP layer:
;;   npm install -g @agentclientprotocol/claude-agent-acp
;;
;; Starting a session:
;;   M-x agent-shell                    start or reuse a session (C-u — pick agent)
;;   M-x agent-shell-anthropic-start-claude-code   Claude Code specifically
;;   M-x agent-shell-new-shell / -fork / -resume-session
;;   M-x agent-shell-prompt-compose     write the prompt in a separate buffer
;;   M-x agent-shell-switch-buffer / -buffers / -toggle
;;
;; Inside `agent-shell-mode':
;;   RET         `agent-shell-submit'
;;   C-c C-c     `agent-shell-interrupt' — cancel the running request
;;   C-x k       end the session: `kill-buffer' shuts the ACP client down
;;   C-c C-o     switch to the other agent-shell buffer
;;   C-<up> / C-<down>             previous / next input at the prompt,
;;                                 scroll one line anywhere above it
;;   wheel                         scrolls by pixels, not by whole lines
;;   C-<tab>     cycle session mode
;;   C-c C-m     set session `mode'
;;   C-c C-v     set `model'
;;   C-c C-t     set thought `level'
;;   C-c C-s     set a session config option
;;   TAB / S-TAB, n / p            next / previous item (prompt, block, image…)
;;   C-M-u       back up out of the current item
;;   r           quote region into the prompt
;;   + / - / 0   image scale increase / decrease / reset
;; The single-letter keys (n p r + - 0) self-insert while point is at the
;; input prompt, so they only navigate out in the transcript above it.
(use-package agent-shell
  :straight `(agent-shell
              :type git :host github
              :repo "xenodium/agent-shell"
              :branch "main")
  :after (shell-maker acp)
  ;; `k/agent-shell' rather than `agent-shell': it brings the Emacs MCP
  ;; server up first -- see the MCP bridge section at the end of this file.
  :bind (("C-M-a j" . k/agent-shell)
         :map agent-shell-mode-map
         ("C-<up>" . k/agent-shell-previous-input-or-scroll-down)
         ("C-<down>" . k/agent-shell-next-input-or-scroll-up)
         ("<wheel-up>" . k/agent-shell-wheel-scroll-up)
         ("<wheel-down>" . k/agent-shell-wheel-scroll-down)
         ;; C-v is handled by `k/agent-shell-paste-override-map' instead:
         ;; a binding here would be shadowed by cua-mode.
         ("S-<insert>" . k/agent-shell-paste-dwim))
  :hook ((agent-shell-mode . k/agent-shell-manage-partial-lines)
         (agent-shell-mode . k/agent-shell-enable-paste-override))
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  ;; Restoring a session defaults to `minimal': the title only, over
  ;; `session/resume', which replays no messages -- hence a resumed shell
  ;; that looks empty, unlike `/resume' in the terminal.  `full' replays the
  ;; whole conversation instead, via `session/load'; agents that don't
  ;; advertise `session/load' quietly fall back to the old behaviour.
  ;; Replaying a multi-megabyte conversation in one go is the heaviest thing
  ;; this mode does, so if the memory watchdog ever catches a restore, step
  ;; down to `first-last' (first and last turns) or `last'.
  (setq agent-shell-session-restore-verbosity 'full))

;; The agent icon in the header line is not governed by a face at all.
;; `agent-shell--fetch-agent-icon' downloads a PNG from lobe-icons — for
;; Anthropic that is `claudecode.png', named by `:icon-name' in
;; agent-shell-anthropic.el — and `svg-embed' drops those raster pixels
;; straight into the header SVG, where no face can reach them.  Tint a cached
;; copy on the way out instead.  Removing the tinted files regenerates them;
;; removing the originals makes agent-shell download them again.
(defcustom k/agent-shell-icon-color "#CE5C00"
  "Colour the agent icon in the agent-shell header is tinted with.
Set to nil to leave the downloaded icon as it comes."
  :type '(choice (const :tag "Leave as downloaded" nil)
                 (string :tag "Colour"))
  :group 'agent-shell)

(defun k/agent-shell-tint-agent-icon (path)
  "Return a copy of PATH tinted with `k/agent-shell-icon-color'.
PATH is returned untouched when tinting is off, when ImageMagick is
absent or when the conversion fails — the original icon beats a header
with no icon in it."
  (let ((magick (executable-find "magick")))
    (if (not (and path k/agent-shell-icon-color magick (file-exists-p path)))
        path
      (let ((tinted (format "%s-%s.png"
                            (file-name-sans-extension path)
                            (string-remove-prefix "#" k/agent-shell-icon-color))))
        (unless (file-exists-p tinted)
          (ignore-errors
            (call-process magick nil nil nil
                          path "-alpha" "on" "-channel" "RGB"
                          "-fill" k/agent-shell-icon-color "-colorize" "100"
                          (concat "PNG32:" tinted))))
        (if (file-exists-p tinted) tinted path)))))

(advice-add 'agent-shell--fetch-agent-icon
            :filter-return #'k/agent-shell-tint-agent-icon)

;; MCP bridge back into Emacs: xref, imenu, project and diagnostics tools the
;; agent can call.  agent-shell launches the agent itself and knows nothing
;; about claude-code-ide, so only the server half of that package is used
;; here — no terminal, no keybindings, no transient menu.
;;
;; Two things claude-code-ide does for itself when *it* spawns the CLI have to
;; be done by hand.  The port is random by default, but the URL has to be
;; stable to be written into an MCP config, so pin it.  And every tool body
;; runs inside `claude-code-ide-mcp-server-with-session-context', which errors
;; out unless the session id taken from the URL path is a registered one, so
;; a session has to be registered before the agent calls anything.  Its
;; project directory scopes the tools; re-register with another directory to
;; point them at a different project.
;;
;; Both happen on the first `k/agent-shell' rather than at startup, so an
;; Emacs that never talks to an agent never opens the port.
(defconst k/emacs-mcp-session-id "emacs"
  "Session id in the Emacs MCP server URL path.
The agent reaches the tools at http://localhost:PORT/mcp/SESSION-ID.")

;; `web-server' is a hard dependency of claude-code-ide, but straight names
;; local repo directories after the repo basename alone, and two unrelated
;; projects share the name "emacs-web-server": eschulte's (this package) and
;; skeeto's (which is simple-httpd).  Whichever is cloned first wins the
;; directory, and here it was skeeto's, so straight built a web-server with
;; no web-server.el in it, `ws-start' stayed undefined and the MCP server
;; failed to start.  A distinct `:local-repo' keeps the two apart.
(use-package web-server
  :straight `(web-server
              :type git :host github
              :repo "eschulte/emacs-web-server"
              :local-repo "eschulte-emacs-web-server"))

(use-package claude-code-ide
  :straight `(claude-code-ide
              :type git :host nil
              :repo "https://github.com/manzaltu/claude-code-ide.el"
              :branch "main")
  :init
  (setq claude-code-ide-mcp-server-port 51234)
  :after web-server
  :config
  ;; Registers the tools and flips `claude-code-ide-enable-mcp-server', which
  ;; `claude-code-ide-mcp-server-ensure-server' refuses to start without.
  ;; Neither opens a port on its own.
  (claude-code-ide-emacs-tools-setup))

(defun k/agent-shell-ensure-mcp-server ()
  "Start the Emacs MCP server and register its session unless already up.
Returns the port, or nil when the server could not be started -- callers
carry on either way, since the tools are an addition to what the agent
can do on its own and not a precondition for talking to it."
  (when-let* (((fboundp 'claude-code-ide-mcp-server-ensure-server))
              (port (claude-code-ide-mcp-server-ensure-server)))
    ;; Stopping the server clears the session table, so a session registered
    ;; before a restart is gone; registering resets the session's last-active
    ;; buffer, so leave a live one alone.
    (unless (claude-code-ide-mcp-server-get-session-context
             k/emacs-mcp-session-id)
      (claude-code-ide-mcp-server-register-session
       k/emacs-mcp-session-id (expand-file-name user-emacs-directory) nil))
    port))

(defun k/agent-shell (&optional arg)
  "Start or reuse an agent shell with the Emacs MCP tools server running.
ARG is passed to `agent-shell' untouched, prefix behaviour and all."
  (interactive "P")
  (k/agent-shell-ensure-mcp-server)
  (agent-shell arg))

(provide 'agent-shell-conf)
