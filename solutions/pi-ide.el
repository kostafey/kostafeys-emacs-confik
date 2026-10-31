;;; pi-ide.el --- Pi coding agent in Ghostel with Emacs MCP tools -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ghostel "0") (claude-code-ide "0"))
;; Keywords: tools, terminals

;;; Commentary:

;; `pi-ide' opens this project's Pi coding agent and connects it to an
;; Emacs MCP endpoint without editing Pi's mcp.json.  Pi has no
;; command-line MCP option, so pi-ide writes a small Pi extension, loads
;; it with `-e' and passes the endpoint as a flag the extension
;; registers.  The extension registers the endpoint as an MCP server for
;; that session only.
;; Terminals, the MCP endpoint and the editor tools come from `agent-ide'.
;; Explicit editor selections accompany prompts through the extension's
;; `before_agent_start' handler.  Other editor context and unsaved text
;; are fetched only on request.

;;; Code:

(require 'agent-ide)

(defgroup pi-ide nil
  "Pi coding agent integration with Emacs through Ghostel and MCP."
  :group 'tools :prefix "pi-ide-")

(defcustom pi-ide-cli "pi"
  "Pi executable to run in Ghostel."
  :type 'string)

(defcustom pi-ide-arguments nil
  "Additional arguments passed to Pi, such as (\"--model\" \"local/x\")."
  :type '(repeat string))

(defcustom pi-ide-enable-mcp t
  "Whether new Pi terminals connect to the Emacs MCP tools server."
  :type 'boolean)

(defcustom pi-ide-include-selection t
  "Attach the last editor buffer's active region to new Pi prompts.
Only the explicitly selected text and its location are included.  Without
an active region, no editor context is attached.  Requires MCP."
  :type 'boolean)

(defvar-local pi-ide-project-root nil
  "Project root associated with this Pi buffer.")

(defvar pi-ide--extension-file nil
  "Pi extension file written by this Emacs session, or nil.")

(declare-function ghostel-send-key "ghostel" (key-name &optional mods))

(defun pi-ide--fence (text)
  "Return a Markdown code fence longer than any backtick run in TEXT."
  (let ((longest 0) (start 0))
    (while (string-match "`+" text start)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    (make-string (max 3 (1+ longest)) ?`)))

(defun pi-ide--prompt-context (buffer selection context)
  "Return the extension's prompt context for SELECTION in editor BUFFER.
CONTEXT is the session context.  The result carries a `summary' for the
notification and the `context' message.  The selection is quoted
verbatim rather than JSON-escaped, which keeps it readable for small
local models."
  (let* ((file (buffer-file-name buffer))
         (name (if file
                   (file-relative-name file (plist-get context :project-dir))
                 (buffer-name buffer)))
         (start (alist-get 'start selection))
         (end (alist-get 'end selection))
         (text (alist-get 'text selection))
         (fence (pi-ide--fence text))
         ;; A region ending at a line start does not cover that line.
         (last (if (and (zerop (alist-get 'column end))
                        (> (alist-get 'line end) (alist-get 'line start)))
                   (1- (alist-get 'line end))
                 (alist-get 'line end))))
    `((summary . ,(format "%s:%d-%d" name (alist-get 'line start) last))
      (context
       . ,(format (concat "The user explicitly selected this text in "
                          "Emacs for the current prompt.  Treat it as "
                          "source context.\n"
                          "%s: %s, from line %d column %d "
                          "to line %d column %d "
                          "(lines 1-based, columns 0-based).\n"
                          "%s\n%s%s%s")
                  (if file "File" "Buffer") (or file name)
                  (alist-get 'line start) (alist-get 'column start)
                  (alist-get 'line end) (alist-get 'column end)
                  fence text
                  (if (string-suffix-p "\n" text) "" "\n")
                  fence)))))

;;; Pi extension

(defconst pi-ide--extension-source
  "// Written by pi-ide.el and loaded with `pi -e'.  It connects this Pi
// session to its Emacs MCP endpoint and attaches the explicit Emacs
// selection to each prompt.
export default function (pi) {
  pi.registerFlag(\"emacs-ide-url\", {
    type: \"string\",
    description: \"Emacs MCP endpoint of this pi-ide session\",
  });
  pi.registerFlag(\"emacs-ide-selection\", {
    type: \"boolean\",
    default: false,
    description: \"Attach the Emacs selection to each prompt\",
  });

  const endpoint = () => {
    const url = pi.getFlag(\"emacs-ide-url\");
    return typeof url === \"string\" && url ? url : undefined;
  };

  // Flags are parsed after extensions load, so register the server here.
  pi.on(\"session_start\", () => {
    const url = endpoint();
    if (!url) return;
    pi.registerMcpServer(\"emacs_ide\", {
      url,
      exposure: \"direct\",
      toolExposure: { get_prompt_context: \"hidden\" },
      description: \"Emacs buffers, selection, diagnostics and navigation\",
    });
  });

  pi.on(\"before_agent_start\", async (event, ctx) => {
    const url = endpoint();
    if (event.messageType !== \"user\" || !url
        || pi.getFlag(\"emacs-ide-selection\") !== true) return;
    let selection;
    try {
      const response = await fetch(url, {
        method: \"POST\",
        headers: {
          \"Content-Type\": \"application/json\",
          Accept: \"application/json, text/event-stream\",
        },
        body: JSON.stringify({
          jsonrpc: \"2.0\", id: 1, method: \"tools/call\",
          params: { name: \"get_prompt_context\", arguments: {} },
        }),
        signal: AbortSignal.timeout(5000),
      });
      const reply = await response.json();
      if (reply.error) throw new Error(reply.error.message);
      selection = JSON.parse(reply.result?.content?.[0]?.text ?? \"{}\");
    } catch (error) {
      ctx.ui.notify(`Emacs selection not attached: ${error}`, \"warning\");
      return;
    }
    if (!selection.context) return;
    ctx.ui.notify(`Attached Emacs selection ${selection.summary}`, \"info\");
    return {
      message: {
        customType: \"emacs-selection\",
        content: selection.context,
        display: false,
      },
    };
  });
}
"
  "TypeScript source of the Pi extension that pi-ide loads with `-e'.")

(defun pi-ide--extension-file ()
  "Return the Pi extension file, writing it when missing or outdated."
  (unless (and pi-ide--extension-file
               (file-exists-p pi-ide--extension-file)
               (equal (with-temp-buffer
                        (insert-file-contents pi-ide--extension-file)
                        (buffer-string))
                      pi-ide--extension-source))
    (pi-ide--delete-extension-file)
    (setq pi-ide--extension-file
          (make-temp-file "pi-ide-" nil ".ts" pi-ide--extension-source)))
  pi-ide--extension-file)

(defun pi-ide--delete-extension-file ()
  "Delete the Pi extension file written by this Emacs session."
  (when (and pi-ide--extension-file (file-exists-p pi-ide--extension-file))
    (ignore-errors (delete-file pi-ide--extension-file))))

(add-hook 'kill-emacs-hook #'pi-ide--delete-extension-file)

(defun pi-ide--mcp-arguments (url)
  "Return Pi arguments loading the extension and connecting it to URL."
  (append (list "-e" (pi-ide--extension-file) "--emacs-ide-url" url)
          (when pi-ide-include-selection
            (list "--emacs-ide-selection"))))

(defvar pi-ide--client nil
  "The Pi client of `agent-ide'.")

;; Redefined on every load, so reloading the file updates the client.
(setq pi-ide--client
      (agent-ide-define-client
       (agent-ide-client-create
        :name "Pi"
        :command 'pi-ide
        :buffer-prefix "pi"
        :id-prefix "pi-ide-"
        :cli-variable 'pi-ide-cli
        :arguments-variable 'pi-ide-arguments
        :enable-mcp-variable 'pi-ide-enable-mcp
        :include-selection-variable 'pi-ide-include-selection
        :root-variables '(pi-ide-project-root)
        :input-mode 'pi-ide-input-mode
        :mcp-arguments #'pi-ide--mcp-arguments
        ;; Pi already prefixes MCP tools with the server name, as in
        ;; `mcp__emacs_ide__read_buffer'.
        :tool-prefix ""
        :prompt-context #'pi-ide--prompt-context
        :prompt-context-description
        (concat "Prompt hook of the pi-ide extension: the explicit editor "
                "selection only, or an empty object.  Hidden from the model.")
        :server-name "pi-ide"
        :instructions
        (concat "Emacs tools operate in this Pi session's project.  "
                "The explicit editor selection is attached to prompts when "
                "enabled.  Use get_editor_context for editor context on "
                "request.  Use read_buffer for unsaved edits; file tools see "
                "only saved files.  Unselected files are never attached "
                "automatically."))))

;;; Terminal commands

(defun pi-ide-insert-newline ()
  "Insert a newline in the current Pi prompt without submitting it.
Sends C-j, Pi's newline key in every keyboard protocol."
  (interactive)
  (ghostel-send-key "j" "ctrl"))

(define-minor-mode pi-ide-input-mode
  "Keep Pi input bindings separate from other Ghostel sessions."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "S-<return>") #'pi-ide-insert-newline)
            (define-key map (kbd "S-RET") #'pi-ide-insert-newline)
            map)
  (agent-ide-input-mode-changed pi-ide--client))

(defun pi-ide ()
  "Open or switch to this project's Pi coding agent with Emacs MCP tools."
  (interactive)
  (agent-ide-open pi-ide--client))

(defun pi-ide-send-region (beg end)
  "Paste the region between BEG and END into this project's Pi session."
  (interactive "r")
  (agent-ide-send-region pi-ide--client beg end))

(defun pi-ide-session-buffer ()
  "Return the live Pi terminal of the current buffer's project, or nil."
  (agent-ide-session-buffer pi-ide--client))

(defun pi-ide-send-string (text)
  "Submit TEXT as a prompt to this project's Pi session."
  (agent-ide-send-string pi-ide--client text))

(provide 'pi-ide)

;;; pi-ide.el ends here
