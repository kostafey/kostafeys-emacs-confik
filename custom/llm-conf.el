;;; Large language models interaction configuration. -*- lexical-binding: t -*-

;; Start server:
;; -------------
;; `Qwen2.5-Coder-3B'
;; llama-server -m ~/.cache/llama.cpp/Qwen_Qwen2.5-Coder-3B-Instruct-GGUF_qwen2.5-coder-3b-instruct-q5_k_m.gguf -c 40960
;;
;; llama serve -hf bartowski/Qwen2.5-Coder-3B-Instruct-GGUF:Q5_K_M --port 8012 --cors-origins "http://localhost:8012"

(defun get-language-from-mode ()
  "Get the programming language name from the mode name."
  (let ((mode-name (symbol-name major-mode)))
    (cond
     ((string= mode-name "java-mode")
      "Java")
     ((string= mode-name "scala-mode")
      "Scala")
     ((string= mode-name "clojure-mode")
      "Clojure")
     ((string= mode-name "sql-mode")
      "SQL")
     ((string= mode-name "emacs-lisp-mode")
      "Emacs Lisp")
     ((string= mode-name "fennel-mode")
      "Fennel")
     ((string= mode-name "xml-mode")
      "XML")
     ((string= mode-name "lisp-mode")
      "Lisp")
     ((string= mode-name "javascript-mode")
      "JavaScript")
     ((string= mode-name "typescript-mode")
      "TypeScript")
     ((string= mode-name "go-mode")
      "Go")
     ((string= mode-name "rust-mode")
      "Rust")
     ((member mode-name '("c-mode" "c++-mode"))
      "C/C++")
     ((string= mode-name "php-mode")
      "PHP")
     ((string= mode-name "python-mode")
      "Python")
     ((string= mode-name "ruby-mode")
      "Ruby")
     ((string= mode-name "perl-mode")
      "Perl")
     (t
      "Unknown"))))

(use-package gptel
  :straight 'gptel
  :config (progn
            ;; Llama.cpp offers an OpenAI compatible API
            (setq
             gptel-model   'Qwen_Qwen2.5-Coder-3B-Instruct-GGUF
             gptel-backend (gptel-make-openai "llama-cpp"
                             :stream t
                             :protocol "http"
                             :host "localhost:8012"
                             :models '(Qwen_Qwen2.5-Coder-3B-Instruct-GGUF))
             gptel-directives
             '((default     . "You are a large language model and a helpful assistant. Respond concisely.")
               (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
               (code-only   . "Output ONLY the requested code. No prose, no markdown formatting, and no ``` blocks.")
               (writing     . "You are a large language model and a writing assistant. Respond concisely.")
               (chat        . "You are a large language model and a conversation partner. Respond concisely.")))

            (defun k/gptel-add-file ()
              "Send the current buffer-file to gptel-add-file function."
              (interactive)
              (let ((file-name (buffer-file-name)))
                (gptel-add-file file-name)
                (message (format "gptel add file to LLM context: %s"
                                 file-name))))

            (defun k/gptel-context-print ()
              "Output gptel-context variable in minibuffer as message."
              (interactive)
              (message "Current gptel context: \n%s" gptel-context))

            (defun k/gptel-strip-code-blocks (beg end)
              "Remove markdown code blocks from the LLM response."
              (when (not (equal (get-language-from-mode) "Unknown"))
                (save-excursion
                  (goto-char beg)
                  (while (re-search-forward "^```.*$" end t)
                    (replace-match "")))))

            (add-hook 'gptel-post-response-functions #'k/gptel-strip-code-blocks)

            (defun k/gptel-minibuffer ()
              "Prompt for a query in the minibuffer."
              (interactive)
              (let* ((selected-region (when (use-region-p)
                                        (let ((beg (region-beginning))
                                              (end (region-end)))
                                          (buffer-substring beg end))))
                     (prog-lang (get-language-from-mode))
                     (programming-buffer-p (not (equal prog-lang "Unknown")))
                     (system-message (if programming-buffer-p
                                         (cdr (assq 'code-only gptel-directives))
                                       (cdr (assq 'default gptel-directives))))
                     (prompt (read-string
                              (format "Query gptel%s: "
                                      (if programming-buffer-p
                                          (format " (%s)" prog-lang)
                                        "")))))
                (when prompt
                  (progn
                    (gptel--sanitize-model)
                    (let ((fsm (gptel-make-fsm :handlers gptel-send--handlers)))
                      (gptel-request
                          (format "%s%s%s"
                                  (if selected-region
                                      (concat selected-region "\n")
                                    "")
                                  (if programming-buffer-p
                                      (format (concat "Use programming language: %s. "
                                                      "Do NOT write explanations. "
                                                      "Output ONLY the code. "
                                                      "Do not use markdown code blocks (```) for code. ")
                                              prog-lang)
                                    "")
                                  prompt)
                        :stream gptel-stream
                        :system system-message
                        :transforms gptel-prompt-transform-functions
                        :fsm fsm)
                      (message "Querying %s..."
                               (thread-first (gptel-fsm-info fsm)
                                             (plist-get :backend)
                                             (or gptel-backend)
                                             (gptel-backend-name))))
                    (gptel--update-status " Waiting..." 'warning)))))

            (defun k/gptel--save-cursor ()
              "Return the cursor state to restore once an LLM response arrives.
The result holds the current position, plus the window showing it and
that window's scroll position."
              (let ((window (selected-window)))
                (list :point (point-marker)
                      :window window
                      :window-start (copy-marker (window-start window)))))

            (defun k/gptel--restore-cursor (state)
              "Put the cursor back to the position recorded in STATE.
STATE comes from `k/gptel--save-cursor'.  The point of the originating
window and its scroll position are restored as well, so text inserted
by gptel neither drags the cursor along nor scrolls the buffer."
              (when-let* ((position (plist-get state :point))
                          (buffer (marker-buffer position)))
                (with-current-buffer buffer
                  (goto-char position))
                (let ((window (plist-get state :window)))
                  (when (and (window-live-p window)
                             (eq (window-buffer window) buffer))
                    (set-window-point window position)
                    (set-window-start window (plist-get state :window-start) t)))))

            (defun k/gptel--response-done-p (response info)
              "Return non-nil when RESPONSE completes the request described by INFO.
A streamed request ends with a RESPONSE of t, a non-streamed one with
the response text itself.  Failed and aborted requests get a RESPONSE
of nil or `abort' and report their own status, so they are not counted
as done here."
              (if (plist-get info :stream)
                  (eq response t)
                (stringp response)))

            (defun k/gptel--insert-keeping-cursor (response info)
              "Insert RESPONSE with gptel's own handler, keeping the cursor in place.
INFO is the gptel request plist; its `:context' carries the cursor
state saved by `k/gptel--save-cursor'.  A streamed response calls this
for every chunk, so the cursor stays put for the whole response.  The
\"Querying...\" message is replaced with \"Done\" once the response ends."
              (if (plist-get info :stream)
                  (gptel-curl--stream-insert-response response info)
                (gptel--insert-response response info))
              (k/gptel--restore-cursor (plist-get info :context))
              (when (k/gptel--response-done-p response info)
                (message "Done %s" (gptel-backend-name
                                    (or (plist-get info :backend)
                                        gptel-backend)))))

            (defun k/gptel-rewrite ()
              "Rewrite the region, or the paragraph at point, for technical documentation.
The response is inserted after the source text, while point, the mark
and the scroll position stay where they were."
              (interactive)
              (let* ((bounds (if (use-region-p)
                                 (cons (region-beginning) (region-end))
                               (save-mark-and-excursion
                                 (mark-paragraph)
                                 (cons (region-beginning) (region-end)))))
                     (selected-region (buffer-substring (car bounds) (cdr bounds)))
                     (system-message (format "%s %s"
                                             (cdr (assq 'writing gptel-directives))
                                             "Rewrite this text in canonical English for technical documentation.")))
                (gptel--sanitize-model)
                (let ((fsm (gptel-make-fsm :handlers gptel-send--handlers)))
                  (gptel-request
                      (concat selected-region "\n")
                    :stream gptel-stream
                    :system system-message
                    :position (copy-marker (cdr bounds))
                    :context (k/gptel--save-cursor)
                    :callback #'k/gptel--insert-keeping-cursor
                    :transforms gptel-prompt-transform-functions
                    :fsm fsm)
                  (message "Querying %s..."
                           (thread-first (gptel-fsm-info fsm)
                                         (plist-get :backend)
                                         (or gptel-backend)
                                         (gptel-backend-name))))
                (gptel--update-status " Waiting..." 'warning))))

  :bind (("M-C-a b" . k/gptel-add-file)
         ("M-C-a s" . gptel-send)
         ("M-C-a m" . gptel-menu)
         ("M-C-a q" . gptel-context-quit)
         ("M-C-a c" . k/gptel-context-print)
         ("M-C-a l" . k/gptel-context-print)
         ("M-C-a r" . gptel-context-remove-all)
         ("M-C-a x" . k/gptel-minibuffer)
         ("M-C-a w" . k/gptel-rewrite)))

(provide 'llm-conf)
