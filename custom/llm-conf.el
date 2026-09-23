;;; Large language models interaction configuration. -*- lexical-binding: t -*-

;; Start server:
;; -------------
;; `-hf' downloads models to the Hugging Face cache, `~/.cache/huggingface/hub'
;; (`$HF_HOME/hub' if set).
;;
;; `Qwen2.5-Coder-3B'
;; llama serve -hf bartowski/Qwen2.5-Coder-3B-Instruct-GGUF:Q5_K_M --port 8012
;;
;; `Qwen2.5-3B'
;; llama serve -hf bartowski/Qwen2.5-3B-Instruct-GGUF:Q5_K_M --port 8012
;;
;; `Qwen3-8B'
;; reasoning-budget 128:
;; llama serve -hf Qwen/Qwen3-8B-GGUF:Q4_K_M --reasoning-format deepseek --reasoning-budget 128 --reasoning-budget-message "Enough thinking, give the final answer now." -c 16384 -ngl 99 --port 8012
;; reasoning off:
;; llama serve -hf Qwen/Qwen3-8B-GGUF:Q4_K_M --reasoning off -c 16384 -ngl 99 --port 8012
;;
;; `Qwen3-4B'
;; Vulkan:
;; llama serve -hf Qwen/Qwen3-4B-GGUF:Q4_K_M --reasoning off -c 4096 -ngl 99 --device Vulkan1 -fa on -ctk q8_0 -ctv q8_0 --port 8012
;; CUDA:
;; llama serve -hf Qwen/Qwen3-4B-GGUF:Q4_K_M --reasoning off -c 40960 -ngl 99 -fa on -ctk q8_0 -ctv q8_0 --port 8012
;;
;; `Gemma-3-4B'
;; Vulkan:
;; llama serve -hf unsloth/gemma-3-4b-it-GGUF:Q4_K_M --no-mmproj -c 8192 -ngl 99 --device Vulkan1 -fa on -ctk q8_0 -ctv q8_0 --port 8012
;; CUDA:
;; llama serve -hf unsloth/gemma-3-4b-it-GGUF:Q4_K_M --no-mmproj -c 131072 -ngl 99 -fa on -ctk q8_0 -ctv q8_0 --port 8012
;;
;; `Ministral-3-3B'
;; Vulkan:
;; llama serve -hf unsloth/Ministral-3-3B-Instruct-2512-GGUF:Q5_K_M --no-mmproj -c 8192 -ngl 99 --device Vulkan1 -fa on -ctk q8_0 -ctv q8_0 --temp 0.15 --port 8012
;; CUDA:
;; llama serve -hf unsloth/Ministral-3-3B-Instruct-2512-GGUF:Q5_K_M --no-mmproj -c 65536 -ngl 99 -fa on -ctk q8_0 -ctv q8_0 --temp 0.15 --port 8012
;;
;; Qwen3 thinks by default. `--reasoning-format deepseek' keeps the thoughts
;; out of `message.content' and `gptel-include-reasoning' set to nil drops
;; them, but they are still generated and still cost time:
;; proofreading one sentence took 264 tokens of thinking, 5.3 s, and
;; arrived at the same answer 18 tokens gave without it.  The budget cuts
;; the thought short at 128 tokens, some two seconds at ~50 tok/s, and the
;; message asks for the answer instead of leaving it cut off mid-sentence.
;;
;; Thinking off costs nothing on rewriting, proofreading and code, but it
;; does cost `k/llm-suggest': asked to continue a sentence, the model
;; repeated it back instead, three runs out of five, against none out of
;; five with thinking on.  The instruction is the hard part there, not the
;; text.

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
               (chat        . "You are a large language model and a conversation partner. Respond concisely."))
             gptel-include-reasoning nil)

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
                        :callback #'k/gptel--insert-keeping-cursor
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
The source text flashes to show what was sent, the response is inserted
after it, and point, the mark and the scroll position stay where they
were."
              (interactive)
              (let* ((bounds (if (use-region-p)
                                 (cons (region-beginning) (region-end))
                               (save-mark-and-excursion
                                 (mark-paragraph)
                                 (cons (region-beginning) (region-end)))))
                     (selected-region (buffer-substring (car bounds) (cdr bounds)))
                     (system-message (cdr (assq 'writing gptel-directives))))
                (k/flash-region (car bounds) (cdr bounds))
                (gptel--sanitize-model)
                (let ((fsm (gptel-make-fsm :handlers gptel-send--handlers)))
                  (gptel-request
                      (format "%s: `%s`. %s."
                              "Rewrite this text in canonical English for technical documentation"
                              (concat selected-region)
                              "Do not quote the result text")
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
                (gptel--update-status " Waiting..." 'warning)))

            ;;-------------------------------------------------------
            ;; Inline suggestion
            ;;
            ;; The LLM continues the text before point and the result is
            ;; shown as a shadowed overlay, the way a copilot does it.  This
            ;; is deliberately not a `completion-at-point' function: capfs
            ;; have to answer synchronously, while a request to the local
            ;; server takes long enough for `corfu' to have moved on.  Word
            ;; completion in the corfu popup comes from the dictionaries in
            ;; `dict-conf.el' instead.

            (defvar k/llm-suggest-context-chars 2000
              "How much text before point is sent as context.")

            (defvar k/llm-suggest-overlay nil
              "Overlay showing the suggestion, if any.")

            (defvar k/llm-suggest--exit nil
              "Function deactivating `k/llm-suggest-map'.")

            (defvar k/llm-suggest-map
              (let ((map (make-sparse-keymap)))
                (define-key map (kbd "TAB") #'k/llm-suggest-accept)
                (define-key map (kbd "<tab>") #'k/llm-suggest-accept)
                (define-key map (kbd "C-<return>") #'k/llm-suggest-accept)
                (define-key map (kbd "C-g") #'k/llm-suggest-dismiss)
                map)
              "Keymap active while a suggestion is on screen.
Any key outside of it dismisses the suggestion and runs as usual.")

            (defun k/llm-suggest--clear ()
              "Remove the suggestion overlay and its transient keymap."
              (when (overlayp k/llm-suggest-overlay)
                (delete-overlay k/llm-suggest-overlay))
              (setq k/llm-suggest-overlay nil)
              ;; Cleared first: this function is also the map's exit hook,
              ;; so it would otherwise call itself.
              (let ((exit k/llm-suggest--exit))
                (setq k/llm-suggest--exit nil)
                (when exit (funcall exit))))

            (defun k/llm-suggest-dismiss ()
              "Discard the suggestion on screen."
              (interactive)
              (k/llm-suggest--clear)
              (message "Suggestion dismissed"))

            (defun k/llm-suggest-accept ()
              "Insert the suggestion on screen."
              (interactive)
              (let ((overlay k/llm-suggest-overlay))
                (if (not (overlayp overlay))
                    (k/llm-suggest--clear)
                  (let ((text (overlay-get overlay 'k/llm-suggest))
                        (position (overlay-start overlay)))
                    (k/llm-suggest--clear)
                    (goto-char position)
                    (insert text)))))

            (defun k/llm-suggest--clean (text)
              "Drop markdown fences and surrounding newlines from TEXT.
A leading space is kept, it is often exactly what the continuation
needs, but a leading newline is not: it is what is left over once a
fenced block loses its fence, and inserting it would break the line."
              (string-trim-right
               (replace-regexp-in-string
                "\\`\n+" ""
                (replace-regexp-in-string "^```.*$" "" text))))

            (defun k/llm-suggest--show (text marker)
              "Show TEXT as the suggested continuation at MARKER."
              (if (or (string-empty-p text) (not (marker-buffer marker)))
                  (message "No suggestion")
                (with-current-buffer (marker-buffer marker)
                  (let ((display (propertize text 'face 'shadow))
                        (overlay (make-overlay marker marker nil t t)))
                    ;; Keeps the cursor drawn before the suggestion instead
                    ;; of after it.
                    (put-text-property 0 1 'cursor t display)
                    (overlay-put overlay 'k/llm-suggest text)
                    (overlay-put overlay 'after-string display)
                    (setq k/llm-suggest-overlay overlay
                          k/llm-suggest--exit
                          (set-transient-map k/llm-suggest-map t
                                             #'k/llm-suggest--clear))
                    (message "TAB accepts the suggestion, any other key drops it")))))

            (defun k/llm-suggest ()
              "Ask the LLM to continue the text before point.
The suggestion is shown inline; \\[k/llm-suggest-accept] inserts it."
              (interactive)
              (k/llm-suggest--clear)
              (let* ((beg (max (point-min) (- (point) k/llm-suggest-context-chars)))
                     (prefix (buffer-substring-no-properties beg (point)))
                     (prog-lang (get-language-from-mode))
                     (programming-buffer-p (not (equal prog-lang "Unknown")))
                     (system-message (if programming-buffer-p
                                         (cdr (assq 'code-only gptel-directives))
                                       (cdr (assq 'writing gptel-directives))))
                     (marker (copy-marker (point))))
                (gptel--sanitize-model)
                (gptel-request
                    (format (concat "Continue the %s below, starting exactly at its end. "
                                    "Output ONLY the continuation: no explanation, "
                                    "no quotes, no markdown code blocks (```), "
                                    "and do not repeat any of the given text. "
                                    "Keep it to %s.\n\n%s")
                            (if programming-buffer-p
                                (format "%s code" prog-lang)
                              "text")
                            (if programming-buffer-p "a single line" "one sentence")
                            prefix)
                  :stream nil
                  :system system-message
                  :callback
                  (lambda (response info)
                    (if (stringp response)
                        (k/llm-suggest--show (k/llm-suggest--clean response) marker)
                      (message "No suggestion: %s"
                               (or (plist-get info :status) response)))))
                (message "Querying %s for a suggestion..."
                         (gptel-backend-name gptel-backend)))))

  :bind (("M-C-a b" . k/gptel-add-file)
         ("M-C-a s" . gptel-send)
         ("M-C-a m" . gptel-menu)
         ("M-C-a q" . gptel-context-quit)
         ("M-C-a c" . k/gptel-context-print)
         ("M-C-a l" . k/gptel-context-print)
         ("M-C-a r" . gptel-context-remove-all)
         ("M-C-a x" . k/gptel-minibuffer)
         ("M-C-a w" . k/gptel-rewrite)
         ("M-C-a i" . k/llm-suggest)))

(provide 'llm-conf)
