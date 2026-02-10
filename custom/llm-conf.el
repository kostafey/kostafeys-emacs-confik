;;; Large language models interaction configuration. -*- lexical-binding: t -*-

;; Start server:
;; -------------
;; llama-server -m ~/.cache/llama.cpp/Qwen_Qwen2.5-Coder-3B-Instruct-GGUF_qwen2.5-coder-3b-instruct-q4_k_m.gguf -c 40960

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
                             :host "localhost:8080"
                             :models '(Qwen_Qwen2.5-Coder-3B-Instruct-GGUF)))

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

            (defun k/gptel-minibuffer ()
              "Prompt for a query in the minibuffer."
              (interactive)
              (let* ((prog-lang (get-language-from-mode))
                     (prompt (read-string
                              (format "Query gptel%s: "
                                      (if (not (equal prog-lang "Unknown"))
                                          (format " (%s)" prog-lang)
                                        "")))))
                (when prompt
                  (progn
                    (gptel--sanitize-model)
                    (let (
                          (fsm (gptel-make-fsm :handlers gptel-send--handlers)))
                      (gptel-request
                          (format "%s%s"
                                  (if (not (equal prog-lang "Unknown"))
                                      (format "Use programming language: %s. "
                                              prog-lang)
                                    "")
                                  prompt)
                        :stream gptel-stream
                        :transforms gptel-prompt-transform-functions
                        :fsm fsm)
                      (message "Querying %s..."
                               (thread-first (gptel-fsm-info fsm)
                                             (plist-get :backend)
                                             (or gptel-backend)
                                             (gptel-backend-name))))
                    (gptel--update-status " Waiting..." 'warning))))))

  :bind (("M-C-a b" . k/gptel-add-file)
         ("M-C-a s" . gptel-send)
         ("M-C-a m" . gptel-menu)
         ("M-C-a q" . gptel-context-quit)
         ("M-C-a c" . k/gptel-context-print)
         ("M-C-a l" . k/gptel-context-print)
         ("M-C-a r" . gptel-context-remove-all)
         ("M-C-a x" . k/gptel-minibuffer)))

(provide 'llm-conf)
