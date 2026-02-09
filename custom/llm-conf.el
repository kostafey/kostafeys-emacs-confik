;;; Large language models interaction configuration. -*- lexical-binding: t -*-

;; Start server:
;; -------------
;; llama-server -m ~/.cache/llama.cpp/Qwen_Qwen2.5-Coder-3B-Instruct-GGUF_qwen2.5-coder-3b-instruct-q4_k_m.gguf -c 40960

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
                (message (format
                          "%s add file to LLM context: %s"
                          (propertize "gptel"
                                      'face 'font-lock-keyword-face)
                          file-name))))

            (defun k/gptel-context-print ()
              "Output gptel-context variable in minibuffer as message."
              (interactive)
              (message "Current gptel context: \n%s" gptel-context))

            (defun k/gptel-minibuffer ()
              "Prompt for a query in the minibuffer."
              (interactive)
              (let ((prompt (read-string (format
                                          "Query %s: "
                                          (propertize "gptel"
                                                      'face 'font-lock-keyword-face)))))
                (when prompt
                  (progn
                    (gptel--sanitize-model)
                    (let (
                          (fsm (gptel-make-fsm :handlers gptel-send--handlers)))
                      (gptel-request prompt
                        :stream gptel-stream
                        :transforms gptel-prompt-transform-functions
                        :fsm fsm)
                      (message "Querying %s..."
                               (thread-first (gptel-fsm-info fsm)
                                             (plist-get :backend)
                                             (or gptel-backend)
                                             (gptel-backend-name))))
                    (gptel--update-status " Waiting..." 'warning))))))

  :bind (("M-C-A b" . k/gptel-add-file)
         ("M-C-A s" . gptel-send)
         ("M-C-A m" . gptel-menu)
         ("M-C-A q" . gptel-context-quit)
         ("M-C-A c" . k/gptel-context-print)
         ("M-C-A r" . gptel-context-remove-all)
         ("M-C-A x" . k/gptel-minibuffer)))

(provide 'llm-conf)
