;;; Large language models interaction configuration. -*- lexical-binding: t -*-

;; Start server:
;; -------------
;; llama-server -m ~/.cache/llama.cpp/Qwen_Qwen2.5-Coder-3B-Instruct-GGUF_qwen2.5-coder-3b-instruct-q4_k_m.gguf -c 20480

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
              (message "Current gptel context: \n%s" gptel-context)))

  :bind (("M-C-A b" . k/gptel-add-file)
         ("M-C-A s" . gptel-send)
         ("M-C-A q" . gptel-context-quit)
         ("M-C-A c" . k/gptel-context-print)
         ("M-C-A r" . gptel-context-remove-all)))

(provide 'llm-conf)
