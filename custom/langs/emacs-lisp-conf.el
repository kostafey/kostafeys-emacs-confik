;;=============================================================================
;; Byte-compilation
;;
(setq max-specpdl-size 100000) ;for byte-compile
(setq max-lisp-eval-depth 500000)
;; cd ~/.emacs.d; emacs --batch -f batch-byte-compile **/*.el

(load-file "~/.emacs.d/artifacts/redef.el")

(setq eval-expression-print-level nil)

(defun byte-recompile-custom-files()
  (interactive)
  (let ((site-lisp-path "~/.emacs.d/"))
    (byte-recompile-directory (concat site-lisp-path "custom/") 0 t)
    (byte-recompile-directory (concat site-lisp-path "custom/langs") 0 t)
    (byte-recompile-directory (concat site-lisp-path "solutions") 0 t)
    (byte-recompile-directory (concat site-lisp-path "artifacts/") 0 t)))

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defun native-compile-current-buffer ()
  "`native-compile' current buffer if it's emacs-lisp-mode."
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (native-compile-async buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

;;------------------------------------------------------------
;; pprint
(define-derived-mode elisp-result-mode emacs-lisp-mode "elisp-result"
  "Major mode for emacs lisp result.")

(defun pprint (form)
  (let ((result-buffer-name "*elisp-result*"))
    (if (buffer-live-p (get-buffer-create result-buffer-name))
        (kill-buffer result-buffer-name))
    (let ((buffer (get-buffer-create result-buffer-name)))
      (temp-buffer-window-show
       buffer
       (with-current-buffer buffer
         (elisp-result-mode)
         (let ((map (current-local-map)))
           (define-key map "q" 'quit-window))
         (princ (cl-prettyprint form)))))))

(defun k/el-pprint-eval-last-sexp ()
  (interactive)
  (pprint (eval (elisp--preceding-sexp))))

;;=============================================================================
;; ElDoc
;;
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(provide 'emacs-lisp-conf)
