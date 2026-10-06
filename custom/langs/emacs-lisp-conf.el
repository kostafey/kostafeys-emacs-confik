;;; -*- lexical-binding: t -*-
;;=============================================================================
;; Byte-compilation
;;
(setq max-specpdl-size 100000) ;for byte-compile
(setq max-lisp-eval-depth 500000)
;; cd ~/.emacs.d; emacs --batch -f batch-byte-compile **/*.el

;;=============================================================================
;; Indentation
;;
(defvar calculate-lisp-indent-last-sexp)

(defun k/lisp-indent-keyword-list (orig-fun indent-point state)
  "Indent a list starting with a keyword under its first element.
Delegate everything else to ORIG-FUN (`lisp-indent-function').
  before:              after:
  (:foo bar            (:foo bar
        :baz qux)       :baz qux)"
  (let ((last-sexp calculate-lisp-indent-last-sexp))
    (if (not (and (elt state 2)
                  (save-excursion
                    (goto-char (1+ (elt state 1)))
                    (parse-partial-sexp (point) last-sexp 0 t)
                    (looking-at-p ":"))))
        (funcall orig-fun indent-point state)
      ;; Same as the built-in branch for a non-symbol car: indent under
      ;; the first sexp of the line holding the last complete sexp.
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) last-sexp 0 t)
      (unless (> (line-beginning-position 2) last-sexp)
        (goto-char last-sexp)
        (beginning-of-line)
        (parse-partial-sexp (point) last-sexp 0 t))
      (backward-prefix-chars)
      (current-column))))

(advice-add 'lisp-indent-function :around #'k/lisp-indent-keyword-list)

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

(defun k/el-insert-eval-last-sexp ()
  (interactive)
  (insert (format " => %s" (eval (elisp--preceding-sexp)))))

(defun k/el-eval-buffer ()
  "Evaluate the current buffer and say so."
  (interactive)
  (eval-buffer)
  (message "Elisp buffer evaluated."))

(define-key emacs-lisp-mode-map (kbd "C-c p") 'k/el-insert-eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-p") 'k/el-pprint-eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-n e b") 'k/el-eval-buffer)

;; Eval Emacs Lisp in any mode
(global-set-key (kbd "C-c M-e") 'eval-last-sexp)
(global-set-key (kbd "C-c M-E") 'k/el-insert-eval-last-sexp)
;;=============================================================================
;; ElDoc
;;
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(provide 'emacs-lisp-conf)
