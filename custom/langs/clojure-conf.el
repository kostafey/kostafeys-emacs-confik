(require 'elpa-conf)
(use-elpa 'cider)

;;----------------------------------------------------------------------
;; clojure
;;
;; lsp
(use-elpa 'lsp-mode)
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)
(setq lsp-enable-file-watchers t)
(setq lsp-file-watch-threshold 10000)

(add-hook 'cider-mode-hook 'eldoc-mode)

;; Stop the error buffer from popping up while working in the REPL buffer
(setq cider-popup-stacktraces t)

;; Enable error buffer popping also in the REPL
(setq cider-repl-popup-stacktraces t)

;; Make C-c C-z switch to the *nrepl* buffer in the current window
(add-to-list 'same-window-buffer-names "*nrepl*")

;; It will attempt to connect via ssh to remote hosts when unable to connect
;; directly.
(setq nrepl-use-ssh-fallback-for-remote-hosts t)

;; Enabling CamelCase support for editing commands(like forward-word,
;; backward-word, etc) in nREPL is quite useful since we often have to deal with
;; Java class and method names.
(add-hook 'cider-mode-hook 'subword-mode)

(defun my-cider-in-ns ()
  "Switch current namespace to current buffer namespace."
  (interactive)
  (cider-interactive-eval
   (concat "(in-ns '" (cider-current-ns) ")") nil))

(defun my-cider-eval-buffer (&optional buffer)
  "Load (eval) BUFFER's file in nREPL and switch current namespace."
  (interactive)
  (cider-eval-buffer buffer)
  (my-cider-in-ns))

(setq cider-default-repl-command "lein")

;;----------------------------------------------------------------------
;; clojure-script
;;
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

(defun initialize-cljs-repl ()
  (interactive)
  (insert "(require 'cljs.repl.browser)")
  (cider-repl-return)
  (insert "(cemerick.piggieback/cljs-repl
  :repl-env (cljs.repl.browser/repl-env :port 9000))")
  (cider-repl-return))

;;----------------------------------------------------------------------
;; eval in vterm
;;
(use-elpa 'dash)
(use-elpa 'vterm)

(defun k/vterm-buffer ()
  "Set buffer `vterm-buffer-name' or return nil if buffer does not exist."
  (let ((current-vterm-buffer
         (-find (lambda (b) (equal (buffer-name b) vterm-buffer-name))
                (buffer-list))))
    (when current-vterm-buffer
      (set-buffer current-vterm-buffer))))

(defun k/clojure-send-vterm (data)
  "Send DATA as string to *vterm* buffer."
  (save-excursion
    (when (k/vterm-buffer)
      (vterm-send-string data)
      (vterm-send-return))))

(defun k/clojure-eval-last-sexp-in-vterm ()
  (interactive)
  (k/clojure-send-vterm (prin1-to-string (sexp-at-point))))

(defun k/nrepl-current-session ()
  "Return the current nrepl session or nil."
  (let* ((buff (or nrepl-connection-buffer
                   (cider-current-repl)))
         (sess (if buff
                   (with-current-buffer buff
                     nrepl-session))))
    sess))

(defun k/clojure-eval-last-sexp ()
  "Evaluate the expression preceding point in CIDER (if exists) or vterm."
  (interactive)
  (if (k/nrepl-current-session)
      (cider-eval-last-sexp)
    (k/clojure-eval-last-sexp-in-vterm)))

(defun k/clojure-get-current-namespace ()
  "Return current clojure file namespace as string."
  (save-excursion
    (goto-char (point-min))
    (let* ((beg (progn
                  (re-search-forward "(ns")
                  (point)))
           (end (progn
                  (re-search-forward "$")
                  (point))))
      (string-trim (buffer-substring beg end)))))

(defun k/clojure-switch-to-current-namespace ()
  "Pass `in-ns' expression to vterm."
  (interactive)
  (let ((ns (k/clojure-get-current-namespace)))
    (k/clojure-send-vterm (format "(in-ns '%s)" ns))
    (message ns)))

(provide 'clojure-conf)
