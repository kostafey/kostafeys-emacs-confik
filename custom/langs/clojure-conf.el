(require 'elpa-conf)

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
;; autocompletition - ac-cider
;;
(use-elpa 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;;----------------------------------------------------------------------
;; clojure-script
;;
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(defun initialize-cljs-repl ()
  (interactive)
  (insert "(require 'cljs.repl.browser)")
  (cider-repl-return)
  (insert "(cemerick.piggieback/cljs-repl
  :repl-env (cljs.repl.browser/repl-env :port 9000))")
  (cider-repl-return))

;;----------------------------------------------------------------------
;; flycheck-clojure
;;
;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (require 'flycheck-tip)
;; (flycheck-tip-use-timer 'verbose)

(provide 'clojure-conf)
