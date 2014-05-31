(require 'elpa-conf)

;;----------------------------------------------------------------------
;; clojure
;;
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

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

;;----------------------------------------------------------------------
;; autocompletition - ac-nrepl
;;
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

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
;; clomacs and elisp-clojure extensions:
;; - clojure-offline
;;

(add-to-list 'load-path (concat site-lisp-path "clojure-offline/src/elisp/"))
(require 'clojure-offline)


(provide 'clojure-conf)
