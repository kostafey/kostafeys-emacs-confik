(require 'elpa-conf)
(require 'clojure-offline)

;; (dolist (p clojure-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))

(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

;; Stop the error buffer from popping up while working in the REPL buffer
(setq nrepl-popup-stacktraces nil)

;; Make C-c C-z switch to the *nrepl* buffer in the current window
(add-to-list 'same-window-buffer-names "*nrepl*")

;; Enabling CamelCase support for editing commands(like forward-word,
;; backward-word, etc) in nREPL is quite useful since we often have to deal with
;; Java class and method names.
(add-hook 'nrepl-mode-hook 'subword-mode)

;; autocompletition

;; (require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(defun initialize-cljs-repl ()
  (interactive)
  (insert "(require 'cljs.repl.browser)")
  (nrepl-return)
  (insert "(cemerick.piggieback/cljs-repl
:repl-env (doto (cljs.repl.browser/repl-env :port 9000)
cljs.repl/-setup))")
  (nrepl-return))

(provide 'clojure-conf)
