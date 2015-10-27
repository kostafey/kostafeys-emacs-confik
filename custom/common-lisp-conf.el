
(setq inferior-lisp-program "sbcl")
(setq slime-net-coding-system 'utf-8-unix)

(slime-setup '(slime-repl
               slime-fuzzy
               slime-fancy-inspector
               slime-indentation))

(add-to-list 'auto-mode-alist '("\\.stumpwmrc$" . lisp-mode))

(provide 'common-lisp-conf)
