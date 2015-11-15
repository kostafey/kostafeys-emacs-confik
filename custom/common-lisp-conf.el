
(setq inferior-lisp-program "sbcl")
(setq slime-net-coding-system 'utf-8-unix)

(slime-setup '(slime-repl
               slime-fuzzy
               slime-fancy-inspector
               slime-indentation))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(add-to-list 'auto-mode-alist '("\\.stumpwmrc$" . lisp-mode))

(provide 'common-lisp-conf)
