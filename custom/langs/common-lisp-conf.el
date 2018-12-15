(require 'functions)

(setq inferior-lisp-program "ros run")
(setq slime-net-coding-system 'utf-8-unix)

(add-to-list 'load-path
             (file-name-as-directory
              (concat-path site-lisp-path
                           "artifacts"
                           "slime-repl-ansi-color")))
(require 'slime)
;; (require 'slime-repl-ansi-color)

(slime-setup '(slime-repl
               slime-fuzzy
               slime-banner
               slime-fancy-inspector
               slime-indentation
               slime-asdf
               ;; slime-repl-ansi-color
               ))

(setq slime-header-line-p nil)

(setq common-lisp-hyperspec-root
      (let ((local-hyperspec
             (file-name-as-directory
              (concat-path site-lisp-path "HyperSpec"))))
        (if (file-exists-p local-hyperspec)
            (concat "file://" local-hyperspec)
          "http://www.lispworks.com/reference/HyperSpec/")))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(add-to-list 'auto-mode-alist '("\\.stumpwmrc$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

(provide 'common-lisp-conf)
