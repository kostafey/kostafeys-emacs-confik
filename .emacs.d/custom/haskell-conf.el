(load (concat site-lisp-path "haskell-mode-2.7.0/haskell-site-file.el"))

(add-hook 'haskell-mode-hook 'font-lock-mode)
(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; (setq haskell-program-name "C:/Progra~1/WinHugs/hugs.exe -98")
;; (setq haskell-program-name "C:/Progra~1/WinHugs/hugs.exe +98")
;; (setq haskell-program-name "C:/Program~1/Haskell~1/2009.2.0.2/bin/ghci.exe")

(setq haskell-program-name "ghci.exe")

(provide 'haskell-conf)
