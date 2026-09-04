;;; php-conf.el --- PHP configuration for lsp

(use-package php-mode
  :vc (:url "https://github.com/emacs-php/php-mode.git"
       :branch "master"
       :lisp-dir "lisp")
  :defer t)

(add-hook 'php-mode-hook 'lsp)

(provide 'php-conf)

;;; php-conf.el ends here
