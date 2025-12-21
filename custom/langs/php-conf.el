;;; php-conf.el --- PHP configuration for lsp

(straight-use-package
 '(php-mode :type git :host github
				    :repo "emacs-php/php-mode" :branch "master"))

(add-hook 'php-mode-hook 'lsp)

(provide 'php-conf)

;;; php-conf.el ends here
