;;; php-conf.el --- PHP configuration  -*- lexical-binding: t -*-

(straight-use-package
 '(php-mode :type git :host github
				    :repo "emacs-php/php-mode" :branch "master"))

;; eglot runs intelephense, from `k/npm-language-servers-install'
;; (js-conf).
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(php-mode . ("intelephense" "--stdio"))))
(add-hook 'php-mode-hook #'eglot-ensure)

(provide 'php-conf)

;;; php-conf.el ends here
