(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default-input-method "russian-computer")
 '(ecb-layout-name "my-left")
 '(ecb-options-version "2.40")
 '(edts-man-root "/home/kostafey/.emacs.d/edts/doc/R7B")
 '(jde-jdk-registry (quote (("1.6.0" . "C:\\Java\\jdk1.6.0_24\\"))))
 '(magit-save-some-buffers (quote dontask))
 '(mouse-wheel-down-event (quote wheel-up))
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-up-event (quote wheel-down))
 '(package-selected-packages
   (quote
    (use-package org google-translate direx npm-mode rjsx-mode restclient gorepl-mode java-snippets flycheck-rust cargo racer rust-mode darcsum vc-darcs rg gruvbox-theme doom-themes sql-indent nord-theme temporary-persistent langtool jdee groovy-mode json-reformat d-mode web-beautify tern tern-auto-complete js-comint jedi-direx drawille request markdown-mode yaml-mode clojure-mode cider sbt-mode slime go-mode auto-complete yasnippet flx flycheck paredit s simple-httpd js2-mode undercover git-gutter-fringe wrap-region web-mode vlf typing tabbar sr-speedbar sphinx-frontend smex skewer-mode rainbow-mode rainbow-delimiters quack projectile powershell popup-switcher php-mode paredit-everywhere organic-green-theme noflet names multiple-cursors markdown-toc magit lua-mode lorem-ipsum log4j-mode javadoc-lookup jabber idle-highlight-mode icomplete+ highlight-symbol highlight-parentheses goto-last-change go-eldoc go-autocomplete git-gutter flycheck-tip flycheck-clojure flx-isearch flx-ido fill-column-indicator erlang ensime elisp-slime-nav elfeed ecb dired+ diffview diff-hl clojure-snippets clojure-mode-extra-font-locking cl-lib-highlight circe auctex ahg ag ace-jump-mode ac-slime ac-etags ac-cider)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail))))
 '(semantic-new-buffer-setup-functions
   (quote
    ((emacs-lisp-mode . semantic-default-elisp-setup)
     (c-mode . semantic-default-c-setup)
     (c++-mode . semantic-default-c-setup)
     (arduino-mode . semantic-default-c-setup)
     (html-mode . semantic-default-html-setup)
     (java-mode . wisent-java-default-setup)
     (js-mode . wisent-javascript-setup-parser)
     (python-mode . wisent-python-default-setup)
     (f90-mode . semantic-default-f90-setup)
     (srecode-template-mode . srecode-template-setup-parser)
     (texinfo-mode . semantic-default-texi-setup)
     (makefile-automake-mode . semantic-default-make-setup)
     (makefile-gmake-mode . semantic-default-make-setup)
     (makefile-makepp-mode . semantic-default-make-setup)
     (makefile-bsdmake-mode . semantic-default-make-setup)
     (makefile-imake-mode . semantic-default-make-setup)
     (makefile-mode . semantic-default-make-setup))))
 '(session-use-package t nil (session))
 '(show-trailing-whitespace nil)
 '(visible-bell nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit nil)))))
