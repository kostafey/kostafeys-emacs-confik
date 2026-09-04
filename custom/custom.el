(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(magit-save-some-buffers 'dontask)
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((paredit :url "https://github.com/emacsmirror/paredit.git" :branch "master")
     (eldoc :url "https://github.com/emacs-straight/eldoc.git" :branch
       "master")))
 '(safe-local-variable-values
   '((scala-indent:default-run-on-strategy quote
                                           scala-indent:keywords-only-strategy)
     (scala-indent:default-run-on-strategy
      . scala-indent:keywords-only-strategy)
     (scala-indent:indent-value-expression) (scala-indent:step . 2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column-indicator ((t (:foreground "#CCCCCC" :weight normal))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil))))
 '(speedbar-selected-face ((t (:foreground "#119011" :underline t)))))
