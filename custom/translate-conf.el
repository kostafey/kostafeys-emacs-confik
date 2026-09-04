;;; translate-conf.el

(use-package google-translate
  :vc (:url "https://github.com/atykhonov/google-translate.git"
       :branch "master")
  :init (progn
          (require 'google-translate)
          (require 'google-translate-smooth-ui))
  :bind (("C-C t" . google-translate-smooth-translate)
         ("C-c t" . google-translate-at-point))
  ;; Add translation to kill-ring after translate commands if it's `t'.
  :config (setq google-translate-translation-to-kill-ring t))

(provide 'translate-conf)
