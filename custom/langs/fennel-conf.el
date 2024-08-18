(require 'elpa-conf)
(use-elpa 'use-package)

(use-package fennel-mode
  :ensure t
  :config
  (progn
    (setq fennel-program "fennel --repl")))

(provide 'fennel-conf)
