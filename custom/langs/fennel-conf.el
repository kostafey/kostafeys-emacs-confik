;;; fennel-conf.el

(use-package fennel-mode
  :straight '(fennel-mode :type git :host github
			                    :repo "emacsmirror/fennel-mode" :branch "master")
  :config
  (progn
    (setq fennel-program "fennel --repl")))

(provide 'fennel-conf)
