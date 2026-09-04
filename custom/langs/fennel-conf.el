;;; fennel-conf.el

(use-package fennel-mode
  :vc (:url "https://github.com/emacsmirror/fennel-mode.git"
       :branch "master")
  :config
  (progn
    (setq fennel-program "fennel --repl")))

(provide 'fennel-conf)
