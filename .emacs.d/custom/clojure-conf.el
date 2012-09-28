(require 'package)
(defvar clojure-packages '(clojure-mode
                           nrepl))
;; (dolist (p clojure-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))

(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

(setq nrepl-popup-stacktraces nil)

(add-to-list 'same-window-buffer-names "*nrepl*")

(provide 'clojure-conf)
