;;=============================================================================
;; Scheme
(require 'quack)

(defun kostafey-scheme-mode-hook ()
  (setq semantic-auto-parse-mode nil)
  (semantic-idle-scheduler-mode -1))
(add-hook 'scheme-mode-hook 'kostafey-scheme-mode-hook)

;;=============================================================================

(provide 'scheme-conf)

