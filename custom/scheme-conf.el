;;=============================================================================
;; Scheme

(defun kostafey-scheme-mode-hook ()
  (setq semantic-auto-parse-mode nil)
  (setq semantic-idle-scheduler-mode nil))
(add-hook 'scheme-mode-hook 'kostafey-scheme-mode-hook)

;;=============================================================================

(provide 'scheme-conf)
