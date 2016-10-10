;; Set of tips to increase responsibility speed for windows.

(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 511 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 10 t #'garbage-collect)

  (setq ac-delay 0.5)

  (if (>= emacs-major-version 25)
      (remove-hook 'find-file-hooks 'vc-refresh-state)
    (remove-hook 'find-file-hooks 'vc-find-file-hook)))

(provide 'perfomance-conf)

;;; perfomance-conf.el ends here
