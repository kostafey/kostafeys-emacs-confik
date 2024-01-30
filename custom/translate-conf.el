(require 'elpa-conf)
(use-elpa 'google-translate)
(require 'google-translate)

(require 'google-translate-smooth-ui)
(global-set-key (kbd "C-C t") 'google-translate-smooth-translate)
(global-set-key (kbd "C-c t") 'google-translate-at-point)
;; Add translation to kill-ring after translate commands if it's `t'.
(setq google-translate-translation-to-kill-ring t)

(provide 'translate-conf)
