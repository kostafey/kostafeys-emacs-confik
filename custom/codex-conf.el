;;; codex-conf.el --- Configure Codex IDE integration -*- lexical-binding: t; -*-

(require 'codex-ide)

;; Organic Green's `organic-green' color, also used for successful tool markers.
(set-face-attribute 'codex-ide-working nil :foreground "#119011")

;; Preserve the personal names used before the implementation became a package.
(when (boundp 'k/codex-cli)
  (setq codex-ide-cli (symbol-value 'k/codex-cli)))
(defvaralias 'k/codex-cli 'codex-ide-cli)
(defalias 'k/codex #'codex-ide)
(defalias 'k/codex-send-region #'codex-ide-send-region)
(defalias 'k/codex-insert-newline #'codex-ide-insert-newline)

(global-set-key (kbd "C-M-a J") #'k/codex)
(global-set-key (kbd "C-M-a C") #'k/codex-send-region)

(provide 'codex-conf)
;;; codex-conf.el ends here
