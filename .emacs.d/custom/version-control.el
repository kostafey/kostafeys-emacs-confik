(require 'magit)

(global-set-key (kbd "C-c C-s") 'magit-status)
(custom-set-variables
 '(magit-save-some-buffers (quote dontask)))

(provide 'version-control)
