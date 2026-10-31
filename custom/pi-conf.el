;;; pi-conf.el --- Configure Pi IDE integration -*- lexical-binding: t; -*-

(require 'pi-ide)

(global-set-key (kbd "C-M-a p") #'pi-ide)
(global-set-key (kbd "C-M-a P") #'pi-ide-send-region)

(provide 'pi-conf)
;;; pi-conf.el ends here
