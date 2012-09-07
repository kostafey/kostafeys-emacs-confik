;;; java-conf.el

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'my-indent-setup)
;;-----------------------------------------------------------------------------

;;=============================================================================

(add-to-list 'load-path (concat site-lisp-path "jdee-2.4.0.1/lisp/"))
;; (add-to-list 'load-path (concat site-lisp-path "ejsql/"))
(require 'beanshell)

(setq bsh-jar "C:/Documents and Settings/KGSedykh/Application Data/.emacs.d/jdee-2.4.0.1/java/lib/bsh.jar")




(provide 'java-conf)
