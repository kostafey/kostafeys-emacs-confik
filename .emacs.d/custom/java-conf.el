;;; java-conf.el

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'my-indent-setup)
;;-----------------------------------------------------------------------------

;;=============================================================================
(add-to-list 'load-path (concat site-lisp-path "jdee-2.4.0.1/lisp/"))
(require 'jde)

(setq bsh-jar "C:/Documents and Settings/KGSedykh/Application Data/.emacs.d/jdee-2.4.0.1/java/lib/bsh.jar")

;;=============================================================================

(add-to-list 'load-path (concat site-lisp-path "emacs-groovy-mode_2011-06-29/"))

(autoload 'groovy-mode "groovy-mode"
  "Mode for editing groovy source files" t)
(setq auto-mode-alist
      (append '(("\\.groovy$" . groovy-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("groovy" . groovy-mode))
                                     interpreter-mode-alist))

(autoload 'groovy-mode "groovy-mode" "Groovy mode." t)
(autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
(autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")

(add-hook 'groovy-mode-hook
          '(lambda ()
             (inf-groovy-keys)
             ))

;; can set groovy-home here, if not in environment
(setq inferior-groovy-mode-hook
      '(lambda()
         (setq groovy-home "C:/Progra~1/Groovy/Groovy~1/")
         ))


;;; make Groovy mode electric by default.
;; (add-hook 'groovy-mode-hook
;;           '(lambda ()
;;              (require 'groovy-electric)
;;              (groovy-electric-mode)))



(provide 'java-conf)