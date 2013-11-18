;;; java-conf.el

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'my-indent-setup)

;;=============================================================================
;; beanshell
;;
(require 'beanshell)
(setq bsh-jar (find-file-in-load-path "bsh-2.0b4.jar"))

;; skeeto/javadoc-lookup
;; Path example: `~/Java/jdk/docs/api/'
(let ((javadoc-env (getenv "JAVADOC")))
  (when javadoc-env
    (apply 'javadoc-add-roots (split-string javadoc-env ";"))))

;; skeeto/ant-project-mode

;;=============================================================================
;; eclim
;;
;; (require 'eclim)
;; (global-eclim-mode)
;; (require 'eclimd)
;; (custom-set-variables
;;  '(eclim-eclipse-dirs '("/opt/eclipse_64")))

;; (setq help-at-pt-display-when-idle t)
;; (setq help-at-pt-timer-delay 0.1)
;; (help-at-pt-set-timer)

;; ;; add the emacs-eclim source
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)
;;
;;=============================================================================

(provide 'java-conf)
