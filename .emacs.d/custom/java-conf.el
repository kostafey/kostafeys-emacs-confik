;;; java-conf.el

;; Envieronment variables in use are:
;; - `JAVADOC'
;; - `CATALINA_HOME'

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'my-indent-setup)

;;=============================================================================
;; beanshell
;;
(require 'beanshell)
(setq bsh-jar (find-file-in-load-path "bsh-2.0b4.jar"))
;;-----------------------------------------------------------------------------
;; skeeto/javadoc-lookup
(require 'javadoc-lookup)
;; Path example: `~/Java/jdk/docs/api/'
(let ((javadoc-env (getenv "JAVADOC")))
  (when javadoc-env
    (apply 'javadoc-add-roots (split-string javadoc-env ";"))))
;;-----------------------------------------------------------------------------
;; skeeto/ant-project-mode
;; TODO: add
;;-----------------------------------------------------------------------------
;; maven
(defmacro maven-def-task (name command)
  `(defun ,name ()
     (interactive)
     (cd (projectile-project-root))
     (compile ,command t)))

(maven-def-task maven-tomcat-deploy "mvn tomcat7:redeploy")
(maven-def-task maven-compile "mvn compile")
(maven-def-task maven-clean "mvn clean")
(maven-def-task maven-package "mvn package")

;;-----------------------------------------------------------------------------
;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-markup-indent-offset 4)

;;=============================================================================
;; tomacat
(defvar tomcat-process nil "Process running tomcat")
(defvar tomcat-buffer nil "Buffer containing tomcat-process")
(defvar tomcat-is-running nil "Is tomcat running ?")

(defvar tomcat-script
  (let* ((catalina-home (getenv "CATALINA_HOME"))
         ;; or set `default-directory' variable
         (catalina-bin (concat-path catalina-home "bin")))
    (expand-file-name (if (eq system-type 'windows-nt) 
                          "catalina.bat"
                        "catalina.sh")
                      catalina-bin))
  "Script to start or stop tomcat")

(defun tomcat-start ()
  (switch-to-buffer "*tomcat*")
  (setq tomcat-buffer (current-buffer))
  (erase-buffer)
  (log4j-mode)
  (setq tomcat-process
        (start-process "tomcat" (current-buffer)
                       tomcat-script "run"))
  (setq tomcat-is-running t)
  (beginning-of-buffer)
  (message "Tomcat started."))

(defun tomcat-stop ()
  (message "Stopping Tomcat ...")
  (save-excursion
    (switch-to-buffer "*tomcat*")
    (goto-char (point-max)))
  (call-process tomcat-script nil "*tomcat-stop*" t "stop")
  (setq tomcat-is-running nil)
  (kill-buffer "*tomcat-stop*")
  (message "Tomcat stopped"))

(defun tomcat-toggle()
  "Stop or start Tomcat"
  (interactive)
  (if tomcat-is-running
      (tomcat-stop)
    (tomcat-start)))

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
