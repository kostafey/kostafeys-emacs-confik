;;; java-conf.el -- Emacs Java configuration

;; Envieronment variables in use are:
;; - `JAVADOC'
;; - `CATALINA_HOME'

;;-----------------------------------------------------------------------------
;; lsp-java
;;
(use-package lsp-mode :ensure t)
(use-package company-lsp :ensure t)

(use-package lsp-java
  :ensure t
  :after lsp
  :config (progn (add-hook 'java-mode-hook 'lsp)
                 (custom-set-variables '(lsp-ui-sideline-enable nil)
                                       '(lsp-ui-doc-enable nil))))

;; The server installation will be in `lsp-java-server-install-dir'
;; For update: (lsp-install-server t)
;;         or: (lsp--install-server-internal ['jdtls] t)

(defun k/java-mode-hook ()
  (my-coding-hook)
  (flycheck-mode t)
  (c-set-offset 'arglist-intro '+))

(add-hook 'java-mode-hook 'k/java-mode-hook)

;;-----------------------------------------------------------------------------
;; beanshell
;;
(require 'beanshell)
(setq bsh-jar (find-file-in-load-path "bsh-2.0b4.jar"))

;;-----------------------------------------------------------------------------
;; skeeto/javadoc-lookup
;;
(require 'javadoc-lookup)
;; Path example: `~/Java/jdk/docs/api/'
(let ((javadoc-env (getenv "JAVADOC")))
  (when javadoc-env
    (apply 'javadoc-add-roots (split-string javadoc-env ";"))))

;;-----------------------------------------------------------------------------
;; maven
;;
(defmacro maven-def-task (name command)
  `(defun ,name ()
     (interactive)
     (cd (projectile-project-root))
     (compile ,command t)))

(maven-def-task maven-tomcat-deploy "mvn tomcat7:redeploy")
(maven-def-task maven-compile "mvn compile")
(maven-def-task maven-install "mvn install")
(maven-def-task maven-clean "mvn clean")
(maven-def-task maven-package "mvn package")
(maven-def-task maven-all "mvn clean package tomcat7:redeploy")

;;-----------------------------------------------------------------------------
;; Inserting getters and setters
;; Based on:
;; `https://www.ecyrd.com/JSPWiki/wiki/InsertingGettersAndSettersInEmacs'
;;
(defun java-getter-setter (type field)
  "Inserts a Java field, and getter/setter methods."
  (interactive "MType: \nMField: ")
  (let ((oldpoint (point))
        (capfield (concat (capitalize (substring field 0 1))
                          (substring field 1))))
    (insert (concat "public " type " get" capfield "()\n"
                    "{\n"
                    "    return this." field ";\n"
                    "}\n\n"
                    "public void set" capfield "(" type " " field ")\n"
                    "{\n"
                    "    this." field " = " field ";\n"
                    "}\n"))
    (c-indent-region oldpoint (point) t)))

(defun make-class-getter-setter (type var)
  (format
   (concat "public %s get%s() { return %s; }\n"
           "public void set%s(%s %s) { this.%s = %s; }\n")
   ;; getter line
   type (upcase-initials var) var
   ;; setter line
   type (upcase-initials var) type var var var))

(defun extract-class-variables (&rest modifiers)
  (let ((regexp
	     (concat
	      "^\\([ \t]*\\)"
          ;; "\\(private\\)?"
	      "\\(" (mapconcat (lambda (m) (format "%S" m)) modifiers "\\|") "\\)"
	      "\\([ \t]*\\)"
	      "\\([A-Za-z0-9<>]+\\)"
	      "\\([ \t]*\\)"
	      "\\([a-zA-Z0-9]+\\);$")))
    (save-excursion
      (goto-char (point-min))
      (loop for pos = (search-forward-regexp regexp nil t)
	        while pos collect (let ((modifier (match-string 2))
				                    (type (match-string 4))
				                    (name (match-string 6)))
				                (list modifier type name))))))

(defun java-generate-getters-setters (&rest modifiers)
  (interactive)
  (let ((oldpoint (point)))
    (insert
     (mapconcat (lambda (var) (apply 'make-class-getter-setter (rest var)))
                (apply 'extract-class-variables modifiers)
                "\n"))
    (c-indent-region oldpoint (point) t)))

(defalias 'java-create-getters-setters 'java-generate-getters-setters)

;;-----------------------------------------------------------------------------
;; JBehave: story-mode
;;
(define-generic-mode story-mode
  '("!--")
  '("Given" "When" "Then" "Narrative" "Meta" "And" "Scenario" "Examples")
  '(("|.*|" . 'font-lock-constant-face)
    ("'.*'" . 'font-lock-string-face))
  '("\\.story$")
  nil
  "Story mode is a minor mode for editing JBehave story files")
(add-to-list 'auto-mode-alist '("\\.story" . story-mode))

;;=============================================================================
;; jflex-mode
;;
(autoload 'jflex-mode "jflex-mode" nil t)
(setq auto-mode-alist (cons '("\\(\\.flex\\|\\.jflex\\)\\'" . jflex-mode) auto-mode-alist))

;;=============================================================================
;; java-decompiler
;;
;; mvn org.apache.maven.plugins:maven-dependency-plugin:2.10:get -Dartifact=org.benf:cfr:0.139
;;
(let ((home (if (eq system-type 'windows-nt)
                (concat (getenv "HOMEDRIVE") (getenv "HOMEPATH"))
              (file-truename "~"))))
  (customize-set-variable
   'jdecomp-decompiler-paths
   (list
    (cons 'cfr (concat
                home
                "/.m2/repository/org/benf/cfr/0.139/cfr-0.139.jar")))))

(customize-set-variable 'jdecomp-decompiler-type 'cfr)
(jdecomp-mode 1)

(provide 'java-conf)
