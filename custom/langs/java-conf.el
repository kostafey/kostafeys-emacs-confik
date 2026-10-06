;;; java-conf.el -- Emacs Java configuration  -*- lexical-binding: t -*-

;; Envieronment variables in use are:
;; - `JAVADOC'
;; - `CATALINA_HOME'

;;--------------------------------------------------------------------
;; maven
;;
;; "mvn archetype:generate -DarchetypeGroupId=org.apache.maven.archetypes -DarchetypeArtifactId=maven-archetype-simple"
;;
(defmacro maven-def-task (name command)
  `(defun ,name ()
     (interactive)
     (cd (project-root (project-current t)))
     (compile ,command t)))

(maven-def-task maven-compile "mvn compile")
(maven-def-task maven-install "mvn install")
(maven-def-task maven-clean   "mvn clean")
(maven-def-task maven-package "mvn package")

;;--------------------------------------------------------------------
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
      (cl-loop for pos = (search-forward-regexp regexp nil t)
	        while pos collect (let ((modifier (match-string 2))
				                    (type (match-string 4))
				                    (name (match-string 6)))
				                (list modifier type name))))))

(defun java-generate-getters-setters (&rest modifiers)
  (interactive)
  (let ((oldpoint (point)))
    (insert
     (mapconcat (lambda (var) (apply 'make-class-getter-setter (cdr var)))
                (apply 'extract-class-variables modifiers)
                "\n"))
    (c-indent-region oldpoint (point) t)))

(defalias 'java-create-getters-setters 'java-generate-getters-setters)

;;--------------------------------------------------------------------
;; java-decompiler
;;
;; mvn org.apache.maven.plugins:maven-dependency-plugin:get \
;;   -Dartifact=org.benf:cfr:0.139
;;
(use-package jdecomp
  :straight '(jdecomp :type git :host github
			                :repo "xiongtx/jdecomp" :branch "master"))

(let ((home (if (eq system-type 'windows-nt)
                (s-replace-all
                 (list (cons "\\" "/"))
                 (concat (getenv "HOMEDRIVE") (getenv "HOMEPATH")))
              (file-truename "~"))))
  (customize-set-variable
   'jdecomp-decompiler-paths
   (list
    (cons 'cfr (concat
                home
                "/.m2/repository/org/benf/cfr/0.139/cfr-0.139.jar")))))

(customize-set-variable 'jdecomp-decompiler-type 'cfr)

(defun toggle-java-decompile-mode ()
  (interactive)
  (let ((enable (if jdecomp-mode -1 1)))
    (jdecomp-mode enable)
    (message (format
              "java decompile mode %s."
              (propertize (if jdecomp-mode "enabled" "disabled")
                          'face 'font-lock-keyword-face)))))

(jdecomp-mode 1)

(when (not (executable-find "file"))
  (defun jdecomp--jar-p (file)
    "Return t if FILE is a JAR."
    (s-ends-with? ".jar" file)))

(provide 'java-conf)
