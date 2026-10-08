;;; java-conf.el -- Emacs Java configuration  -*- lexical-binding: t -*-

(require 's)
(require 'treesit)

;;--------------------------------------------------------------------
;; java-ts-mode, turned on in tree-sitter-conf
;;
;; `java-ts-mode' paints identifiers passed as arguments with the face of a
;; declaration, and every method call receiver as a variable, classes too
;; (FormatUtils.format).  Give arguments the face of a variable use, and
;; capitalized receivers the face of a type, as cc-mode does; constants
;; (MAX_SIZE, LOG) stay constants.  Parameters get a face of their own,
;; plain by default, to stand apart from the variables declared in the body.
;; Not k/: a tree-sitter query takes no "/" in a capture name.
(defface k-java-ts-parameter-face '((t))
  "Face for the parameters of methods, constructors, lambdas and catch.")

(defun k/java-ts-font-lock-setup ()
  "Correct the faces `java-ts-mode' gives parameters, arguments, receivers."
  (setq-local treesit-font-lock-settings
              (append treesit-font-lock-settings
                      (treesit-font-lock-rules
                       :language 'java
                       :override t
                       :feature 'definition
                       '((formal_parameter
                          name: (identifier) @k-java-ts-parameter-face)
                         (spread_parameter
                          (variable_declarator
                           name: (identifier) @k-java-ts-parameter-face))
                         (catch_formal_parameter
                          name: (identifier) @k-java-ts-parameter-face))
                       ;; Record components are fields rather: back to
                       ;; the face of a declaration.
                       :language 'java
                       :override t
                       :feature 'definition
                       '((record_declaration
                          parameters:
                          (formal_parameters
                           [(formal_parameter
                             name: (identifier) @font-lock-variable-name-face)
                            (spread_parameter
                             (variable_declarator
                              name: (identifier)
                              @font-lock-variable-name-face))])))
                       :language 'java
                       :override t
                       :feature 'expression
                       '(((argument_list
                           (identifier) @font-lock-variable-use-face)
                          (:match? "\\`[a-z]" @font-lock-variable-use-face))
                         ((method_invocation
                           object: (identifier) @font-lock-type-face)
                          (:match? "\\`[A-Z][a-z]" @font-lock-type-face))))))
  (treesit-font-lock-recompute-features))

(add-hook 'java-ts-mode-hook #'k/java-ts-font-lock-setup)

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
