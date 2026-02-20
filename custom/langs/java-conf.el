;;; java-conf.el -- Emacs Java configuration

;; Envieronment variables in use are:
;; - `JAVADOC'
;; - `CATALINA_HOME'

;; The server installation will be in `lsp-java-server-install-dir'
;; Run all tests: `projectile-test-project'

;;-----------------------------------------------------------------------------
;; lsp-java
;;
(use-package lsp-mode
  :straight '(lsp-mode :type git :host github
			                 :repo "emacs-lsp/lsp-mode" :branch "master"))

(use-package lsp-java
  :straight '(lsp-java :type git :host github
			                 :repo "emacs-lsp/lsp-java" :branch "master")
  :after lsp
  :config (progn (add-hook 'java-mode-hook 'lsp)
                 (custom-set-variables '(lsp-ui-sideline-enable nil)
                                       '(lsp-ui-doc-enable nil))))

(defun k/java-mode-hook ()
  (my-coding-hook)
  (c-set-offset 'arglist-intro '+)
  (define-key java-mode-map (kbd "C-x C-e") #'jshell-eval-last-expr)
  (define-key java-mode-map (kbd "M-e") #'jshell-eval-region)
  (define-key java-mode-map (kbd "C-M-a") nil))

(add-hook 'java-mode-hook #'k/java-mode-hook)
(add-hook 'java-mode-hook #'lsp)

;;-----------------------------------------------------------------------------
;; jshell
;;
(require 'shell-conf)

(defun jshell-get-buffer ()
  (save-window-excursion
    (let ((vterm-buffer (or (-find (lambda (b) (equal (buffer-name b)
                                                 "*eshell*"))
                                   (buffer-list))
                            (prog1
                                (k/shell)
                              (with-current-buffer "*eshell*"
                                (eshell-return-to-prompt)
                                (insert "jshell")
                                (eshell-send-input))))))
      vterm-buffer)))

(defun jshell ()
 (interactive)
 (pop-to-buffer (jshell-get-buffer)))

(defun jshell-send (data)
  (interactive)
  (with-current-buffer (jshell-get-buffer)
    (insert data)
    (eshell-send-input)))

(defun jshell-eval-last-expr ()
  (interactive)
  (cl-multiple-value-bind
      (start end)
      (k/scala-get-last-scala-expr)
    (k/scala-flash-region start end)
    (jshell-send (buffer-substring start end))))

(defun jshell-eval-region (start end)
  (interactive "r")
  (let ((data (trim-string
               (buffer-substring-no-properties start end))))
    (k/scala-flash-region start end)
    (jshell-send data)))

;;--------------------------------------------------------------------
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

;; "mvn archetype:generate -DarchetypeGroupId=org.apache.maven.archetypes -DarchetypeArtifactId=maven-archetype-simple"

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

;;--------------------------------------------------------------------
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

;;--------------------------------------------------------------------
;; jflex-mode
;;
(autoload 'jflex-mode "jflex-mode" nil t)
(setq auto-mode-alist (cons '("\\(\\.flex\\|\\.jflex\\)\\'" . jflex-mode)
                            auto-mode-alist))

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
