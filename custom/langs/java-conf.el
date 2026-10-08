;;; java-conf.el -- Emacs Java configuration  -*- lexical-binding: t -*-

(require 'cl-lib)
(require 's)
(require 'treesit)

;;--------------------------------------------------------------------
;; java-ts-mode, turned on in tree-sitter-conf
;;
;; Imenu nested the way the declarations are: a type holds its fields,
;; enum constants, record components, constructors, methods and nested
;; types.  Constructors and methods are named with their parameter types,
;; to tell overloads apart.  Every name carries the region of its
;; declaration, as eglot's do: Imenu then offers ".." to visit a type
;; itself, and breadcrumb shows the declarations point is inside.  The
;; stock index is flat lists by kind, missing enums, fields, constructors.

(defun k/java-ts-node-text (node)
  "Return the text of NODE on one line, nil if NODE is nil."
  (when node
    (replace-regexp-in-string "[ \t\n\r]+" " " (treesit-node-text node t))))

(defun k/java-ts-child-text (node field)
  "Return the text of the FIELD child of NODE, nil if there is none."
  (k/java-ts-node-text (treesit-node-child-by-field-name node field)))

(defun k/java-ts-children (node &rest types)
  "Return the named children of NODE of one of the node TYPES."
  (when node
    (treesit-filter-child
     node (lambda (child) (member (treesit-node-type child) types)) t)))

(defun k/java-ts-parameters (node)
  "Return the formal parameters of the method, constructor or record NODE.
A compact constructor takes the components of its record."
  (k/java-ts-children
   (or (treesit-node-child-by-field-name node "parameters")
       (when (equal (treesit-node-type node) "compact_constructor_declaration")
         (treesit-node-child-by-field-name
          (treesit-node-parent (treesit-node-parent node)) "parameters")))
   "formal_parameter" "spread_parameter"))

(defun k/java-ts-parameter-name (param)
  "Return the name of the formal parameter PARAM."
  (k/java-ts-child-text
   (if (equal (treesit-node-type param) "spread_parameter")
       (car (k/java-ts-children param "variable_declarator"))
     param)
   "name"))

(defun k/java-ts-parameter-type (param)
  "Return the type of the formal parameter PARAM: \"int[]\", \"String...\".
Package names are left out: java.util.List<String> is List<String>."
  (replace-regexp-in-string
   "\\_<\\(?:[a-z_][[:alnum:]_]*\\.\\)+" ""
   (if (equal (treesit-node-type param) "spread_parameter")
       (concat (k/java-ts-node-text
                (seq-find (lambda (child)
                            (not (member (treesit-node-type child)
                                         '("modifiers" "variable_declarator"
                                           "line_comment" "block_comment"))))
                          (treesit-node-children param t)))
               "...")
     (concat (k/java-ts-child-text param "type")
             (k/java-ts-child-text param "dimensions")))))

(defun k/java-ts-imenu-entry (name node &optional target)
  "Return the Imenu entry NAME for the declaration NODE, nil without NAME.
TARGET is the list of member entries or the position to visit, the start
of NODE by default.  NAME is marked with the region of NODE."
  (when name
    (let ((region (cons (treesit-node-start node) (treesit-node-end node))))
      (cons (propertize name 'imenu-region region 'breadcrumb-region region)
            (or target (treesit-node-start node))))))

(defun k/java-ts-imenu-self (node)
  "Return the Imenu entry visiting the type NODE, to put among its members.
`consult-imenu' lists no type that has members otherwise.  The entry is
named by the keyword of the type, its empty region keeps breadcrumb from
showing it."
  (let* ((start (treesit-node-start node))
         (region (cons start (1- start))))
    (cons (propertize (pcase (treesit-node-type node)
                        ("interface_declaration" "interface")
                        ("enum_declaration" "enum")
                        ("record_declaration" "record")
                        ("annotation_type_declaration" "@interface")
                        (_ "class"))
                      'imenu-region region 'breadcrumb-region region)
          start)))

(defun k/java-ts-imenu-members (node)
  "Return the Imenu entries for the declarations directly in NODE."
  (when node
    (delq nil (mapcan #'k/java-ts-imenu-entries
                      (treesit-node-children node t)))))

(defun k/java-ts-imenu-entries (node)
  "Return the Imenu entries for the declaration NODE, a list."
  (pcase (treesit-node-type node)
    ((or "class_declaration" "interface_declaration" "enum_declaration"
         "record_declaration" "annotation_type_declaration")
     (let ((members
            (delq nil
                  (append
                   (mapcar (lambda (param)
                             (k/java-ts-imenu-entry
                              (k/java-ts-parameter-name param) param))
                           (and (equal (treesit-node-type node)
                                       "record_declaration")
                                (k/java-ts-parameters node)))
                   (k/java-ts-imenu-members
                    (treesit-node-child-by-field-name node "body"))))))
       (list (k/java-ts-imenu-entry
              (k/java-ts-child-text node "name") node
              (and members (cons (k/java-ts-imenu-self node) members))))))
    ((or "field_declaration" "constant_declaration")
     (mapcar (lambda (declarator)
               (k/java-ts-imenu-entry (k/java-ts-child-text declarator "name")
                                      node (treesit-node-start declarator)))
             (k/java-ts-children node "variable_declarator")))
    ((or "method_declaration" "constructor_declaration"
         "compact_constructor_declaration"
         "annotation_type_element_declaration")
     (when-let* ((name (k/java-ts-child-text node "name")))
       (list (k/java-ts-imenu-entry
              (propertize (format "%s(%s)" name
                                  (mapconcat #'k/java-ts-parameter-type
                                             (k/java-ts-parameters node) ", "))
                          'k/breadcrumb-name name)
              node))))
    ("enum_constant"
     (list (k/java-ts-imenu-entry (k/java-ts-child-text node "name") node)))
    ("enum_body_declarations"
     (k/java-ts-imenu-members node))))

(defun k/java-ts-imenu ()
  "Return the Imenu index of a `java-ts-mode' buffer."
  (k/java-ts-imenu-members (treesit-buffer-root-node 'java)))

(defun k/java-ts-imenu-setup ()
  "Index the buffer with `k/java-ts-imenu'."
  (setq-local imenu-create-index-function #'k/java-ts-imenu))

(add-hook 'java-ts-mode-hook #'k/java-ts-imenu-setup)

;; Breadcrumb names methods without their parameters, which take up the
;; header line; `breadcrumb-jump' still lists them in full, from Imenu.
(defun k/breadcrumb-short-name (args)
  "Put the `k/breadcrumb-name' of an Imenu crumb in its place, if it has one.
ARGS are those of `breadcrumb--format-ipath-node': the crumb, and more."
  (if-let* ((name (get-text-property 0 'k/breadcrumb-name (car args))))
      (cons name (cdr args))
    args))

(with-eval-after-load 'breadcrumb
  (advice-add 'breadcrumb--format-ipath-node :filter-args
              #'k/breadcrumb-short-name))

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
    (indent-region oldpoint (point))))

(defun make-class-getter-setter (type var)
  (format
   (concat "public %s get%s() { return %s; }\n"
           "public void set%s(%s %s) { this.%s = %s; }\n")
   ;; getter line
   type (upcase-initials var) var
   ;; setter line
   (upcase-initials var) type var var var))

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
    (indent-region oldpoint (point))))

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
