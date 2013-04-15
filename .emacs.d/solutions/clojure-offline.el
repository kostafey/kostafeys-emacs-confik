;;; clojure-offline.el -- The set of functions to help use clojure offline.

;;; Copyright © 2013 - Kostafey <kostafey@gmail.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;;; --------------------------------
;;; How to work with clojure offline
;;; --------------------------------
;;
;; Short story
;; ===========
;;
;; `clj-off-create-script'
;; e.g. (clj-off-create-script [[ring/ring-core "1.1.8"]])
;;
;; Long story
;; ==========
;;
;; 1. Install leiningen
;; --------------------
;;
;; You can download `lein' script as usal from `leiningen.org' and place it
;; somewhere in the `PATH' environment variable.
;;
;; The actual location of the `leiningen-%LEIN_VERSION%-standalone.jar' is holds
;; in the `LEIN_JAR_URL' variable in the lein script (or can be seen in the
;; error log when you run lein self-install). Download and place this jar in the
;; %HOME%\.lein\self-installs\
;;
;; To ensure lein is installed correctly you can run `lein' from the shell.
;;
;; 2. Install lein-localrepo (leiningen plugin)
;; --------------------------------------------
;;
;; Download `lein-localrepo-<version>.jar' file. The probable location of the
;; file can be obtained by evaluating the following elisp script:
;;
;; (clj-off-guess-clojars-url [lein-localrepo "0.4.1"])
;;
;; Place it to the %HOME%\.m2\repository\<group-id>\<artifact-id>\<version>\
;; folder, e.g. %HOME%\.m2\repository\lein-localrepo\lein-localrepo\0.4.1\
;; Create the `lein-localrepo-0.4.1.pom' file in the same folder.
;;
;; Install as a global plugin in ~/.lein/profiles.clj:
;;
;; {:user {:plugins [[lein-localrepo "0.4.1"]]}}
;;
;; To ensure lein-localrepo is installed correctly you can run the shell:
;;
;; lein localrepo help
;;
;; 3. Download and install all required libraries
;; ----------------------------------------------
;;
;; Select (mark) :dependencies vector in the defproject expression of the
;; `project.clj' file, like here (selection is marked by # symbols):
;;
;; :dependencies #[[org.clojure/clojure "1.5.1"]
;;                 [compojure "1.1.5"]
;;                 [me.raynes/laser "1.1.1"]
;;                 [mysql/mysql-connector-java "5.1.24"]
;;                 [korma "0.3.0-RC5"]
;;                 [lib-noir "0.4.9"]]#
;;
;; Run M-x clj-off-create-script RET RET
;;
;; WARNING!
;;
;; There is no guarantee that the file is located in the printed path.
;; E.g. https://clojars.org/repo/org/clojure/clojure/1.5.1/clojure-1.5.1.jaris
;; a wrong path since clojure-1.5.1.jar is not hosted in the clojars.org
;; (yet). So, you should find it manually. But if the jar is hosted in the
;; clojars.org, the printed url is likely correct.
;;
;; Repeat this for :plugins and :dev :dependencies sections.
;;
;; 4. Create *.pom files in all apropriate .m2 subfolders
;; ------------------------------------------------------
;;
;; Try to run M-x nrepl-jack-in RET
;;
;; ?. Capture the universe
;  -----------------------
;; TODO:
;; (clj-off-guess-clojars-url [org.clojure/tools.nrepl "0.2.2"])
;;
;; http://search.maven.org/remotecontent?filepath=org/clojure/tools.nrepl/0.2.2/tools.nrepl-0.2.2.jar
;;
;; (clj-off-create-script [[leinjacker "0.4.1"]])
;;
;; for run `lein repl'
;; (clj-off-create-script ["org.thnetos:cd-client:pom:0.3.6"])
;;
;; 5. Correct errors
;; -----------------
;;
;; error in process sentinel:
;; Could not start nREPL server:
;; Could not transfer artifact org.clojure:tools.nrepl:pom:0.2.1 from/to central
;; (http://repo1.maven.org/maven2/): repo1.maven.org
;; Could not transfer artifact clojure-complete:clojure-complete:pom:0.2.2
;; from/to central (http://repo1.maven.org/maven2/): repo1.maven.org
;; This could be due to a typo in :dependencies or network issues.
;;
;; (clj-off-create-script ["org.clojure:tools.nrepl:pom:0.2.1"
;;                         "clojure-complete:clojure-complete:pom:0.2.2"])
;;


(defvar clojure-offline-script-buffer-name "*clojure-offline*")

(defun force-symbol-name (some-symbol)
  "Return lisp symbol `some-symbol' as a string at all costs!"
  (mapconcat 'char-to-string
             (string-to-list (symbol-name some-symbol)) ""))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clj-off-parse-artifact (artifact-name)
  "Parse `artifact-name' to list (`group-id' `artifact-id' `version')
Input formats, e.g.:
 [org.clojure/clojure \"1.5.1\"]
 \"[org.clojure/clojure \"1.5.1\"]\"
 org.clojure:clojure:pom:1.5.1
Ouptut format, e.g.:
 (\"org.clojure\" \"clojure\" \"1.5.1\")"
  ;; lein format as string e.g. "[org.clojure/clojure \"1.5.1\"]"
  (let* ((artifact-name
          (if (and (not (vectorp artifact-name))
                   (stringp artifact-name)
                   (equal (substring (trim-string artifact-name) 0 1) "["))
              (read (trim-string artifact-name))
            artifact-name)))
    (let* ((is-lein-format (vectorp artifact-name))
           artifact-list
           group-and-artifact
           group-id
           artifact-id
           version)
      (if is-lein-format
          ;; lein format, e.g. [org.clojure/clojure "1.5.1"]
          (progn
            (setq group-and-artifact
                  (split-string (force-symbol-name (elt artifact-name 0))
                                "/"))
            (setq group-id (nth 0 group-and-artifact))
            (setq artifact-id (if (> (length group-and-artifact) 1)
                                  (nth 1 group-and-artifact)
                                (nth 0 group-and-artifact)))
            (setq version (elt artifact-name 1)))
        ;; maven format, e.g. "org.clojure:clojure:pom:1.5.1"
        (progn
          (setq artifact-list (split-string artifact-name ":"))
          (setq group-id (nth 0 artifact-list))
          (setq artifact-id (nth 1 artifact-list))
          (setq version (nth 3 artifact-list))))
      (list group-id artifact-id version))))

;;;###autoload
(defmacro clj-off-with-artifact (artifact-name &rest body)
  "Inject `group-id' `artifact-id' `version' local variables to the `body'
scope."
  `(let* ((artifact (clj-off-parse-artifact ,artifact-name))
          (group-id (nth 0 artifact))
          (artifact-id (nth 1 artifact))
          (version (nth 2 artifact)))
     ,@body))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clj-off-get-jar-urls (artifact-name)
  "Convert from maven's `artifact-name' to probably jar url location on
clojars.
E.g. convert from
lein-ring:lein-ring:pom:0.8.2
to
https://clojars.org/repo/lein-ring/lein-ring/0.8.2/lein-ring-0.8.2.jar"
  (clj-off-with-artifact
   artifact-name
   (let ((art-path (concat (mapconcat 'identity 
                                      (split-string group-id "\\.") "/") "/"
                           artifact-id "/" version "/"
                           artifact-id "-" version ".jar")))
     (concat "https://clojars.org/repo/" art-path "\n"
             "http://repo1.maven.org/maven2/" art-path))))

(defun clj-off-get-list-clojars-url (artifact-names-array)
  (map 'list 'clj-off-get-jar-urls artifact-names-array))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clj-off-get-localrepo-install (artifact-name)
  "Convert from maven's `artifact-name' to local maven repository creation
script.
lein localrepo install <filename> <[groupId/]artifactId> <version>

E.g. convert from
lein-ring:lein-ring:pom:0.8.2
to
lein localrepo install foo-1.0.6.jar com.example/foo 1.0.6"
  (clj-off-with-artifact
   artifact-name
   (let ((jar-file-name ))
     (concat "lein localrepo install "
             artifact-id "-" version ".jar "
             group-id "/" artifact-id " " version))))

(defun clj-off-get-list-localrepo-install (artifact-names-array)
  (map 'list 'clj-off-get-localrepo-install artifact-names-array))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clj-off-get-mvn-deploy (artifact-name)
  "Convert from maven's `artifact-name' to local maven repository creation
script.
E.g. convert from
lein-ring:lein-ring:pom:0.8.2
to
mvn deploy:deploy-file -DgroupId=lein-ring -DartifactId=lein-ring \
    -Dversion=0.8.2 -Dpackaging=jar -Dfile=lein-ring-0.8.2.jar \
    -Durl=file:maven_repository"
  (clj-off-with-artifact
   artifact-name
   (concat "mvn deploy:deploy-file "
           "-DgroupId=" group-id " "
           "-DartifactId=" artifact-id " "
           "-Dversion=" version " "
           "-Dpackaging=" "jar" " "
           "-Dfile=" artifact-id "-" version ".jar" " "
           "-Durl=" "file:maven_repository")))

(defun clj-off-get-list-mvn-deploy (artifact-names-array)
  (map 'list 'clj-off-get-mvn-deploy artifact-names-array))

;;----------------------------------------------------------------------
;;
;;;###autoload
(defun clj-off-guess-clojars-url (artifact-name)
  (interactive
   (list
    (read-from-minibuffer "Clojure artifact: "
                          (buffer-substring (mark) (point)) nil nil
                          'clj-off-artifact-name-history)))
  (message (clj-off-get-jar-urls artifact-name)))

(defun clj-off-create-script-buffer ()
  "Create buffer dedicated to output configure required clojure jars."
  (let ((buf (get-buffer-create clojure-offline-script-buffer-name)))
    (save-excursion
      (set-buffer buf)
      (toggle-truncate-lines nil)
      buf)))

;;;###autoload
(defun clj-off-create-script (artifact-names-array &optional install clear)
  "
`install': nil, 'maven
`clear': nil, t"
  (interactive
   (list
    (read-from-minibuffer "Clojure artifacts list: "
                          (buffer-substring (mark) (point)) nil nil
                          'clj-off-artifacts-list-history)))
  (let ((artifact-names-array (if (and (not (vectorp artifact-names-array))
                                       (stringp artifact-names-array))
                                  (read (trim-string artifact-names-array))
                                artifact-names-array)))
    (set-buffer (clj-off-create-script-buffer))
    (if (equal clear nil)
        (insert "\n\n")
      (erase-buffer))
    (mapc
     (lambda (art) (mapc
               (lambda (a) (insert (concat "wget " a "\n")))
               (split-string art)))
     (clj-off-get-list-clojars-url artifact-names-array))
    (insert "\n")
    (if (and (equal install nil) (equal install 'maven))
        (progn
          (mapc (lambda (art) (insert (concat art "\n")))
                (clj-off-get-list-mvn-deploy artifact-names-array)))
      (progn
        (mapc (lambda (art) (insert (concat art "\n")))
              (clj-off-get-list-localrepo-install artifact-names-array))))
    (switch-to-buffer (clj-off-create-script-buffer))))

(provide 'clojure-offline)
