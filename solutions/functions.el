;;; functions.el -- The collection of misc elisp helper functions.

;;; Copyright © 2013-2014 - Kostafey <kostafey@gmail.com>

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

(defun find-file-in-load-path (search-file-name
                               &optional fail-on-error)
  "Return the full path to SEARCH-FILE-NAME.
SEARCH-FILE-NAME is searching in the emacs `load-path'.
When FAIL-ON-ERROR is t, raise error if nothing found, return nil otherwise."
  (let ((result nil))
    (if search-file-name
        (dolist (path load-path)
          (let ((search-file-path (expand-file-name search-file-name path)))
            (if (file-exists-p search-file-path)
                (setq result search-file-path)))))
    (if (and fail-on-error (not result))
        (error (concat "Can't find file " search-file-name))
      result)))

(defun concat-path (&rest folders)
  "Concatenate list of folders to the path.
E.g.
 (concat-path (getenv \"HOME\") \".m2\" \"repository\")"
  (let ((path))
    (dolist (folder folders)
      (setq path (if (equal ".." folder)
                     (file-name-as-directory (expand-file-name ".." path))
                   (file-name-as-directory (concat path folder)))))
    path))

(defun find-file-recursively (search-file-name
                              &optional start-path fail-on-error)
  "Return the full path to SEARCH-FILE-NAME.
Recursive searching starts from START-PATH.
When FAIL-ON-ERROR is t, raise error if nothing found, return nil otherwise."
  (let ((start-path (if start-path start-path "/"))
        (result nil))
    (if search-file-name
        (dolist (path-entity (directory-files-and-attributes start-path t))
          (if (not result)
              (let ((entity-name (car path-entity)))
                (if (and
                     (cadr path-entity) ; is a directory sign
                     (not (or (equal (file-name-nondirectory entity-name) ".")
                              (equal (file-name-nondirectory entity-name) "..")
                              (equal (substring
                                      (file-name-nondirectory entity-name)
                                      0 2) ".#"))))
                    ;; is a directory
                    (setq result (find-file-recursively
                                  search-file-name
                                  entity-name nil))
                  ;; is a file
                  (if (equal (downcase search-file-name)
                             (downcase (file-name-nondirectory entity-name)))
                      (setq result entity-name)))))))
    (if (and fail-on-error (not result))
        (error (concat "Can't find file " search-file-name)))
    result))

(defun find-file-upwards (search-file-name start-path
                          &optional ascent-depth fail-on-error)
  "Return the full path to `search-file-name' by moving upwards
on the directory tree.
START-PATH is path where searching starts.
ASCENT-DEPTH is a maximim number of '..' acts on the directory tree.
When FAIL-ON-ERROR is t, raise error if nothing found,return nil otherwise."
  (let ((ascent-depth (if ascent-depth ascent-depth 0))
        (result nil))
    (if (and start-path search-file-name)
        (dolist (path-entity (directory-files-and-attributes start-path t))
          (if (not result)
              (let ((entity-name (car path-entity)))
                (if (not
                     (and
                      (cadr path-entity) ; is a directory sign
                      (not (or (equal (file-name-nondirectory entity-name) ".")
                               (equal (file-name-nondirectory entity-name) "..")
                               (equal (substring
                                       (file-name-nondirectory entity-name)
                                       0 2) ".#")))))
                  ;; is a file
                  (if (equal (downcase search-file-name)
                             (downcase (file-name-nondirectory entity-name)))
                      (setq result entity-name)))))))
    (if (and (not result) (> ascent-depth 0))
        (setq result (find-file-upwards search-file-name
                                        (clomacs-concat-path start-path "..")
                                        (1- ascent-depth)
                                        fail-on-error)))
    (if (and fail-on-error (not result))
        (error (concat "Can't find file " search-file-name)))
    result))

(defun force-symbol-name (some-symbol)
  "Return lisp symbol `some-symbol' as a string at all costs!"
  (mapconcat 'char-to-string
             (string-to-list (symbol-name some-symbol)) ""))

(defun strip-text-properties(txt)
  (set-text-properties 0 (length txt) nil txt)
      txt)

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun kostafey-export-keys ()
  "Export my Emacs keybindings configuration."
  (interactive)
  (let* ((keys-file-dir custom-conf-lisp-path)
         (src-filename "key-bindings.org")
         (el-filename "key-bindings.el")
         (md-filename "key-bindings.md")         
         (readme-filename "README.md"))
    (save-restriction
      (save-excursion
        (widen)
        (with-current-buffer
            (find-file-noselect 
             (expand-file-name src-filename keys-file-dir))
          (org-babel-tangle-file 
           buffer-file-name
           (expand-file-name el-filename keys-file-dir) 
           "emacs-lisp")
          (org-md-export-to-markdown nil))
        (rename-file (expand-file-name md-filename keys-file-dir)
                     (expand-file-name readme-filename "~/.emacs.d")
                     t)))))

(provide 'functions)
