;;; functions.el -- The collection of misc elisp helper functions.

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

(defun find-file-in-load-path (search-file-name &optional fail-on-error)
  "Return the full path to `file-name'.
`file-name' is searching in the emacs `load-path'."
  (let ((result nil))
    (dolist (path load-path)
      (let ((search-file-path (expand-file-name search-file-name path)))
        (if (file-exists-p search-file-path)
            (setq result search-file-path))))
    (if (and fail-on-error (not result))
        (error (concat "Can't find file " search-file-name))
      result)))

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

(defun concat-path (&rest folders)
  "Concatenate list of folders to the path.
E.g. 
 (concat-path (getenv \"HOME\") \".m2\" \"repository\")"
  (let ((path))
    (dolist (folder folders)
      (setq path (file-name-as-directory (concat path folder))))
    path))

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
                     (expand-file-name readme-filename "~/")
                     t)))))

(provide 'functions)
