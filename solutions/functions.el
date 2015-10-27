;;; functions.el -- The collection of misc elisp helper functions.

;;; Copyright © 2013-2015 - Kostafey <kostafey@gmail.com>

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
                     (expand-file-name ".." path)
                   (expand-file-name folder path))))
    path))

(defun force-symbol-name (some-symbol)
  "Return lisp symbol `some-symbol' as a string at all costs!"
  (mapconcat 'char-to-string
             (string-to-list (symbol-name some-symbol)) ""))

(defun strip-text-properties(txt)
  "Remove text properties from the string."
  (set-text-properties 0 (length txt) nil txt)
      txt)

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun add-quotes (str)
  (concat "\"" str "\""))

(defun replace-newlines (txt)
  "Replace (kbd c-q c-j) with \n"
  (replace-regexp-in-string "
" "\\\\n" txt))

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
