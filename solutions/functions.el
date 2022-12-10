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
      (if folder
          (setq path (expand-file-name folder path))))
    path))

(defun my-get-default-directory ()
  (let* ((full-home-path (expand-file-name "~")))
    (if (equal
         (substring default-directory 0 (length full-home-path))
         full-home-path)
        (concat "~" (substring default-directory (length full-home-path)))
      default-directory)))

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

(defun escape-quotes (txt)
  (replace-regexp-in-string "\"" "\\\\\"" txt))

(defun replace-newlines (txt)
  "Replace (kbd c-q c-j) with \n"
  (replace-regexp-in-string "
" "\\\\n" txt))

(defun prepare-string-to-shell (txt)
  (add-quotes (replace-newlines (escape-quotes txt))))

(defun revert-buffer-hard (&optional buffer)
  "Revert buffer without keeping it's history (for perfomance)."
  (let ((buff (or buffer (current-buffer)))
        (file-name (buffer-file-name )))
    (kill-buffer buff)
    (find-file file-name)))

(provide 'functions)
