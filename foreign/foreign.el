;;; foreign.el -- External text-manipulation programs wrapper

;; Author: Kostafey <kostafey@gmail.com>
;; Keywords: text-manipulation, performance

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;; This file is not part of GNU Emacs.

;;; Commentary:

;; See docstrings for the following functions:

;; * foreign-format-json
;; * foreign-format-xml
;; * foreign-replace
;; * foreign-find

(require 'functions)

(defcustom foreign-preserve-edit-history t "")

(defvar foreign-bin-path (concat-path "~/.emacs.d" "foreign"))
(defvar module-file-path (concat-path foreign-bin-path "foreign-fmt.so"))

(defun module-reload (module)
  "Reload existing module."
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)
    tmpfile))

(defun foreign-ensure-so (filename)
  (when (not (file-exists-p module-file-path))
    (let ((default-directory foreign-bin-path))
      (shell-command
       (concat "make " filename)))))

(progn
  (foreign-ensure-so "foreign-fmt.so")
  ;; (module-reload (concat-path foreign-bin-path "foreign-fmt.so"))
  (module-load (concat-path foreign-bin-path "foreign-fmt.so")))

(defun foreign-format-file (foreign-src mode)
  (when (and
         (buffer-modified-p)
         (yes-or-no-p "Buffer modified. Save it?"))
    (save-buffer))
  (funcall foreign-src (buffer-file-name))
  (if foreign-preserve-edit-history
      (revert-buffer nil t)
      (revert-buffer-hard))
  (funcall mode)
  (message "Done."))

(defun foreign-format-json ()
  "Pretty print json file."
  (interactive)
  (foreign-format-file 'foreign-jsonpp-go 'js-mode))

(defun foreign-format-xml ()
  "Pretty print xml file."
  (interactive)
  (foreign-format-file 'foreign-xmlpp-go 'xml-mode))

(defun foreign-format-edn ()
  "Pretty print edn file."
  (interactive)
  (foreign-format-file 'foreign-ednpp-go 'clojure-mode))

(defun foreign-replace (old-string new-string)
  "Replace string in file."
  (interactive
   (list
    (read-from-minibuffer "Replace string: "
                          "" nil nil 'foreign-replace-old-history)
    (read-from-minibuffer (concat "Replace with: ")
                          "" nil nil 'foreign-replace-new-history)))
  (foreign-replace-go (buffer-file-name)
                      old-string
                      new-string)
  (revert-buffer-hard)
  (message "Done."))

(defun foreign-find (search-string)
  "Find string in file."
  (interactive
   (list
    (read-from-minibuffer "Find string: "
                          "" nil nil 'foreign-find-history)))
  (let ((pos (foreign-find-go (buffer-file-name)
                              (number-to-string (point))
                              search-string)))
    (goto-char (+ pos 1)))
  (message "Done."))

(provide 'foreign)
