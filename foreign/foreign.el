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

(defvar foreign-bin-path (concat-path "~/.emacs.d" "foreign"))

(defun foreign-get-go-executable (src-go-file)
  (let* ((src-go-path (concat-path foreign-bin-path src-go-file))
         (bin-go-path (substring src-go-path 0 (- (length src-go-path) 3)))
         (bin-go-path (if (eq system-type 'windows-nt)
                          (concat bin-go-path ".exe")
                        bin-go-path)))
    (if (not (file-exists-p bin-go-path))
        (shell-command
         (concat "go build -o " bin-go-path " " src-go-path"\"")))
    bin-go-path))

(defun foreign-run-for-file (foreign-src file-name &rest args)
  (let* ((bin (foreign-get-go-executable foreign-src))
         (command (concat bin " " file-name
                          (if args
                              (concat " "
                                      (cl-reduce
                                       (lambda (x y) (concat x " " y)) args))
                            ""))))
    (shell-command-to-string command)))

(defun foreign-format-file (foreign-src mode)
  (when (and
         (buffer-modified-p)
         (yes-or-no-p "Buffer modified. Save it?"))
    (save-buffer))
  (foreign-run-for-file foreign-src (buffer-file-name))
  (revert-buffer-hard)
  (funcall mode)
  (message "Done."))

(defun foreign-format-json ()
  "Pretty print json file."
  (interactive)
  (foreign-format-file "jsonpp.go" 'js-mode))

(defun foreign-format-xml ()
  "Pretty print xml file."
  (interactive)
  (foreign-format-file "xmlpp.go" 'xml-mode))

(defun foreign-replace (old-string new-string)
  "Replace string in file."
  (interactive
   (list
    (read-from-minibuffer "Replace string: "
                          "" nil nil 'foreign-replace-old-history)
    (read-from-minibuffer (concat "Replace with: ")
                          "" nil nil 'foreign-replace-new-history)))
  (foreign-run-for-file "replace.go"
                        (buffer-file-name)
                        (prepare-string-to-shell old-string)
                        (prepare-string-to-shell new-string))
  (revert-buffer-hard)
  (message "Done."))

(defun foreign-find (search-string)
  "Find string in file."
  (interactive
   (list
    (read-from-minibuffer "Find string: "
                          "" nil nil 'foreign-find-history)))
  (let ((pos (string-to-number
              (foreign-run-for-file "find.go"
                                    (buffer-file-name)
                                    (number-to-string (point))
                                    (prepare-string-to-shell search-string)))))
    (goto-char (+ pos 1)))
  (message "Done."))

(provide 'foreign)
