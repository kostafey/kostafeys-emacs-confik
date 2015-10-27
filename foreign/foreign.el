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
;; * foreign-replace

(require 'functions)

(defvar foreign-bin-path (concat-path "~/.emacs.d" "foreign"))

(defun foreign-get-go-executable (src-go-file)
  (let* ((src-go-path (concat-path foreign-bin-path src-go-file))
         (bin-go-path (substring src-go-path 0 (- (length src-go-path) 3))))
    (if (not (file-exists-p bin-go-path))
        (shell-command
         (concat "bash - c \"go build -o " bin-go-path " " src-go-path"\"")))
    bin-go-path))

(defun foreign-format-json-file (file-name)
  (let ((bin (foreign-get-go-executable "jsonpp.go")))
    (shell-command
     (concat bin " " file-name))))

(defun foreign-replace-file (file-name old-string new-string)
  (let ((bin (foreign-get-go-executable "replace.go")))
    (shell-command
     (concat bin " " file-name " " old-string " " new-string))))

(defun foreign-format-json ()
  "Pretty print json file."
  (interactive)
  (js-mode)
  (foreign-format-json-file (buffer-file-name))
  (revert-buffer t t)
  (message "Done."))

(defun foreign-replace (old-string new-string)
  "Replace string in file."
  (interactive
   (list
    (read-from-minibuffer "Replace string: "
                          "" nil nil 'foreign-replace-old-history)
    (read-from-minibuffer (concat "Replace with: ")
                          "" nil nil 'foreign-replace-new-history)))
  (foreign-replace-file (buffer-file-name)
                        (prepare-string-to-shell old-string)
                        (prepare-string-to-shell new-string))
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (find-file file-name)
    (message "Done.")))

(provide 'foreign)
