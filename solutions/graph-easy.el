;;; graph-easy.el --- Run graph-easy in code comments.

;;; Copyright (C) 2019 - Kostafey <kostafey@gmail.com>

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; graph-easy::begin
;; graph { flow: south; }
;; [ install\n Perl ]  -->
;; [ run\n cpanm Graph::Easy ] -->
;; [ type code below\n graph-easy::begin] -- M-x graph-easy-run -->
;; [get result in\n graph-easy::output]
;; graph-easy::output
;; +---------------------+
;; |       install       |
;; |        Perl         |
;; +---------------------+
;;   |
;;   |
;;   v
;; +---------------------+
;; |         run         |
;; |  cpanm Graph::Easy  |
;; +---------------------+
;;   |
;;   |
;;   v
;; +---------------------+
;; |   type code below   |
;; |  graph-easy::begin  |
;; +---------------------+
;;   |
;;   | M-x graph-easy-run
;;   v
;; +---------------------+
;; |    get result in    |
;; | graph-easy::output  |
;; +---------------------+
;; graph-easy::end

;;; Code:

(defcustom graph-easy-begin "graph-easy::begin" "")
(defcustom graph-easy-output "graph-easy::output" "")
(defcustom graph-easy-end "graph-easy::end" "")
(defcustom graph-easy-input-file "graph-easy-input.txt" "")
(defcustom graph-easy-command "graph-easy --as=ascii --input=%s" "")

(defun graph-easy-get-comment-first-sexp ()
  (if-let ((is-comment (comment-beginning)))
      (if-let ((text (thing-at-point 'sexp)))
          text
        t)))

(defun graph-easy-get-marker-pos (marker-text move)
  (save-excursion
    (end-of-line)
    (let ((found nil)
          (pos nil))
      (while (and (not found) (graph-easy-get-comment-first-sexp))
        (if (equal marker-text
                   (graph-easy-get-comment-first-sexp))
            (progn
              (setq found t)
              (if (> move 0)
                  (end-of-line)
                (beginning-of-line))
              (setq pos (point)))
          (line-move move)))
      pos)))

(defun graph-easy-eval (code)
  (let ((filename (make-temp-file graph-easy-input-file)))
    (with-temp-file filename      
      (insert code))
    (shell-command-to-string
     (format graph-easy-command filename))))

(defun graph-easy-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun graph-easy-run ()
  (interactive)
  (save-excursion
    (let ((beg (graph-easy-get-marker-pos graph-easy-begin -1)))
      (if beg
          (let* ((mode major-mode)
                 (out (graph-easy-get-marker-pos graph-easy-output 1))
                 (end (graph-easy-get-marker-pos graph-easy-end 1))
                 (src (let ((commented-src
                             (buffer-substring (progn
                                                 (goto-char beg)
                                                 (next-line)
                                                 (beginning-of-line)
                                                 (point))
                                               (progn
                                                 (goto-char out)
                                                 (previous-line)
                                                 (end-of-line)
                                                 (point)))))
                        (with-temp-buffer
                          (funcall mode)
                          (insert commented-src)
                          (uncomment-region (point-min) (point-max))
                          (buffer-substring (point-min) (point-max)))))
                 (result (let ((uncommented-result (graph-easy-eval src)))
                           (with-temp-buffer
                             (funcall mode)
                             (insert uncommented-result)
                             (comment-region (point-min) (point-max))
                             (buffer-substring (point-min) (1- (point-max)))))))
            (graph-easy-flash-region beg end)
            (delete-region (progn
                             (goto-char out)
                             (next-line)
                             (beginning-of-line)
                             (point))
                           (progn
                             (goto-char end)
                             (previous-line)
                             (end-of-line)
                             (point)))
            (insert result)
            (message "Done graph-easy."))
        (message "Not in graph-easy block.")))))

(provide 'graph-easy)
