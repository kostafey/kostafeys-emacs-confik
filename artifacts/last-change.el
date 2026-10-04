;;; last-change.el --- Jump to recent changes  -*- lexical-binding: t -*-

;; Copyright 1996, 1997, 1998, 1999, 2001, 2002, 2003, 2010, 2011, 2012, 2015
;; Free Software Foundation, Inc.
;;
;; Author: Christoph Wedler <wedler@users.sourceforge.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the Licence, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://gnu.org/licences/>.

;;; Commentary:

;; `session-jump-to-last-change' from session.el 2.4b
;; (http://emacs-session.sourceforge.net/), the only part of that package
;; in use here.  It walks `buffer-undo-list', so it needs nothing saved
;; across sessions.  Left out are the C-u form, which stored a position
;; for the session data, and the after-jump hook.

;;; Code:

(require 'cl-lib)

(defgroup last-change nil
  "Jump to the positions of recent changes."
  :group 'editing)

(defcustom last-change-threshold 80
  "Number of character positions the change position must be different.
Without prefix arg, `last-change-jump' jumps successively to change
positions which differ by at least this many characters compared to the
current position and previously visited change positions, see
`last-change-remember'."
  :type 'integer)

(defcustom last-change-remember 2
  "Number of previously visited change positions checked additionally.
See `last-change-threshold' and `last-change-jump'."
  :type 'integer)

(defvar last-change--counter 0
  "Number of repeated invocations of `last-change-jump'.")

(defvar last-change--recent nil
  "Current position and previously visited change positions.")

(defun last-change--undo-position (num pos1 pos2)
  "Return a previous undo position, or nil if there is no such position.
If POS1 and POS2 are nil, NUM is the number of undo boundaries to skip.
The position returned is the last change inside the corresponding undo
step.

Otherwise, NUM is the number of undo entries to skip.  The position
returned is the last change after these entries outside the range from
POS1 to POS2.  Increment `last-change--counter' by the number of
entries skipped additionally, set it to nil if nothing is found."
  (let ((undo-list (and (consp buffer-undo-list) buffer-undo-list))
        elem      ; element in undo-list, t = not of interest
        back-list ; used position must be recomputed due to processed elems
        len       ; length of deletion/insertion
        pos)      ; interesting position in undo-list
    (while (and undo-list (null (car undo-list)))
      (pop undo-list))                  ; ignore undo-boundaries at beg
    (while undo-list
      ;; inspect element in undo-list
      (setq elem (pop undo-list))
      (cond ((atom elem)                ; marker position
             (when (or elem pos1) ; undo-boundary is of interest if POS1=nil
               (if (integerp elem)
                   (setq pos elem       ; use point position in undo-list
                         back-list (cons nil back-list))
                 (setq elem t))))       ; ignore uninteresting elem
            ((stringp (car elem))       ; deletion: (TEXT . POSITION)
             (setq pos (abs (cdr elem))
                   len (length (car elem)))
             (push (cl-list* pos (+ pos len) (- len)) back-list)
             (when pos1                 ; adopt POS{1,2} if after deletion
               (if (>  pos1 pos) (cl-incf pos1 len))
               (if (>= pos2 pos) (cl-incf pos2 len))))
            ((integerp (car elem))      ; insertion: (START . END)
             (setq pos (car elem)
                   len (- (cdr elem) pos))
             (push (cl-list* pos pos len) back-list)
             (when pos1                 ; adopt POS{1,2} if after/in insertion
               (if (> pos1 pos)
                   (setq pos1 (if (> pos1 (cdr elem)) (- pos1 len) pos)))
               (if (> pos2 pos)
                   (setq pos2 (if (> pos2 (cdr elem)) (- pos2 len) pos))))
             (setq pos (cdr elem)))     ; point more likely at end of insertion
            (t
             (setq elem t)))
      ;; evaluate element inspection
      (cond ((null pos1)                ; looking for undo-boundaries
             (when (if elem
                       (and (zerop num) pos)
                     (<= (cl-decf num) 0))
               (setq undo-list nil)))
            ((eq elem t)                ; uninteresting element
             (setq pos nil))
            ((> num 0)                  ; interesting, but not the NUM's one
             (cl-decf num)
             (setq pos nil))
            ((and (<= pos1 pos) (<= pos pos2)) ; inside start region
             (cl-incf last-change--counter)
             (setq pos nil))
            (t
             (setq undo-list nil))))
    ;; finalize: evaluate result and process back-list
    (if (or (null pos) (> num 0))       ; no position found in undo-list
        (setq last-change--counter nil
              pos nil)
      (if last-change--counter
          (cl-incf last-change--counter))
      (setq back-list (cdr back-list))
      (while back-list
        (setq elem (pop back-list))
        (cond ((null elem))             ; integer position in undo-list
              ((> pos (cadr elem))      ; position after affected region
               (cl-incf pos (cddr elem))) ; increment/decrement position
              ((> pos (car elem))       ; position in affected region
               (setq pos (car elem))))))  ; set position to region begin
    pos))

(defun last-change-jump (&optional arg)
  "Jump to the position of the last change.
Without prefix arg, jump successively to previous change positions which
differ by at least `last-change-threshold' characters by repeated
invocation of this command.  With prefix argument 0, jump to end of last
change.  With numeric prefix argument, jump to start of first change in
the ARG's undo block in the `buffer-undo-list'."
  (interactive "P")
  ;; set and restrict previously visited undo positions
  (push (point) last-change--recent)
  (if (and (null arg) (eq last-command 'last-change--jump-seq))
      (let ((recent (nthcdr last-change-remember last-change--recent)))
        (if recent (setcdr recent nil)))
    (setcdr last-change--recent nil)    ; only point
    (setq last-change--counter 0))
  (let (pos)
    (if arg
        (setq pos (last-change--undo-position
                   (abs (prefix-numeric-value arg)) nil nil))
      ;; compute position, compare it with positions in
      ;; `last-change--recent'
      (let ((recent last-change--recent) old pos1 pos2)
        (setq pos (point))
        (while recent                   ; at least point is there
          (setq old (pop recent))
          (setq pos1 (- pos last-change-threshold)
                pos2 (+ pos last-change-threshold))
          (when (and (<= pos1 old) (<= old pos2))
            (setq pos (last-change--undo-position
                       last-change--counter pos1 pos2))
            (setq recent (and pos
                              last-change--counter
                              last-change--recent))))))
    (cond ((null pos)
           (message (if (or arg (atom buffer-undo-list))
                        "Do not know position of last change"
                      "Do not know position of last distant change")))
          ((< pos (point-min))
           (goto-char (point-min))
           (message "Change position outside visible region"))
          ((> pos (point-max))
           (goto-char (point-max))
           (message "Change position outside visible region"))
          (t
           (goto-char pos)
           (unless arg
             (setq this-command 'last-change--jump-seq))))))

(provide 'last-change)
