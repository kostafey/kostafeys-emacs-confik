;;; basic-text-editing.el --- set of misc text editing functions.

(require 'simple)

(require 'browse-kill-ring)

(global-set-key (kbd "C-?") 'describe-char)

;; show ascii table
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))


;;;Handy MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  insert current date into the buffer at point  
(defun insert-date()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;Очистка SQL
;"\|\(\\n\)\|\+
(defun insert-crear-sql-regexp()
  (interactive)
  (insert "\"\\|\\(\\\\n\\)\\|\\+"))

;;=============================================================================
;; Join lines
;;=============================================================================
(defun circle-processing (arg function)
  "Circle call 'function' 'arg' times, default - once"
  (interactive)
  (progn
	(if arg
		(setq times arg)
	  (setq times 1)) 
	(let (counter)
	  (dotimes (counter times)
		(apply function nil)))))

(defun join-next-line-space ()
  "Joins next line with current with a space between them"
  (interactive)
	(progn
	  (end-of-line)
	  (next-line)
	  (join-line)))

(defun join-next-line ()
  "Joins next line with current without space between them"
  (interactive)
  (progn
	  (join-next-line-space)
	  (delete-char 1)))

(defun join-next-line-semicolon ()
  "Joins next line with current with semicolon between them"
  (interactive)
  (progn
	  (join-next-line)
	  (insert ";")))

(defun join-next-line-space-n (&optional arg)
  "Joins number of next lines with current with a space between them"
  (interactive "P")
(circle-processing arg 'join-next-line-space))
(global-set-key "\C-j" 'join-next-line-space-n)

(defun join-next-line-n (&optional arg)
  "Joins number of  next lines with current without space between them"
  (interactive "P")
(circle-processing arg 'join-next-line))
(global-set-key "\C-cj" 'join-next-line-n)

(defun join-next-line-semicolon-n (&optional arg)
  "Joins number of  next lines with current with semicolon between them"
  (interactive "P")
(circle-processing arg 'join-next-line-semicolon))
(global-set-key "\C-c\C-j" 'join-next-line-semicolon-n)

;;=============================================================================
;; Duplicate current line
;;=============================================================================
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))
(global-set-key "\C-cd" 'duplicate-line)
;;=============================================================================

(defun comment-or-uncomment-this (&optional lines)
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
          (comment-or-uncomment-region (mark) (point))
          (comment-or-uncomment-region (point) (mark)))
      (comment-or-uncomment-region
       (line-beginning-position)
       (line-end-position lines))))
(global-set-key (kbd "C-;") 'comment-or-uncomment-this)




(provide 'basic-text-editing)