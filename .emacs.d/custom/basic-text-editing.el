;;; basic-text-editing.el --- set of misc text editing functions.

(require 'simple)
(require 'browse-kill-ring)

(defun count-words-region (beginning end arg) 
  "Counting words (chars) in the selected area.
arg - is a searching word (char)"
  (interactive "r\nMSearching word or char: ")
  
  (message "Counting ... ")
  (save-excursion
    (goto-char beginning)
    (let ((count 0)
          (search-word arg))

      (while (and (< (point) end)
                  (search-forward search-word end t))
        (setq count (1+ count)))

      (message 
       "There is %d words in the selected area." count))))

;;=============================================================================
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


;;=============================================================================
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

(defun join-next-line-n (&optional arg)
  "Joins number of  next lines with current without space between them"
  (interactive "P")
(circle-processing arg 'join-next-line))

(defun join-next-line-semicolon-n (&optional arg)
  "Joins number of  next lines with current with semicolon between them"
  (interactive "P")
(circle-processing arg 'join-next-line-semicolon))

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

;;=============================================================================
(defun insert-column-counter (n)
  "Insert a sequence of numbers vertically.
For example, if your text is:

a b
c d
e f

and your cursor is after “a”, then calling this function with argument
3 will change it to become:

a1 b
c2 d
e3 f

If there are not enough existing lines after the cursor
when this function is called, it aborts at the last line.

This command is conveniently used together with `kill-rectangle' and `string-rectangle'."
  (interactive "nEnter the max integer: ")
  (let ((i 1) colpos )
    (setq colpos (- (point) (line-beginning-position)))
    (while (<= i n)
      (insert (number-to-string i))
      (forward-line) (beginning-of-line) (forward-char colpos)
      (setq i (1+ i)))))

;;=============================================================================
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph       
;;; Takes a multi-line paragraph and makes it into a single line of text.       
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;=============================================================================
;; Wrap text with punctation or tag
(when (require 'wrap-region nil 'noerror)
  (wrap-region-global-mode t)
  (wrap-region-add-wrapper "*" "*"))

;=============================================================================
(fset 'align-by-column-macro
   [?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g ?e ?x ?p return ?\\ ?: return])

(defun align-by-column (beginning end)
  "Align declarations leveled by colon `:'
Formating from:
    xtype : 'radio',
    boxLabel : 'some label',
    name : 'some name'
to:
    xtype    : 'radio',
    boxLabel : 'some label',
    name     : 'some name'
"
  (interactive "r")
  ;;(align-regexp beginning end "\:")
  (execute-kbd-macro (symbol-function 'align-by-column-macro)))

;;=============================================================================
;; Recode english to russian input
(defvar u:*en/ru-table*
     '((?q  . ?й) (?w  . ?ц) (?e  . ?у)
       (?r  . ?к) (?t  . ?е) (?y  . ?н) (?u  . ?г)
       (?i  . ?ш) (?o  . ?щ) (?p  . ?з) (?[  . ?х)
       (?]  . ?ъ) (?a  . ?ф) (?s  . ?ы) (?d  . ?в)
       (?f  . ?а) (?g  . ?п) (?h  . ?р) (?j  . ?о)
       (?k  . ?л) (?l  . ?д) (?\; . ?ж) (?\' . ?э)
       (?z  . ?я) (?x  . ?ч) (?c  . ?с) (?v  . ?м)
       (?b  . ?и) (?n  . ?т) (?m  . ?ь) (?,  . ?б)
       (?.  . ?ю) (?/  . ?.) (?!  . ?!) (?@  . ?\")
       (?#  . ?№) (?$  . ?\;) (?%  . ?%) (?^  . ?:)
       (?&  . ??) (?*  . ?*) (?Q  . ?Й) (?W  . ?Ц)
       (?E  . ?У) (?R  . ?К) (?T  . ?Е) (?Y  . ?Н)
       (?U  . ?Г) (?I  . ?Ш) (?O  . ?Щ) (?P  . ?З)
       (?{  . ?Х) (?}  . ?Ъ) (?A  . ?Ф)
       (?S  . ?Ы) (?D  . ?В) (?F  . ?А) (?G  . ?П)
       (?H  . ?Р) (?J  . ?О) (?K  . ?Л) (?L  . ?Д)
       (?:  . ?Ж) (?\" . ?Э) (?Z  . ?Я) (?X  . ?Ч)
       (?C  . ?С) (?V  . ?М) (?B  . ?И) (?N  . ?Т)
       (?M  . ?Ь) (?<  . ?Б) (?>  . ?Ю) (?\? . ?,)))

(defun u:en/ru-recode-region (beg end &optional arg)
  "Recode the given region, that contains Russain text typed in English, into Russian.
With ARG recode from Russian o English."

  (interactive "*r\nP")
  (save-excursion
    (goto-char beg)
    (do () ((>= (point) end))
      (let* ((en-char (char-after (point)))
             (ru-char (if arg 
                          (car (rassoc en-char u:*en/ru-table*))
                        (cdr (assoc en-char u:*en/ru-table*)))))
        (delete-char 1)
        (insert (if ru-char ru-char en-char))))))

;; Copy rectangle to clipboard for use outside from emacs.
;; by Xah Lee at
;; `http://xahlee.blogspot.com/2012/06/emacs-commpand-to-copy-rectangle-to.html'
(defun copy-rectangle-to-clipboard (p1 p2)
  "Copy region as column (rectangle) to operating system's clipboard.
This command will also put the text in register 0. (see: `copy-to-register')"
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (copy-rectangle-to-register ?0 p1 p2)
    (kill-new
     (with-temp-buffer
       (insert-register ?0)
       (buffer-string)))))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(provide 'basic-text-editing)
