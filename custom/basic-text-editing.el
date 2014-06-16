;;; basic-text-editing.el --- set of misc text editing functions.

(require 'elpa-conf)

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

(defun insert-time()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
 (insert (format-time-string "%H:%M" (current-time))))

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

(fset 'align-by-quote-macro
   [?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g ?e ?x ?p return ?\' return])

(defun align-by-quote (beginning end)
  "Align declarations leveled by quote `''"
  (interactive "r")
  (execute-kbd-macro (symbol-function 'align-by-quote-macro)))


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

;; by Xah Lee at
;; `http://code.google.com/p/ergoemacs/source/browse/ergoemacs/ergoemacs-keybindings/functions.el'
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    ) )

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun json-format ()
  "Pretty print json string."
  (interactive)
  (beginning-of-buffer)
  (replace-string "," ",\n")
  (js-mode)
  (mark-whole-buffer)
  (indent-for-tab-command))

(defun slice-text-inner (chars border)
  (let* ((chars (if chars chars 1))
         (chars (if (stringp chars) (string-to-number chars) chars))
         (chars (if (> chars 0) chars 1)))
    (save-excursion
      (beginning-of-line)
      (while (< (point) (line-end-position))
        (insert border)
        (right-char (min chars (- (line-end-position) (point))))
        (if (and (= (preceding-char) 92)
                 (= (following-char) 34)
                 (> chars 1))
            (left-char 1))
        (insert border)
        (newline 1)))))

(defun slice-text (chars)
  "Format string of text to square box of text with CHARS width."
  (interactive "P")
  (if (equal nil chars)
      (let ((chars (read-from-minibuffer
                    "Slice text at char: "
                    "60" nil nil 'slice-text-char-history))
            (border (read-from-minibuffer
                     "Slice text border: "
                     "\"" nil nil 'slice-text-border-history)))
        (slice-text-inner chars border))
    (slice-text-inner chars "")))

(provide 'basic-text-editing)
