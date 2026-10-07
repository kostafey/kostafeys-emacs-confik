;;; basic-navigation.el --- Navigation  -*- lexical-binding: t -*-

;; Moving the point, scrolling, windows and buffers.
;; No third-party dependencies.

;;-----------------------------------------------------------------------------
;; ibuffer
;;
(require 'ibuffer)
(setq-default ibuffer-default-sorting-mode 'major-mode)    ; sorting
(setq ibuffer-never-show-predicates (list "^\\*" "magit")) ; filter buffers

;; list ouptut format
(define-ibuffer-column k/path-and-process
  (:name "Filename/Process"
         :header-mouse-map ibuffer-filename/process-header-map
         :summarizer
         (lambda (strings)
           (setq strings (delete "" strings))
           (let ((procs 0)
	             (files 0))
             (dolist (string strings)
               (when (get-text-property 1 'ibuffer-process string)
                 (setq procs (1+ procs)))
	           (setq files (1+ files)))
             (concat (cond ((zerop files) "No files")
		                   ((= 1 files) "1 file")
		                   (t (format "%d files" files)))
	                 ", "
	                 (cond ((zerop procs) "no processes")
		                   ((= 1 procs) "1 process")
		                   (t (format "%d processes" procs)))))))
  (let ((proc (get-buffer-process buffer))
        (filename (file-name-directory (ibuffer-make-column-filename buffer mark))))
    (if proc
	    (concat (propertize (format "(%s %s)" proc (process-status proc))
			                'font-lock-face 'italic
                            'ibuffer-process proc)
		        (if (> (length filename) 0)
		            (format " %s" filename)
		          ""))
      filename)))

(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 40 40 :left :elide)
			  " " (size 9 -1 :right)
			  " " (mode 16 16 :left :elide) " " k/path-and-process)
		(mark " " (name 16 -1) " " filename)))

;;-------------------------------------------------------------------
;; Odinary C-<right>, C-<left> movements
;;
(defun k/select ()
  (if (not mark-active)
      (cua-set-mark)))

(defun k/deselect ()
  (if (not cua--rectangle)
      (setq deactivate-mark t)))

(defun k/step-forward-word ()
  "Like odinary editors, C-<right> moves forward word."
  (skip-chars-forward " \t")
  (forward-same-syntax 1))

(defun k/step-backward-word ()
  "Like odinary editors, C-<left> moves backward word."
  (skip-chars-backward " \t")
  (forward-same-syntax -1))

(defun k/line-next (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (line-move 1))
(defun k/line-previous (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (line-move -1))

(defun k/char-forward (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (right-char 1))
(defun k/char-backward (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (left-char 1))

(defun k/word-forward (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (k/step-forward-word))
(defun k/word-backward (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (k/step-backward-word))

(defun k/code-block-fences ()
  "Return (OPEN . CLOSE) - beginnings of the opening and closing fence
lines of the markdown/org code block at point, or nil."
  (cond
   ((derived-mode-p 'markdown-mode)
    (when-let* ((bounds (markdown-get-enclosing-fenced-block-construct)))
      (cons (save-excursion (goto-char (car bounds))
                            (line-beginning-position))
            (save-excursion (goto-char (cadr bounds))
                            (skip-chars-backward " \t\n")
                            (line-beginning-position)))))
   ((derived-mode-p 'org-mode)
    (let ((el (org-element-at-point)))
      (when (string-suffix-p "-block" (symbol-name (org-element-type el)))
        (cons (org-element-property :post-affiliated el)
              (save-excursion (goto-char (org-element-property :end el))
                              (skip-chars-backward " \t\n")
                              (line-beginning-position))))))))

(defun k/code-block-content-at-fence (fence)
  "When the current line is the FENCE (`open' or `close') line of a code
block, return (BEG . END) of the block content without the fence lines."
  (when-let* ((fences (k/code-block-fences))
              ((< (car fences) (cdr fences)))
              ((= (line-beginning-position)
                  (if (eq fence 'open) (car fences) (cdr fences)))))
    (let ((beg (save-excursion (goto-char (car fences))
                               (forward-line 1)
                               (point))))
      (cons beg (max beg (1- (cdr fences)))))))

(defun k/sexp-forward (&optional select) (interactive)
       (let ((content (k/code-block-content-at-fence 'open)))
         (when content (goto-char (car content)))
         (if select (k/select) (k/deselect))
         (if content
             (goto-char (cdr content))
           (forward-sexp 1))))
(defun k/sexp-backward (&optional select) (interactive)
       (let ((content (k/code-block-content-at-fence 'close)))
         (when content (goto-char (cdr content)))
         (if select (k/select) (k/deselect))
         (if content
             (goto-char (car content))
           (backward-sexp 1))))
(defun k/line-beginning (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (beginning-of-line))
(defun k/line-end (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (end-of-line))

(defun k/buffer-beginning (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (goto-char (point-min)))
(defun k/buffer-end (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (goto-char (point-max)))

;;-----------------------------------------------------------------------------
;; Eldoc
(require 'eldoc)
;; Run ElDoc after this commands:
(mapc 'eldoc-add-command '(k/char-forward
                           k/char-backward
                           k/word-forward
                           k/word-backward
                           k/sexp-forward
                           k/sexp-backward
                           k/line-next
                           k/line-previous))

;;===================================================================
;; Scrolling
;;
(setq next-screen-context-lines 10)     ; Number of lines of continuity when
                                        ; scrolling by screenfuls.

;; keyboard
(setq scroll-step 1)

;; If point moves off-screen, redisplay will scroll by up to
;; `scroll-conservatively' lines in order to bring point just barely
;; onto the screen again.
(setq scroll-conservatively 50)
;; Point keeps its screen position if the scroll command moved it
;; vertically out of the window, e.g. when scrolling by full screens.
(setq scroll-preserve-screen-position t)
;; Trigger automatic scrolling whenever point gets within this many lines
;; of the top or bottom of the window.
(setq scroll-margin 0)

;; mouse
(setq mouse-wheel-mode t)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-drag-copy-region nil)

(defun smooth-scroll (increment)
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment))

;;-------------------------------------------------------------------
;; sgml-mode
(require 'sgml-mode nil 'noerror)

(defvar k/sgml-tags (list "<" ">"))

(defun k/sgml-skip-tag-forward (&optional select)
  (interactive)
  (if select
      (k/select)
    (k/deselect))
  (if (member (string (following-char)) k/sgml-tags)
      (sgml-skip-tag-forward 1)
    (sgml-forward-sexp 1)))

(defun k/sgml-skip-tag-backward (&optional select)
  (interactive)
  (if select
      (k/select)
    (k/deselect))
  (if (member (string (preceding-char)) k/sgml-tags)
      (sgml-skip-tag-backward 1)
    (sgml-forward-sexp -1)))

(defun k/sgml-skip-tag-forward-select ()
  (interactive)
  (k/sgml-skip-tag-forward t))

(defun k/sgml-skip-tag-backward-select ()
  (interactive)
  (k/sgml-skip-tag-backward t))

;;===================================================================
;; Rotate windows if more than 2 of them
;;
(defun swap-buffers (w1 w2)
  (let ((b1 (window-buffer w1))
        (b2 (window-buffer w2))
        (s1 (window-start w1))
        (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)
    (set-window-start w1 s2)
    (set-window-start w2 s1)))

(defun swap-windows ()
  "If you have 2 windows or 2 frames, it swaps them."
  (interactive)
  (cond ((= (count-windows) 2)
         (let* ((w1 (car (window-list)))
                (w2 (cadr (window-list))))
           (swap-buffers w1 w2)))
        ((= (length (frame-list)) 2)
         (let* ((w1 (car (window-list (car (frame-list)))))
                (w2 (car (window-list (cadr (frame-list))))))
           (swap-buffers w1 w2)))
        (t
         (message "You need exactly 2 windows or frames to do this."))))

(defun mirror-window ()
  "Show the same buffer in the second window as in the active window."
  (interactive)
  (let ((mirror #'(lambda ()
                    (let* ((w1 (car (window-list)))
                           (w2 (cadr (window-list)))
                           (b1 (window-buffer w1))
                           (s1 (window-start w1)))
                      (set-window-start w2 s1)
                      (set-window-buffer w2 b1))) ))
    (cond ((= (count-windows) 1)
           (progn
             (split-window-right)
             (funcall mirror)))
          ((= (count-windows) 2)
           (funcall mirror))
          (t
           (message "You need exactly 2 windows to do this.")))))

;;===================================================================
;; Jump back to the last position of the cursor
;;
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; ------------------------------------------------------------
;; windmove
(require 'windmove)

(defun meta-left ()
  (interactive)
  (if (or (windmove-find-other-window 'left)
          (> (length (frame-list)) 1))
      (windmove-left)
    (error "No window left from selected window")))

(defun meta-right ()
  (interactive)
  (if (or (windmove-find-other-window 'right)
          (> (length (frame-list)) 1))
      (windmove-right)
    (error "No window right from selected window")))

;;-------------------------------------------------------------------
;; Kill or create buffer(s)
;;
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun kill-special-buffers ()
  "Kill special buffers but the current one.
Buffers whose name starts with a space are internal working buffers of
Emacs itself (e.g. \" *tab-line-hscroll*\"); killing them silently breaks
the feature that owns them, so leave them alone."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer))
                (buffer-file-name buffer)
                (string-prefix-p " " (buffer-name buffer)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(defun find-file-from-clipboard ()
  "Open file or directory path from clipboard (kill ring) if path exists."
  (interactive)
  (let ((file-path (current-kill 0)))
    (if (file-exists-p file-path)
        (find-file file-path)
      (message "Can't find file '%s'" file-path))))

(provide 'basic-navigation)

;;; basic-navigation.el ends here
