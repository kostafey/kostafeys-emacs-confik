;;; basic-navigation.el --- Navigation among buffers  -*- lexical-binding: t -*-

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

(provide 'basic-navigation)

;;; basic-navigation.el ends here
