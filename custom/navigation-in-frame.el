;;; navigation-in-frame.el --- Simplify navigation among buffers and windows.

;;-----------------------------------------------------------------------------
;; popup-switcher
;;
(defun psw-org-mode-hook ()
  (if (equal major-mode 'org-mode)
      (outline-show-all)))
(add-hook 'psw-before-menu-hook 'psw-org-mode-hook)
(setq psw-use-flx t)
(setq psw-popup-position 'fill-column)

;;-----------------------------------------------------------------------------
;; ibuffer
;;
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

;;----------------------------------------------------------------------
;; Interact with browser
;;
(defun find-browser-executable ()
  (cond ((executable-find "chromium") "chromium")
        ((executable-find "chromium-browser") "chromium-browser")
        ((executable-find "google-chrome-stable") "google-chrome-stable")
        ((executable-find "palemoon") "palemoon")
        ((executable-find "firefox") "firefox")
        (t (message "Cant find any browser in the PATH."))))

(if (eq system-type 'windows-nt)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program
          "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program (find-browser-executable)))

(defun google (&optional arg)
  "Google the selected region if any, display a query prompt otherwise."
  (interactive "p")
  (let ((e1 (equal arg 1)))
    (browse-url
     (concat
      "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
      (url-hexify-string
       (if mark-active
           (buffer-substring (region-beginning) (region-end))
         (read-string "Google: "
                      (if (and e1 (symbol-at-point))
                          (symbol-name (symbol-at-point))))))))))

(defun goto-url (&optional arg)
  "Go to selected region as URL if any, display a query prompt otherwise."
  (interactive "p")
  (browse-url
   (if mark-active
       (buffer-substring (region-beginning) (region-end))
     (let ((entered-str
            (read-string "Go to URL: "
                         (if (not (equal arg 1))
                             (symbol-name (symbol-at-point))))))
       (if (not (equal "http" (substring entered-str 0 4)))
           (concat "http://" entered-str)
         entered-str)))))

(provide 'navigation-in-frame)

;;; navigation-in-frame.el ends here
