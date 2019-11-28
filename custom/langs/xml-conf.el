(defvar xml-format-newline-attributes nil)

(defun xml-format-toggle-newline-attributes ()
  (interactive)
  (setq xml-format-newline-attributes (not xml-format-newline-attributes))
  (message (format
			"%s new line for each XML tag attribute."
			(propertize (if xml-format-newline-attributes
                            "Add" "Don't add")
						'face 'font-lock-keyword-face))))

(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (mapcar
     (lambda (c)
       (goto-char begin)
       (replace-string c " " nil begin end))
     (list "\n" ""))
    (goto-char begin)
    (replace-regexp "[ \t\n]+" " " nil begin end)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (when xml-format-newline-attributes
     (goto-char begin)
     (while (search-forward-regexp " \\w+=\".*?\"" nil t)
       (let ((beg (match-beginning 0))
             (end (match-end 0)))
         (goto-char beg)
         (insert "\n")
         (goto-char end))))
    (indent-region begin end)
    (whitespace-cleanup))
  (message "Ah, much better!"))

(defun xml-format ()
  (interactive)
  (save-excursion
   (xml-pretty-print-region (point-min) (point-max))
   (mark-whole-buffer)
   (indent-for-tab-command)))

(defun k/nxml-mode-hook ()
  (setq indent-tabs-mode nil
        tab-width 4
        nxml-child-indent 4
        nxml-attribute-indent 4))

(add-hook 'nxml-mode-hook 'k/nxml-mode-hook)

(provide 'xml-conf)
