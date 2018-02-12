(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
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
