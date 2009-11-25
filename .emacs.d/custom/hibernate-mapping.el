(require 'pc-select)
(require 'cua-base)

(fset 'copy-region
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([C-S-right 3 timeout] 0 "%d")) arg)))

(defun hibernate-create-property ()
  "Joins next line with current with semicolon between them"
  (interactive)
	  (insert "<property name=\"")
	  ;(forward-word-mark)
	  (copy-region)
	  (insert "\">")
	  (delete-horizontal-space)
	  (newline)  
	  (insert "	<column name=\"")
	  (yank)
	  (insert "\"")
	  (just-one-space)
	  (insert "sql-type=\"")
	  (end-of-line-nomark)
	  (insert "\" not-null=\"false\"/>")
	  (newline)
	  (insert "</property>"))

(provide 'hibernate-mapping)
