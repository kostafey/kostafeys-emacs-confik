;;; faq-to-csv.el --- Convert a FAQ .md file into a .csv file -*- lexical-binding: t; -*-

;; Source format (per entry):
;;
;;   ### Q: <question text, possibly wrapped across several lines,
;;   continuing until the "A:" marker>
;;   A: <answer text, possibly wrapped across several lines,
;;   continuing until the next "### Q:" header or end of file>
;;
;; Produces a CSV (question,answer) written to the same directory as the
;; source, with the extension changed to .csv.  The question is emitted
;; bare; the answer is always quoted (RFC 4180 style).

(defun faq--csv-quote (field)
  "Return FIELD quoted for CSV output (RFC 4180 style)."
  (concat "\""
          (replace-regexp-in-string "\"" "\"\"" (or field ""))
          "\""))

(defun faq--csv-field (field)
  "Return FIELD quoted only when it contains a comma, quote or newline."
  (if (string-match-p "[\",\n]" (or field ""))
      (faq--csv-quote field)
    (or field "")))

(defun faq--collapse-whitespace (text)
  "Trim TEXT and collapse internal runs of whitespace/newlines to single spaces."
  (string-trim
   (replace-regexp-in-string "[ \t\n\r]+" " " (or text ""))))

(defun faq-md-to-csv ()
  "Convert the FAQ markdown in the current buffer into a CSV file.
The CSV is written next to the source file with a .csv extension."
  (interactive)
  (let* ((md-file (or (buffer-file-name)
                      (error "Current buffer is not visiting a file")))
         (csv-file (concat (file-name-sans-extension md-file) ".csv"))
         (rows '()))
    (save-excursion
      (goto-char (point-min))
      ;; Find each question header.
      (while (re-search-forward "^[ \t]*###[ \t]+Q:[ \t]*" nil t)
        (let* ((q-beg (point))
               ;; Question runs (possibly across several lines) until the
               ;; "A:" answer marker at the start of a line.
               (q-end (if (re-search-forward "^[ \t]*A:[ \t]*" nil t)
                          (match-beginning 0)
                        (error "No \"A:\" answer marker after question")))
               (question (faq--collapse-whitespace
                          (buffer-substring-no-properties q-beg q-end)))
               (ans-beg (point))
               ;; Answer runs until the next "### Q:" header or end of file.
               (ans-end (if (re-search-forward "^[ \t]*###[ \t]+Q:" nil t)
                            (match-beginning 0)
                          (point-max)))
               (answer (faq--collapse-whitespace
                        (buffer-substring-no-properties ans-beg ans-end))))
          (goto-char ans-end)
          (push (cons question answer) rows))))
    (setq rows (nreverse rows))
    (with-temp-file csv-file
      (dolist (row rows)
        (insert (faq--csv-field (car row)) ","
                (faq--csv-quote (cdr row)) "\n")))
    (message "output to file %s" csv-file)
    csv-file))

(provide 'faq-to-csv)
;;; faq-to-csv.el ends here
