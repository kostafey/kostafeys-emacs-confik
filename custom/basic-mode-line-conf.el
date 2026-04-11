;;; basic-mode-line-conf.el --- Mode line configuration.

(defun k/show-buffer-environment ()
  "Create a temporary buffer with current buffer's environment details."
  (interactive)
  (let* ((file-path (or (buffer-file-name) "No file associated"))
         (encoding (coding-system-get buffer-file-coding-system :mime-charset))
         (eol (nth (coding-system-eol-type buffer-file-coding-system)
                   '("unix" "dos" "mac")))
         (maj-mode major-mode)
         ;; Filter minor-mode-list for active modes
         (active-minors (cl-remove-if-not
                         (lambda (m) (and (boundp m) (symbol-value m)))
                         minor-mode-list)))
    (with-current-buffer (get-buffer-create "*Buffer Environment*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "File path:    %s\n" file-path))
        (insert (format "Encoding:     %s\n" encoding))
        (insert (format "Endlines:     %s\n" eol))
        (insert (format "Major mode:   %s\n" maj-mode))
        (insert (format "Minor modes:  %s\n"
                        (mapconcat 'symbol-name active-minors
                                   (format (format "\n%%%ds"
                                                   (length "Minor modes:  "))
                                           " "))))
        (goto-char (point-min))
        (help-mode) ; Adds nice formatting and 'q' to quit
        (display-buffer (current-buffer))))))

(setq-default
 mode-line-format
 (list "  "
       ;; ------------------------------------------------------------
       ;; the `buffer-name'; the file name as a tool tip
       '(:eval (propertize (buffer-name)
                           'face '(:weight bold)
                           'mouse-face 'mode-line-highlight
                           'help-echo (buffer-file-name)
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map [mode-line mouse-1]
                                          'k/show-buffer-environment)
                                        map)))
       ;; ------------------------------------------------------------
       ;; line and column
       " (" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%02l" 'face 'font-lock-string-face) ","
       (propertize "%02c" 'face 'font-lock-string-face)
       ")"
       '(:eval (when mark-active
                 (propertize
                  (format " %s:%s"
                          (region-selection-length)
                          (region-selection-count-lines))
                  'face 'font-lock-constant-face)))
       ;; ------------------------------------------------------------
       ;; csv field index for `csv-mode'
       '(:eval (when (and (eq major-mode 'csv-mode)
                          csv-field-index-mode
                          (fboundp 'k/csv-get-field-index))
                 (propertize
                  (format " %s" (k/csv-get-field-index))
                  'face 'escape-glyph)))
       ;; ------------------------------------------------------------
       ;; version control data
       '(:eval (propertize (if (vc-mode-line buffer-file-name)
                               vc-mode
                             "")
                           'face 'font-lock-constant-face))
       ;; ------------------------------------------------------------
       ;; `project' see (project-mode-line-format) fn
       '(:eval (propertize
                (if (project-current)
                    (format " [%s]"
                            (when-let* ((project (project-current)))
                              (let ((last-coding-system-used last-coding-system-used))
                                (propertize
                                 (project-name project)
                                 'face project-mode-line-face
                                 'mouse-face 'mode-line-highlight
                                 'help-echo "mouse-1: Project menu"
                                 'local-map project-mode-line-map))))
                  "")))
       ;; ------------------------------------------------------------
       ;; read only, insert/overwrite, edited signs
       " (" ;; insert vs overwrite mode, input-method in a tooltip
       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                           'face 'font-lock-preprocessor-face
                           'help-echo (concat "Buffer is in "
                                              (if overwrite-mode
                                                  "overwrite"
                                                "insert")
                                              " mode")))
       ;; was this buffer modified since the last save?
       '(:eval (when (buffer-modified-p)
                 (concat ","  (propertize "*"
                                          'face 'font-lock-string-face
                                          'help-echo "Buffer has been modified"))))
       ;; is this buffer read-only?
       '(:eval (when buffer-read-only
                 (concat ","  (propertize "RO"
                                          'face 'font-lock-string-face
                                          'help-echo "Buffer is read-only"))))
       ") "
       ;; ------------------------------------------------------------
       ;; file encoding
       ;; 'mode-line-mule-info
       '(:eval (propertize
                (format "%s/%s"
                        (let ((base (coding-system-base buffer-file-coding-system)))
                          (if (eq base 'prefer-utf-8)
                              'utf-8
                            base))
                        (let ((eol (coding-system-eol-type buffer-file-coding-system)))
                          (cond ((eq eol 0) "unix")
                                ((eq eol 1) "dos")
                                ((eq eol 2) "mac")
                                (t "unknown"))))
                'help-echo (format"%s\n%s\n%s\n%s"
                                  buffer-file-coding-system
                                  "[f8]		recode-buffer-rotate-ring"
                                  "[C-f8]	eol-buffer-rotate-ring"
                                  "[M-f8]	describe-coding-system")))))

(provide 'basic-mode-line-conf)

;;; basic-mode-line-conf.el ends here
