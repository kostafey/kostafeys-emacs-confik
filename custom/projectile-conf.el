;;; projectile-conf.el -- Projectile custom configuration.

(require 'elpa-conf)
(use-elpa 'projectile)

(projectile-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)

(defun k/projectile--find-file (invalidate-cache &optional ff-variant)
  "Jump to a project's file using completion.
With INVALIDATE-CACHE invalidates the cache first.  With FF-VARIANT set to a
defun, use that instead of `find-file'.   A typical example of such a defun
would be `find-file-other-window' or `find-file-other-frame'

Use clipboard text as an input string. Consider string part before a colon
symbol as a file name and number part after a colon symbol as a line number,
e.g.:
filename.scala:123
(filename.scala:123)"
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((clipboard-contents (current-kill 0))
         (file-with-line-number-matchp
          (eq 0
              (string-match "^(?[A-z0-9_-]+\\.\\(scala\\|xml\\|jsx\\):[0-9]+)?"
                            clipboard-contents))))
    (if file-with-line-number-matchp
        (let* ((file-name-s (replace-regexp-in-string
                             "^(?" ""
                             (car (split-string clipboard-contents ":"))))
               (line-number-s (replace-regexp-in-string
                               ")?$" ""
                               (cadr (split-string clipboard-contents ":"))))
               (project-root (projectile-acquire-root))
               (file (projectile-completing-read
                      (format "Find file [%s]: "
                              (propertize line-number-s
                                          'face 'font-lock-keyword-face))
                      (projectile-project-files project-root)
                      :initial-input file-name-s))
               (ff (or ff-variant #'find-file)))
          (when file
            (funcall ff (expand-file-name file project-root))
            (run-hooks 'projectile-find-file-hook)
            (when (and file-with-line-number-matchp
                       (equal (file-name-nondirectory file) file-name-s))
              (goto-line (string-to-number line-number-s)))))
      (projectile--find-file nil ff-variant))))

(provide 'projectile-conf)
