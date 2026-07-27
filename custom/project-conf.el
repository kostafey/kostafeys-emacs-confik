;;; project-conf.el -- Built-in project.el custom configuration. -*- lexical-binding: t -*-

(require 'project)

(defun k/project-find-file (&optional ff-variant)
  "Jump to a project's file using completion.
With FF-VARIANT set to a defun, use that instead of `find-file'.
A typical example of such a defun would be `find-file-other-window' or
`find-file-other-frame'.

Use clipboard text as an input string. Consider string part before a colon
symbol as a file name and number part after a colon symbol as a line number,
e.g.:
filename.scala:123
(filename.scala:123)"
  (interactive "P")
  (let* ((clipboard-contents (when kill-ring (current-kill 0)))
         (file-with-line-number-matchp
          (when clipboard-contents
            (eq 0
                (string-match "^(?[A-z0-9_-]+\\.\\(scala\\|xml\\|jsx\\):[0-9]+)?"
                              clipboard-contents)))))
    (if file-with-line-number-matchp
        (let* ((file-name-s (replace-regexp-in-string
                             "^(?" ""
                             (car (split-string clipboard-contents ":"))))
               (line-number-s (replace-regexp-in-string
                               ")?$" ""
                               (cadr (split-string clipboard-contents ":"))))
               (project (project-current t))
               (root (project-root project))
               (project-files-relative-names t)
               (all-files (project-files project))
               (setup-hook (lambda () (insert file-name-s)))
               (file
                (minibuffer-with-setup-hook setup-hook
                  (project--read-file-name
                   project                                       ; project
                   (format "Find file [%s]:"
                           (propertize line-number-s
                                       'face
                                       'font-lock-keyword-face)) ; prompt
                   all-files                                     ; all-files
                   nil                                           ; predicate
                   'file-name-history                            ; hist
                   root                                          ; mb-default
                   )))
               (ff (or ff-variant #'find-file)))
          (when file
            (funcall ff (expand-file-name file root))
            (when (and file-with-line-number-matchp
                       (equal (file-name-nondirectory file) file-name-s))
              (goto-line (string-to-number line-number-s))
              (recenter-top-bottom))))
      (project-find-file))))

(provide 'project-conf)
