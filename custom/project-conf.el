;;; project-conf.el -- Built-in project.el custom configuration. -*- lexical-binding: t -*-

(require 'project)
(require 'rx)

(defconst k/project-file-line-regexp
  (rx bos
      (? (any "(["))
      (group (+ (not (any ":()[]" blank "\n")))  ; a bare name, or a path
             "." (+ alnum))                      ; ... ending in an extension
      ":"
      (group (+ digit)))                         ; the line number
  "Match \"FILE.EXT:LINE\" at the start of a string.
Group 1 is the file name -- a bare name or a path -- and group 2 is the
line number.  A leading paren or bracket is tolerated, and so is any
trailing text, so that grep hits and compiler errors match as well:

  filename.scala:123
  (filename.scala:123)
  src/main/filename.scala:123:45: error: ...
  /home/user/project/src/main/filename.scala:123

An extension is required, so that a plain \"word:123\" does not match.")

(defun k/project-find-file (&optional ff-variant)
  "Jump to a project's file using completion.
With FF-VARIANT set to a defun, use that instead of `find-file'.
A typical example of such a defun would be `find-file-other-window' or
`find-file-other-frame'.

Use clipboard text as an input string.  When it begins with a file name
followed by a colon and a line number -- see `k/project-file-line-regexp'
for the shapes accepted -- seed the completion with that name and jump to
that line, e.g.:
filename.scala:123
(filename.scala:123)"
  (interactive "P")
  (let* ((clipboard-contents (when kill-ring (current-kill 0)))
         (file-with-line-number-matchp
          (and clipboard-contents
               (string-match k/project-file-line-regexp clipboard-contents))))
    (if file-with-line-number-matchp
        ;; NB: read the groups first, before anything clobbers the match data.
        (let* ((file-name-s (match-string 1 clipboard-contents))
               (line-number-s (match-string 2 clipboard-contents))
               (project (project-current t))
               (root (project-root project))
               ;; The completion candidates are project-relative, so an
               ;; absolute name -- as pasted from a compiler error -- has to be
               ;; brought back to that form to match any of them.  A name from
               ;; some other tree is no use either; fall back to its base name,
               ;; which the `substring' completion style still finds.
               (seed (let ((abs-root (expand-file-name root))
                           (abs-name (expand-file-name file-name-s)))
                       (cond ((not (file-name-absolute-p file-name-s))
                              file-name-s)
                             ((string-prefix-p abs-root abs-name)
                              (file-relative-name abs-name abs-root))
                             (t (file-name-nondirectory file-name-s)))))
               (project-files-relative-names t)
               (all-files (project-files project))
               ;; `project-files-relative-names' makes `project-files' hand
               ;; back names relative to the project, but the reader below
               ;; turns them back into absolute ones against
               ;; `default-directory' -- not against the project.  Without
               ;; this binding the result points next to the current buffer,
               ;; so `find-file' opens an empty buffer on a path that does not
               ;; exist.  `project-find-file-in' binds it for the same reason.
               (default-directory root)
               (completion-ignore-case read-file-name-completion-ignore-case)
               (setup-hook (lambda () (insert seed)))
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
            ;; Only jump when the file picked is the one the clipboard named.
            (when (equal (file-name-nondirectory file)
                         (file-name-nondirectory file-name-s))
              (goto-char (point-min))
              (forward-line (1- (string-to-number line-number-s)))
              (recenter-top-bottom))))
      (project-find-file))))

(provide 'project-conf)
