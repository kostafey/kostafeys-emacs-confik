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

(defvar-local k/mode-line-total-lines nil
  "Total number of lines in the current buffer, for the mode line.
Nil until `k/mode-line-init-total-lines' has run in this buffer.")

(defun k/mode-line-update-total-lines (&rest _args)
  "Recount the lines of the current buffer.  Return the new count.
`count-lines' rather than `line-number-at-pos' on `point-max': the
latter counts line *separators*, so it is short by one whenever the
buffer does not end in a newline."
  (setq k/mode-line-total-lines (count-lines (point-min) (point-max))))

(defvar k/mode-line-total-lines-sync-limit (* 4 1024 1024)
  "Region size up to which the line count is refreshed on every change.
`count-lines' costs some 0.5 ms at this size -- imperceptible per
keystroke -- so there is nothing to gain by deferring it.  Past it the
recount waits for `k/mode-line-total-lines-idle-delay' instead.")

(defvar k/mode-line-total-lines-idle-delay 0.3
  "Idle seconds before recounting lines in an oversized buffer.")

(defvar-local k/mode-line-total-lines-timer nil
  "Pending idle timer for this buffer's line recount, if any.")

(defun k/mode-line-refresh-total-lines (&rest _args)
  "Refresh `k/mode-line-total-lines' after a change in the current buffer.
Ordinary buffers are recounted right away.  Past
`k/mode-line-total-lines-sync-limit' the recount is deferred until Emacs
goes idle, so a burst of keystrokes costs one pass over the buffer
instead of one per character."
  (if (<= (- (point-max) (point-min)) k/mode-line-total-lines-sync-limit)
      (k/mode-line-update-total-lines)
    (when (timerp k/mode-line-total-lines-timer)
      (cancel-timer k/mode-line-total-lines-timer))
    (setq k/mode-line-total-lines-timer
          (run-with-idle-timer
           k/mode-line-total-lines-idle-delay nil
           ;; The timer fires in whatever buffer happens to be current, so
           ;; carry ours along -- and it may well be gone by then.
           (lambda (buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq k/mode-line-total-lines-timer nil)
                 (k/mode-line-update-total-lines)
                 (force-mode-line-update))))
           (current-buffer)))))

(defun k/mode-line-init-total-lines ()
  "Start tracking the line count of the current buffer.  Return the count."
  ;; Recalculate only when the text has actually changed (insertion/deletion)
  (add-hook 'after-change-functions #'k/mode-line-refresh-total-lines nil t)
  (k/mode-line-update-total-lines))

(add-hook 'find-file-hook #'k/mode-line-init-total-lines)

;;-----------------------------------------------------------------------------
;; Git branch
;;
;; The mode line used to call `vc-mode-line', which does not so much display
;; the VC data as compute it: three `git' subprocesses on the first redisplay
;; of every buffer -- some 0.6 s on MS Windows, where spawning a process runs
;; about 190 ms -- and it does that inside redisplay, exactly while the file
;; is being put on screen.  `perfomance-conf.el' takes `vc-refresh-state' off
;; `find-file-hook' to dodge that very cost; the mode line quietly put it back.
;;
;; Most of it goes into the file *state* (`git status'), and the git-gutter
;; fringe already reports that, line by line.  What is left worth showing is
;; the branch -- and that is one short file read.

(defvar k/mode-line-git-branch-poll 1.0
  "Seconds a branch name is trusted before `HEAD' is looked at again.")

(defvar k/mode-line-git-branch-cache (make-hash-table :test 'equal)
  "Branch names keyed by `HEAD' file: HEAD -> (CHECKED-AT MTIME BRANCH).")

(defvar-local k/mode-line-git-head 'unset
  "The `HEAD' file that governs this buffer, nil when there is none.
`unset' until `k/mode-line-git-head-file' has looked it up.")

(defun k/mode-line-file-first-line (file)
  "Return the first line of FILE, trimmed, or nil if it cannot be read."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents file nil 0 200)
      (string-trim (buffer-substring-no-properties
                    (point-min) (line-end-position))))))

(defun k/mode-line-git-head-file ()
  "Return the `HEAD' file of the repository holding the current buffer.
Nil for a buffer visiting no file, or one outside a working tree.  The
answer is kept in the buffer: `locate-dominating-file' walks up the
directory tree, which is more than the mode line may redo on every
redisplay."
  (when (eq k/mode-line-git-head 'unset)
    (setq k/mode-line-git-head
          (when-let* ((file buffer-file-name)
                      (root (locate-dominating-file file ".git"))
                      (dot-git (expand-file-name ".git" root))
                      (git-dir
                       (if (file-directory-p dot-git)
                           dot-git
                         ;; A linked worktree or a submodule: `.git' is a
                         ;; file reading "gitdir: <path>".
                         (when-let* ((s (k/mode-line-file-first-line dot-git))
                                     (_ (string-prefix-p "gitdir: " s)))
                           (expand-file-name (string-trim (substring s 8))
                                             root)))))
            (expand-file-name "HEAD" git-dir))))
  k/mode-line-git-head)

(defun k/mode-line-git-branch ()
  "Return the branch of the current buffer's file, or nil.
A detached `HEAD' yields the abbreviated commit instead.  The value is
read straight from `.git/HEAD', cached per repository, and refreshed
only when that file's modification time changes -- and, since this runs
from the mode line, at most every `k/mode-line-git-branch-poll' seconds.
A `git checkout' therefore shows up within a second, at the price of one
`stat', and never a subprocess."
  (when-let* ((head (k/mode-line-git-head-file)))
    (let ((entry (or (gethash head k/mode-line-git-branch-cache)
                     (puthash head (list 0 nil nil)
                              k/mode-line-git-branch-cache)))
          (now (float-time)))
      (when (>= (- now (nth 0 entry)) k/mode-line-git-branch-poll)
        (setf (nth 0 entry) now)
        (let ((mtime (file-attribute-modification-time (file-attributes head))))
          (unless (equal mtime (nth 1 entry))
            (setf (nth 1 entry) mtime)
            (setf (nth 2 entry)
                  (when-let* ((s (and mtime (k/mode-line-file-first-line head))))
                    (if (string-prefix-p "ref: " s)
                        (let ((ref (substring s (length "ref: "))))
                          (if (string-prefix-p "refs/heads/" ref)
                              (substring ref (length "refs/heads/"))
                            ref))
                      (substring s 0 (min 7 (length s)))))))))
      (nth 2 entry))))

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
       (propertize "%02l" 'face 'font-lock-string-face) "/"
       ;; `find-file-hook' only covers buffers visiting a file, so the count
       ;; is nil in *scratch*, dired, magit, help, ... where `format' would
       ;; then signal mid-redisplay.  Start tracking on first display instead.
       '(:eval (propertize (format "%2d" (or k/mode-line-total-lines
                                             (k/mode-line-init-total-lines)))
                           'face 'font-lock-string-face))
       ","
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
       ;; git branch, read from `.git/HEAD' -- see above for why not `vc'
       '(:eval (when-let* ((branch (k/mode-line-git-branch)))
                 (propertize (concat " Git-" branch)
                             'face 'font-lock-constant-face
                             'help-echo (format "Git branch, from %s"
                                                (k/mode-line-git-head-file)))))
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
       ;; major mode; the mode symbol itself as a tool tip
       ;; `mode-name' is a mode-line construct, not necessarily a string --
       ;; hence `format-mode-line' rather than plain concatenation.
       '(:eval (propertize (format-mode-line mode-name)
                           'help-echo (format "Major mode: %s" major-mode)))
       " "
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
