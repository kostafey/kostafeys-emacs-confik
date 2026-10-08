;;; pijul-conf.el --- Pijul VCS integration. -*- lexical-binding: t -*-

(add-to-list 'load-path (concat site-lisp-path "artifacts/pijul/"))
(require 'pijul)
(global-pijul-mode 1)

;; `pijul-commit-mode' pops up the `*pijul-context*' palimpsest of the
;; working copy wherever it starts.  Keep it on demand only: C-c p d.
(advice-add 'pijul-commit--show-context :override #'ignore)

(defvar-local k/pijul-commit-change-hash nil
  "Hash of the recorded change this buffer shows; nil for the working copy.")

(defun k/pijul-commit-show-context ()
  "Show the `*pijul-context*' palimpsest of this buffer's change.
That is the recorded change in `*pijul-change: ...*', the working copy
elsewhere."
  (interactive)
  (let ((default-directory (file-name-as-directory (pijul-commit--repo))))
    (if k/pijul-commit-change-hash
        (pijul-context-change k/pijul-commit-change-hash)
      (pijul-context-diff))))

(defun k/pijul-commit-quit ()
  "Kill the current `pijul-commit-mode' buffer and restore its window."
  (interactive)
  (quit-window t))

(defface k/pijul-commit-added
  '((t :inherit diff-indicator-added))
  "Face for added (`+') lines in `pijul-commit-mode'.")

(defface k/pijul-commit-removed
  '((t :inherit diff-indicator-removed))
  "Face for removed (`-') lines in `pijul-commit-mode'.")

;; `pijul-commit-font-lock-keywords' names the `+'/`-' faces unquoted, so
;; font-lock evaluates them as (void) variables and never colors those
;; lines.  Swap in quoted faces of our own.
(require 'diff-mode)                    ; for `diff-indicator-*' faces
(setq pijul-commit-font-lock-keywords
      (mapcar (lambda (kw)
                (pcase (cdr kw)
                  ('pijul-context-added (cons (car kw) ''k/pijul-commit-added))
                  ('pijul-context-removed (cons (car kw) ''k/pijul-commit-removed))
                  (_ kw)))
              pijul-commit-font-lock-keywords))

(defconst k/pijul-ignore-patterns '("*~" "\\#*#" ".#*")
  "Entries `k/pijul-init' appends to a new repository's `.ignore'.
Emacs backups, auto-saves and lock files.  `.ignore' follows gitignore
syntax, so a leading `#' has to be escaped.")

(defun k/pijul-init (dir)
  "Create a Pijul repository in DIR and add Emacs junk to its `.ignore'.
`pijul init' itself writes `.ignore' with `.git' and `.DS_Store'."
  (interactive
   (list (read-directory-name "Pijul init in: " default-directory nil t)))
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (when-let ((root (pijul-repository-root dir)))
      (user-error "Already in a Pijul repository: %s" root))
    (with-temp-buffer
      (let ((default-directory dir))
        (unless (zerop (call-process pijul-context-program nil t nil "init"))
          (user-error "pijul init failed: %s" (string-trim (buffer-string))))))
    (let ((ignore (expand-file-name ".ignore" dir)))
      (with-temp-buffer
        (when (file-exists-p ignore)
          (insert-file-contents ignore))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (dolist (pattern k/pijul-ignore-patterns)
          (unless (save-excursion
                    (goto-char (point-min))
                    (re-search-forward
                     (concat "^" (regexp-quote pattern) "$") nil t))
            (insert pattern "\n")))
        (write-region nil nil ignore nil 'silent)))
    ;; Buffers already visiting files there missed `global-pijul-mode'.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name
                   (string-prefix-p dir (expand-file-name buffer-file-name)))
          (pijul-mode 1))))
    (message "Pijul repository created in %s" dir)))

(defun k/pijul-add (files)
  "Add FILES to Pijul version control; directories recursively.
Interactively, the marked files in Dired, otherwise a file read from the
minibuffer, defaulting to the current buffer's file."
  (interactive
   (list (if (derived-mode-p 'dired-mode)
             (dired-get-marked-files)
           (list (read-file-name "Pijul add: " nil buffer-file-name t
                                 (and buffer-file-name
                                      (file-name-nondirectory buffer-file-name)))))))
  (let* ((files (mapcar #'expand-file-name files))
         (root (or (pijul-repository-root (file-name-directory (car files)))
                   (user-error "Not inside a Pijul repository"))))
    (message "pijul add: %s"
             (string-trim (apply #'k/pijul--output root "add" "-r" files)))
    ;; Only when shown: refreshing pops the preview up.
    (when (get-buffer-window "*pijul-record-preview*" t)
      (k/pijul-record-preview-refresh root))))

(defun k/pijul-record (&optional all)
  "Record a change in the current Pijul repository.
Run `pijul record' asynchronously with this Emacs as its editor, so the
change text opens here in `pijul-commit-mode': delete the hunks to leave
out, fill in `message', save, then finish with \\[server-edit].

With prefix argument ALL, record every change without the editor,
reading the message from the minibuffer."
  (interactive "P")
  ;; A second run would open another change text, and keys typed while
  ;; the first one appears land in it (e.g. a doubled `c' in the preview).
  (when (process-live-p (get-process "pijul-record"))
    (let ((live (k/pijul-commit--live-buffer)))
      (when live (pop-to-buffer live))
      (user-error "pijul record is already running%s"
                  (if live "; finish (C-c C-c) or cancel (C-c C-k) it" ""))))
  (let* ((root (or (pijul-repository-root)
                   (user-error "Not inside a Pijul repository")))
         (default-directory root)
         (msg (when all
                (let ((m (string-trim (read-string "Record message: "))))
                  (if (string-empty-p m) (user-error "Empty message") m))))
         (buf (get-buffer-create "*pijul-record*"))
         ;; pijul looks the editor up by exact file name in PATH (no
         ;; PATHEXT, no quoting), hence `.exe' and our own bin dir first.
         (process-environment
          (append
           (list (concat "EDITOR=" (if (eq system-type 'windows-nt)
                                       "emacsclient.exe"
                                     "emacsclient"))
                 "VISUAL="
                 (concat "PATH=" (directory-file-name invocation-directory)
                         path-separator (getenv "PATH")))
           process-environment)))
    (require 'server)
    (unless (server-running-p) (server-start))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer)))
    (make-process
     :name "pijul-record"
     :buffer buf
     :command `(,pijul-context-program "record"
                ,@(when all (list "-a" "-m" msg)))
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (zerop (process-exit-status proc))
             (progn
               (k/pijul-git-gutter-refresh root)
               (k/pijul-record-preview-refresh root)
               (message "pijul record: %s"
                        (with-current-buffer (process-buffer proc)
                          ;; The last line is "Hash: ..."; emacsclient's
                          ;; "Waiting for Emacs..." precedes it.
                          (or (car (last (split-string (buffer-string) "\n" t)))
                              "done"))))
           (pop-to-buffer (process-buffer proc))))))))

(defun k/pijul--close-buffer (buf)
  "Kill BUF, if live.  Its windows go back to what they showed before,
or away if made for it."
  (when (buffer-live-p buf)
    (dolist (win (get-buffer-window-list buf nil t))
      (quit-restore-window win 'bury))
    (kill-buffer buf)))

(defun k/pijul-record-preview-refresh (root)
  "Bring `*pijul-record-preview*' of ROOT up to date after a record.
Close it when nothing is left to record."
  (let ((buf (get-buffer "*pijul-record-preview*")))
    (when (and buf
               (let ((repo (buffer-local-value 'pijul-commit-repository buf)))
                 (and repo (file-equal-p repo root))))
      (if (string-empty-p (string-trim (k/pijul--output root "diff")))
          (k/pijul--close-buffer buf)
        (let ((default-directory root))
          (pijul-record-preview))))))

(defun k/pijul-commit--live-buffer ()
  "The change-text buffer a running `pijul record' is waiting on, if any."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (and (derived-mode-p 'pijul-commit-mode)
                     (bound-and-true-p server-buffer-clients))))
            (buffer-list)))

(defun k/pijul-commit--bad-header ()
  "Position of the first non-comment line unless it is `message = \"...'.
Nil when the header looks right.  Catches stray keystrokes there, which
`pijul record' only answers by reopening the file with a comment."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp)) (looking-at-p "\\(#.*\\)?$"))
      (forward-line 1))
    (unless (looking-at-p "message = \"")
      (point))))

(defun k/pijul-commit-finish ()
  "Save the change text and hand it back to `pijul record'."
  (interactive)
  (let ((bad (k/pijul-commit--bad-header)))
    (when bad
      (goto-char bad)
      (user-error "The change text must start with message = \"...\"")))
  (save-buffer)
  (server-edit))

(defun k/pijul-commit--warn-syntax-error ()
  "Say so when `pijul record' reopens the change text after a parse error.
Its only report is a comment at the top of the file, easy to miss."
  (when (and (derived-mode-p 'pijul-commit-mode)
             (save-excursion
               (goto-char (point-min))
               (looking-at-p "# Syntax errors")))
    (message "%s" (propertize "pijul record: syntax error in the change text, fix it and C-c C-c again (C-c C-k cancels)"
                              'face 'warning))))

(add-hook 'server-visit-hook #'k/pijul-commit--warn-syntax-error)

(defun k/pijul-commit-cancel ()
  "Abort the `pijul record' waiting on this buffer; record nothing.
Handing back an untouched or emptied change text is not a safe way to
abort, so kill the `pijul record' process first, then the buffer and
its temporary file."
  (interactive)
  (let ((proc (get-process "pijul-record"))
        (file buffer-file-name))
    (when (process-live-p proc)
      (set-process-sentinel proc #'ignore)
      (delete-process proc))
    (set-buffer-modified-p nil)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer))
    (when (and file (file-exists-p file))
      (delete-file file))
    (message "pijul record cancelled")))

;; ------------------------------------------------------------
;; pijul log

(require 'parse-time)                  ; for `parse-iso8601-time-string'

(defvar k/pijul-log-limit 256
  "How many changes `k/pijul-log' shows at first, and `+' adds.")

(defface k/pijul-log-hash
  '((t :inherit font-lock-constant-face))
  "Face for change hashes in `k/pijul-log-mode'.")

(defface k/pijul-log-author
  '((t :inherit font-lock-variable-name-face))
  "Face for authors in `k/pijul-log-mode'.")

(defface k/pijul-log-date
  '((t :inherit font-lock-comment-face))
  "Face for dates in `k/pijul-log-mode'.")

(defvar-local k/pijul-log--root nil
  "Repository root this `k/pijul-log-mode' buffer shows the log of.")

(defvar-local k/pijul-log--count nil
  "How many changes this `k/pijul-log-mode' buffer shows.")

(defun k/pijul--output (root &rest args)
  "Run pijul with ARGS in ROOT; return its output, or signal on failure."
  (let ((default-directory root)
        (coding-system-for-read 'utf-8))
    (with-temp-buffer
      (unless (zerop (apply #'call-process pijul-context-program nil t nil args))
        (user-error "pijul %s failed: %s"
                    (string-join args " ") (string-trim (buffer-string))))
      (buffer-string))))

(defun k/pijul-log--author-name (author)
  "The name part of AUTHOR, a `Name (login) <email>' string."
  (string-trim
   (if (string-match "\\`\\([^(<]*\\)" author)
       (match-string 1 author)
     author)))

(defun k/pijul-log--entries (root count)
  "The last COUNT changes of ROOT as a list of alists, newest first.
`pijul log' prints \"No matching logs found\" before the JSON of an
empty log, so parse from the first `['."
  (let ((out (k/pijul--output root "log" "--output-format" "json"
                              "--limit" (number-to-string count))))
    (json-parse-string (substring out (or (string-search "[" out) 0))
                       :object-type 'alist :array-type 'list
                       :null-object nil)))

(defun k/pijul-log--set-margin (width)
  "Give the log buffer a right margin of WIDTH columns, in all its windows."
  (setq right-margin-width width)
  (dolist (win (get-buffer-window-list nil nil t))
    (set-window-margins win (car (window-margins win)) width)))

(defun k/pijul-log--revert (&rest _)
  "Render the log, one line per change, keeping point on its change."
  (let* ((inhibit-read-only t)
         (hash-at-point (get-text-property (line-beginning-position)
                                           'k/pijul-hash))
         (line (line-number-at-pos))
         (entries (k/pijul-log--entries k/pijul-log--root k/pijul-log--count))
         (rows (mapcar
                (lambda (e)
                  (let-alist e
                    (list .hash
                          (car (split-string (or .message "") "\n"))
                          (k/pijul-log--author-name (or (car .authors) ""))
                          (format-time-string
                           "%Y-%m-%d %H:%M"
                           (parse-iso8601-time-string .timestamp)))))
                entries))
         (author-width
          (min 20 (apply #'max 0 (mapcar (lambda (r) (string-width (nth 2 r)))
                                         rows)))))
    (k/pijul-log--set-margin (+ author-width 1 16 1))
    (erase-buffer)
    (if (null rows)
        (insert (propertize "No changes recorded" 'font-lock-face 'shadow))
      (dolist (r rows)
        (pcase-let ((`(,hash ,msg ,author ,date) r))
          (insert
           (propertize
            (concat
             ;; Author and date go to the right margin, as in magit: the
             ;; message is cut at the window edge, whatever its width.
             ;; At the line start, since a truncated line never displays
             ;; what lies past the window edge, margin specs included.
             (propertize
              " " 'display
              `((margin right-margin)
                ,(concat
                  (propertize (truncate-string-to-width
                               author author-width 0 ?\s "…")
                              'face 'k/pijul-log-author)
                  " "
                  (propertize date 'face 'k/pijul-log-date))))
             (propertize (substring hash 0 8) 'font-lock-face 'k/pijul-log-hash)
             " "
             (if (string-empty-p msg)
                 (propertize "(no message)" 'font-lock-face 'shadow)
               msg))
            'k/pijul-hash hash)
           "\n")))
      (when (= (length rows) k/pijul-log--count)
        (insert (propertize "Type + to show more history\n"
                            'font-lock-face 'shadow))))
    (goto-char (point-min))
    (let ((pos (and hash-at-point
                    (text-property-any (point-min) (point-max)
                                       'k/pijul-hash hash-at-point))))
      (if pos
          (goto-char pos)
        (forward-line (1- line))))))

(defun k/pijul-log-more ()
  "Show `k/pijul-log-limit' more changes."
  (interactive)
  (setq k/pijul-log--count (+ k/pijul-log--count k/pijul-log-limit))
  (revert-buffer))

(defvar k/pijul-log-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'k/pijul-log-show-change)
    (define-key m (kbd "n") #'next-line)
    (define-key m (kbd "p") #'previous-line)
    (define-key m (kbd "+") #'k/pijul-log-more)
    m)
  "Keymap for `k/pijul-log-mode'.")

(define-derived-mode k/pijul-log-mode special-mode "Pijul-Log"
  "Major mode for `pijul log' output, one line per change.
\\{k/pijul-log-mode-map}"
  (setq truncate-lines t)
  (setq-local revert-buffer-function #'k/pijul-log--revert))

(defun k/pijul-log ()
  "Show `pijul log' of the current repository, one line per change.
RET shows the change at point, + shows more history, g refreshes."
  (interactive)
  (let* ((root (or (pijul-repository-root)
                   (and (derived-mode-p 'pijul-commit-mode)
                        (pijul-commit--repo))
                   (user-error "Not inside a Pijul repository")))
         (buf (get-buffer-create
               (format "*pijul-log: %s*"
                       (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buf
      (k/pijul-log-mode)
      (setq k/pijul-log--root root
            k/pijul-log--count k/pijul-log-limit
            default-directory root))
    ;; Displayed first: the message column is fitted to the window.
    (pop-to-buffer buf)
    (k/pijul-log--revert)
    (goto-char (point-min))))

(defun k/pijul-log-show-change ()
  "Show the change at point with `pijul change', in `pijul-commit-mode'."
  (interactive)
  (let* ((hash (or (get-text-property (line-beginning-position) 'k/pijul-hash)
                   (user-error "No change at point")))
         (root k/pijul-log--root)
         (buf (get-buffer-create (format "*pijul-change: %s*"
                                         (substring hash 0 10)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (default-directory root))
        (erase-buffer)
        (insert (k/pijul--output root "change" hash))
        (goto-char (point-min))
        (pijul-commit-mode)
        (setq-local pijul-commit-repository root)
        (setq k/pijul-commit-change-hash hash
              default-directory root
              buffer-read-only t)))
    (pop-to-buffer buf)))
;; ------------------------------------------------------------

;; ------------------------------------------------------------
;; Visit the change at point, as RET and C-x d do in magit-status

(declare-function k/display-buffer-in-next-window "version-control")

(defun k/pijul-commit--hunk-location ()
  "Return (FILE LINE COLUMN) the hunk line at point changes.
FILE is absolute.  The `:LINE' of a hunk header counts in the working
copy, where its `+' lines follow one another; a `-' line or a header
line stands for the line the hunk starts at."
  (let ((root (or pijul-commit-repository default-directory))
        (column (let ((column (current-column)))
                  (save-excursion
                    (beginning-of-line)
                    (if (looking-at-p "[-+] ") (max 0 (- column 2)) 0)))))
    (save-excursion
      (let ((here (line-beginning-position)))
        (end-of-line)
        (unless (re-search-backward "^[0-9]+\\. " nil t)
          (user-error "No change at point"))
        (let* ((header (buffer-substring-no-properties
                        (point) (line-end-position)))
               (location
                (cond
                 ((string-match " in \"\\(.*?\\)\":\\([0-9]+\\)" header)
                  (cons (match-string 1 header)
                        (string-to-number (match-string 2 header))))
                 ((string-match "File addition: \"\\(.*?\\)\" in \"\\(.*?\\)\""
                                header)
                  (cons (if (string-empty-p (match-string 2 header))
                            (match-string 1 header)
                          (concat (match-string 2 header) "/"
                                  (match-string 1 header)))
                        1))
                 (t (user-error "No file in this change"))))
               (added 0))
          (forward-line 1)
          (while (< (point) here)
            (when (looking-at-p "\\+ ") (setq added (1+ added)))
            (forward-line 1))
          (list (expand-file-name (car location) root)
                (+ (cdr location) added)
                column))))))

(defun k/pijul-commit--visit (other-window)
  "Visit the file the hunk line at point changes, there.
In the next window when OTHER-WINDOW, cf. `k/display-buffer-in-next-window'."
  (pcase-let* ((`(,file ,line ,column) (k/pijul-commit--hunk-location))
               (buffer (find-file-noselect file)))
    (if other-window
        (let ((display-buffer-overriding-action
               (list #'k/display-buffer-in-next-window)))
          (pop-to-buffer buffer))
      (pop-to-buffer-same-window buffer))
    (unless (file-directory-p file)
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column))))

(defun k/pijul-commit-visit-file ()
  "Visit the file of the change at point, at the changed line."
  (interactive)
  (k/pijul-commit--visit nil))

(defun k/pijul-commit-visit-file-other-window ()
  "Visit the file of the change at point, at the changed line, in the
next window.  Cf. `k/magit-diff-visit-worktree-file-other-window'."
  (interactive)
  (k/pijul-commit--visit t))

;; `pijul-commit-mode' also edits `.pijul-commit' files during `pijul
;; record', where `q', `c', `l', `d' and RET must self-insert: bind them
;; only in read-only buffers such as `*pijul-record-preview*'.
(define-key pijul-commit-mode-map (kbd "q")
  '(menu-item "" k/pijul-commit-quit
              :filter (lambda (cmd) (and buffer-read-only cmd))))
(define-key pijul-commit-mode-map (kbd "c")
  '(menu-item "" k/pijul-record
              :filter (lambda (cmd) (and buffer-read-only cmd))))
(define-key pijul-commit-mode-map (kbd "l")
  '(menu-item "" k/pijul-log
              :filter (lambda (cmd) (and buffer-read-only cmd))))
(define-key pijul-commit-mode-map (kbd "d")
  '(menu-item "" k/pijul-commit-show-context
              :filter (lambda (cmd) (and buffer-read-only cmd))))
(define-key pijul-commit-mode-map (kbd "RET")
  '(menu-item "" k/pijul-commit-visit-file
              :filter (lambda (cmd) (and buffer-read-only cmd))))
(define-key pijul-commit-mode-map (kbd "C-x d")
  '(menu-item "" k/pijul-commit-visit-file-other-window
              :filter (lambda (cmd) (and buffer-read-only cmd))))
;; Only while `pijul record' waits on this buffer via emacsclient.
(dolist (binding '(("C-c C-c" . k/pijul-commit-finish)
                   ("C-c C-k" . k/pijul-commit-cancel)))
  (define-key pijul-commit-mode-map (kbd (car binding))
    `(menu-item "" ,(cdr binding)
                :filter (lambda (cmd)
                          (and (bound-and-true-p server-buffer-clients) cmd)))))

;; ------------------------------------------------------------
;; git-gutter for Pijul
;;
;; git-gutter dispatches on the backend in `git-gutter:vcs-check-function'
;; and `git-gutter:start-diff-process1'; teach both about `pijul'.  The
;; diff is the recorded version (`pijul reset --dry-run FILE') against
;; the file, in the `-U0' unified format git-gutter parses, produced by
;; `git diff --no-index' (works outside any git repository).
;; git-gutter runs on every `switch-to-buffer', a `consult' preview
;; included, and merely starting pijul takes about 0.1s on Windows, so
;; `pijul reset' runs asynchronously ahead of the diff, and only once the
;; pristine changed since its last run for the file.  Every pijul command
;; touches the pristine, `pijul reset' included, so the cache tells its
;; own touches from those of other commands.

(defvar k/pijul-git-gutter--old nil
  "File the git-gutter diff under Pijul compares the visited file with.
Bound by `k/pijul-git-gutter-start-diff-process' around the diff.")

(defvar k/pijul-git-gutter--cache (make-hash-table :test #'equal)
  "Recorded versions of files, and the pristines they were read from.
A file maps to (GENERATION . RECORDED): RECORDED is the file its
recorded version was written to, nil when the file is not recorded.
A pristine maps to (MTIME . GENERATION): MTIME is its modification
time right after the last `pijul reset --dry-run' here, which touches
it itself, as every pijul command does.  A pristine touched by anything
else since starts a new GENERATION, which invalidates its files.")

(defun k/pijul-git-gutter--pristine-mtime (pristine)
  "Modification time of the PRISTINE database file, nil if none."
  (and pristine
       (file-attribute-modification-time (file-attributes pristine))))

(defun k/pijul-git-gutter--cached (file pristine)
  "Return the valid `k/pijul-git-gutter--cache' entry of FILE, or nil."
  (let ((state (gethash pristine k/pijul-git-gutter--cache))
        (entry (gethash file k/pijul-git-gutter--cache)))
    (and state entry
         (equal (car state) (k/pijul-git-gutter--pristine-mtime pristine))
         (eql (car entry) (cdr state))
         (or (null (cdr entry)) (file-exists-p (cdr entry)))
         entry)))

(defun k/pijul-git-gutter--cache-put (file pristine before recorded)
  "Cache RECORDED for FILE read from PRISTINE, whose mtime was BEFORE."
  (let* ((state (gethash pristine k/pijul-git-gutter--cache))
         (generation (if (and state (equal (car state) before))
                         (cdr state)
                       (1+ (or (cdr state) 0)))))
    (puthash pristine
             (cons (k/pijul-git-gutter--pristine-mtime pristine) generation)
             k/pijul-git-gutter--cache)
    (puthash file (cons generation recorded) k/pijul-git-gutter--cache)))

(defun k/pijul-git-gutter--recorded-file (file)
  "Temporary file for FILE's recorded (pristine) version.
Named after FILE rather than kept in a buffer-local variable, which a
major mode change would wipe, leaking the file."
  (expand-file-name (concat "pijul-gutter-" (md5 (expand-file-name file)))
                    temporary-file-directory))

(defun k/pijul-git-gutter--cleanup ()
  (when buffer-file-name
    (let ((recorded (k/pijul-git-gutter--recorded-file buffer-file-name)))
      (when (file-exists-p recorded)
        (delete-file recorded)))))

(defun k/pijul-git-gutter-start-diff (file proc-buf)
  "Start the git-gutter diff process for FILE under Pijul.
FILE is compared with `k/pijul-git-gutter--old', or with itself."
  (let ((file (expand-file-name file)))
    (start-process "git-gutter" proc-buf
                   "git" "--no-pager" "-c" "core.autocrlf=false"
                   "diff" "--no-index" "--no-color" "--no-ext-diff" "-U0"
                   "--" (or k/pijul-git-gutter--old file) file)))

(defun k/pijul-git-gutter--reset (file pristine callback)
  "Write the recorded version of FILE asynchronously, then call CALLBACK.
CALLBACK gets the file holding the recorded version, or nil when FILE
is not recorded yet (untracked or only `pijul add'ed).  The result is
kept in `k/pijul-git-gutter--cache' along with the state of PRISTINE."
  (let ((recorded (k/pijul-git-gutter--recorded-file file))
        (before (k/pijul-git-gutter--pristine-mtime pristine))
        (out (generate-new-buffer " *pijul-gutter*" t)))
    (make-process
     :name "pijul-gutter"
     :buffer out
     :command (list pijul-context-program "reset" "--dry-run" file)
     :coding 'no-conversion
     :noquery t
     :stderr (get-buffer-create " *pijul-gutter-stderr*")
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((ok (and (eq (process-status proc) 'exit)
                        (zerop (process-exit-status proc)))))
           (when ok
             (with-current-buffer out
               (let ((coding-system-for-write 'no-conversion))
                 (write-region nil nil recorded nil 'silent))))
           (kill-buffer out)
           (k/pijul-git-gutter--cache-put file pristine before
                                          (and ok recorded))
           (funcall callback (and ok recorded))))))))

(defun k/pijul-git-gutter-start-diff-process (orig curfile proc-buf)
  "Call ORIG with CURFILE and PROC-BUF once the recorded file is written.
PROC-BUF already exists meanwhile, which keeps `git-gutter' from
starting another diff of the same file.  The recorded file of the last
run is reused while `k/pijul-git-gutter--cache' holds it valid."
  (if (not (eq git-gutter:vcs-type 'pijul))
      (funcall orig curfile proc-buf)
    (add-hook 'kill-buffer-hook #'k/pijul-git-gutter--cleanup nil t)
    (let* ((curbuf (current-buffer))
           (file (expand-file-name curfile))
           (pristine (when-let* ((root (pijul-repository-root)))
                       (expand-file-name ".pijul/pristine/db" root)))
           (cached (k/pijul-git-gutter--cached file pristine))
           ;; Not recorded yet: diff the file against itself, i.e. show
           ;; no marks, as git does.
           (diff (lambda (recorded)
                   (if (not (and (buffer-live-p curbuf)
                                 (buffer-live-p proc-buf)))
                       (when (buffer-live-p proc-buf)
                         (kill-buffer proc-buf))
                     (with-current-buffer curbuf
                       (let ((k/pijul-git-gutter--old recorded))
                         (condition-case err
                             (funcall orig curfile proc-buf)
                           (error
                            (kill-buffer proc-buf)
                            (message "git-gutter: %s"
                                     (error-message-string err))))))))))
      (if cached
          (funcall diff (cdr cached))
        (k/pijul-git-gutter--reset file pristine diff)))))

(defun k/pijul-git-gutter-check (orig vcs)
  (if (eq vcs 'pijul)
      (and (pijul-repository-root) t)
    (funcall orig vcs)))

(defun k/pijul-git-gutter-dispatch (orig file proc-buf)
  (if (eq git-gutter:vcs-type 'pijul)
      (k/pijul-git-gutter-start-diff file proc-buf)
    (funcall orig file proc-buf)))

(defun k/pijul-git-gutter-refresh (root)
  "Redraw git-gutter marks in buffers visiting files under ROOT."
  ;; The pristine just changed, though possibly within the same mtime
  ;; tick as the last `pijul reset' here.
  (clrhash k/pijul-git-gutter--cache)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (bound-and-true-p git-gutter-mode)
                 buffer-file-name
                 ;; Not `string-prefix-p': drive letter case and 8.3
                 ;; short names differ on Windows.
                 (file-in-directory-p buffer-file-name root))
        (git-gutter)))))

(with-eval-after-load 'git-gutter
  ;; First, so a Pijul repository inside a git work tree wins.
  (add-to-list 'git-gutter:handled-backends 'pijul)
  (advice-add 'git-gutter:vcs-check-function
              :around #'k/pijul-git-gutter-check)
  (advice-add 'git-gutter:start-diff-process1
              :around #'k/pijul-git-gutter-dispatch)
  (advice-add 'git-gutter:start-diff-process
              :around #'k/pijul-git-gutter-start-diff-process))
;; ------------------------------------------------------------

(provide 'pijul-conf)
