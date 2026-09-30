;;; pijul-conf.el --- Pijul VCS integration. -*- lexical-binding: t -*-

(add-to-list 'load-path (concat site-lisp-path "artifacts/pijul/"))
(require 'pijul)
(global-pijul-mode 1)

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
               (message "pijul record: %s"
                        (with-current-buffer (process-buffer proc)
                          ;; The last line is "Hash: ..."; emacsclient's
                          ;; "Waiting for Emacs..." precedes it.
                          (or (car (last (split-string (buffer-string) "\n" t)))
                              "done"))))
           (pop-to-buffer (process-buffer proc))))))))

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

(defvar k/pijul-log-font-lock-keywords
  '(("^Change \\([A-Z0-9]+\\)" 1 'font-lock-constant-face)
    ("^\\(?:Author\\|Date\\): .*$" . 'font-lock-comment-face))
  "Font-lock keywords for `k/pijul-log-mode'.")

(defvar-local k/pijul-log--root nil
  "Repository root this `k/pijul-log-mode' buffer shows the log of.")

(defun k/pijul--output (root &rest args)
  "Run pijul with ARGS in ROOT; return its output, or signal on failure."
  (let ((default-directory root)
        (coding-system-for-read 'utf-8))
    (with-temp-buffer
      (unless (zerop (apply #'call-process pijul-context-program nil t nil args))
        (user-error "pijul %s failed: %s"
                    (string-join args " ") (string-trim (buffer-string))))
      (buffer-string))))

(defun k/pijul-log--revert (&rest _)
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (insert (k/pijul--output k/pijul-log--root "log"))
    (goto-char (point-min))
    (forward-line (1- line))))

(defvar k/pijul-log-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'k/pijul-log-show-change)
    (define-key m (kbd "n") #'k/pijul-log-next)
    (define-key m (kbd "p") #'k/pijul-log-previous)
    m)
  "Keymap for `k/pijul-log-mode'.")

(define-derived-mode k/pijul-log-mode special-mode "Pijul-Log"
  "Major mode for `pijul log' output.
\\{k/pijul-log-mode-map}"
  (setq-local font-lock-defaults '(k/pijul-log-font-lock-keywords t))
  (setq-local revert-buffer-function #'k/pijul-log--revert))

(defun k/pijul-log ()
  "Show `pijul log' of the current repository.
RET shows the change at point, n/p move between changes, g refreshes."
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
            default-directory root)
      (k/pijul-log--revert)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun k/pijul-log-next ()
  "Move to the next change."
  (interactive)
  (end-of-line)
  (if (re-search-forward "^Change " nil t)
      (beginning-of-line)
    (message "No more changes")))

(defun k/pijul-log-previous ()
  "Move to the previous change."
  (interactive)
  (beginning-of-line)
  (unless (re-search-backward "^Change " nil t)
    (message "No previous change")))

(defun k/pijul-log-show-change ()
  "Show the change at point with `pijul change', in `pijul-commit-mode'."
  (interactive)
  (let* ((hash (save-excursion
                 (end-of-line)
                 (if (re-search-backward "^Change \\([A-Z0-9]+\\)" nil t)
                     (match-string-no-properties 1)
                   (user-error "No change at point"))))
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
        (setq default-directory root
              buffer-read-only t)))
    (pop-to-buffer buf)))
;; ------------------------------------------------------------

;; `pijul-commit-mode' also edits `.pijul-commit' files during `pijul
;; record', where `q', `c' and `l' must self-insert: bind them only in
;; read-only buffers such as `*pijul-record-preview*'.
(define-key pijul-commit-mode-map (kbd "q")
  '(menu-item "" k/pijul-commit-quit
              :filter (lambda (cmd) (and buffer-read-only cmd))))
(define-key pijul-commit-mode-map (kbd "c")
  '(menu-item "" k/pijul-record
              :filter (lambda (cmd) (and buffer-read-only cmd))))
(define-key pijul-commit-mode-map (kbd "l")
  '(menu-item "" k/pijul-log
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
  "Start the git-gutter diff process for FILE under Pijul."
  (add-hook 'kill-buffer-hook #'k/pijul-git-gutter--cleanup nil t)
  (let* ((file (expand-file-name file))
         (recorded (k/pijul-git-gutter--recorded-file file))
         ;; Not recorded yet (untracked or only `pijul add'ed): diff the
         ;; file against itself, i.e. show no marks, as git does.
         (old (if (zerop (call-process pijul-context-program nil
                                       (list :file recorded) nil
                                       "reset" "--dry-run" file))
                  recorded
                file)))
    (start-process "git-gutter" proc-buf
                   "git" "--no-pager" "-c" "core.autocrlf=false"
                   "diff" "--no-index" "--no-color" "--no-ext-diff" "-U0"
                   "--" old file)))

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
              :around #'k/pijul-git-gutter-dispatch))
;; ------------------------------------------------------------

(provide 'pijul-conf)
