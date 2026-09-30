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
             (message "pijul record: %s"
                      (with-current-buffer (process-buffer proc)
                        ;; The last line is "Hash: ..."; emacsclient's
                        ;; "Waiting for Emacs..." precedes it.
                        (or (car (last (split-string (buffer-string) "\n" t)))
                            "done")))
           (pop-to-buffer (process-buffer proc))))))))

(defun k/pijul-commit-finish ()
  "Save the change text and hand it back to `pijul record'."
  (interactive)
  (save-buffer)
  (server-edit))

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

;; `pijul-commit-mode' also edits `.pijul-commit' files during `pijul
;; record', where `q' and `c' must self-insert: bind them only in
;; read-only buffers such as `*pijul-record-preview*'.
(define-key pijul-commit-mode-map (kbd "q")
  '(menu-item "" k/pijul-commit-quit
              :filter (lambda (cmd) (and buffer-read-only cmd))))
(define-key pijul-commit-mode-map (kbd "c")
  '(menu-item "" k/pijul-record
              :filter (lambda (cmd) (and buffer-read-only cmd))))
;; Only while `pijul record' waits on this buffer via emacsclient.
(dolist (binding '(("C-c C-c" . k/pijul-commit-finish)
                   ("C-c C-k" . k/pijul-commit-cancel)))
  (define-key pijul-commit-mode-map (kbd (car binding))
    `(menu-item "" ,(cdr binding)
                :filter (lambda (cmd)
                          (and (bound-and-true-p server-buffer-clients) cmd)))))

(provide 'pijul-conf)
