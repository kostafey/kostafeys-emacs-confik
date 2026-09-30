;;; pijul-conf.el --- Pijul VCS integration. -*- lexical-binding: t -*-

(add-to-list 'load-path (concat site-lisp-path "artifacts/pijul/"))
(require 'pijul)
(global-pijul-mode 1)

(defun k/pijul-commit-quit ()
  "Kill the current `pijul-commit-mode' buffer and restore its window."
  (interactive)
  (quit-window t))

;; `pijul-commit-mode' also edits `.pijul-commit' files during `pijul
;; record', where `q' must self-insert: bind it only in read-only buffers
;; such as `*pijul-record-preview*'.
(define-key pijul-commit-mode-map (kbd "q")
  '(menu-item "" k/pijul-commit-quit
              :filter (lambda (cmd) (and buffer-read-only cmd))))

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

(provide 'pijul-conf)
