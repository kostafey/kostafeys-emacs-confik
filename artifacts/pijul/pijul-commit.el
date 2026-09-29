;;; pijul-commit.el --- Mode for the Pijul change/record buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The Pijul team
;; SPDX-License-Identifier: GPL-2.0-or-later

;; A major mode for Pijul's change-text format — the buffer you edit when
;; `pijul record' opens `$EDITOR' on a `.pijul-commit' file, and the same
;; format `pijul diff' prints. On top of font-locking it provides
;; point-following: as point moves across hunks, the matching span is pulsed
;; in the `pijul-context' companion buffer. That is the "another buffer with
;; that part highlighted" experience.
;;
;; It activates automatically on `.pijul-commit' files. For the live
;; `pijul record' flow, set `$EDITOR'/`$VISUAL' to an Emacs client (see
;; editors/emacs/README.md); the repository is found via `.pijul' detection,
;; falling back to the most recently visited Pijul repo (`pijul--last-repo').

;;; Code:

(require 'pijul-context)
(require 'pulse)

;; Provided by pijul.el; declared here so this file byte-compiles alone.
(defvar pijul--last-repo)
(declare-function pijul-repository-root "pijul" (&optional dir))

(defvar pijul-commit-hunk-header-re
  "^[0-9]+\\.[ \t]+[A-Za-z]+ in \"\\(.*?\\)\":\\([0-9]+\\)"
  "Regexp matching a hunk header.  Group 1 is the file, group 2 the line.")

(defvar-local pijul-commit-repository nil
  "Repository root associated with this change-text buffer.")

(defvar-local pijul-commit--last-hunk nil
  "The last hunk synced to the companion, to avoid redundant work.")

(defvar pijul-commit-font-lock-keywords
  `((,pijul-commit-hunk-header-re . font-lock-keyword-face)
    ("^#.*$" . font-lock-comment-face)
    ("^\\([A-Za-z_]+\\) = " 1 font-lock-variable-name-face)
    ("^\\+.*$" . pijul-context-added)
    ("^-.*$" . pijul-context-removed))
  "Font-lock keywords for `pijul-commit-mode'.")

(defun pijul-commit--repo ()
  "Best guess at the repository root for this buffer."
  (or pijul-commit-repository
      (setq pijul-commit-repository
            (or (pijul-repository-root)
                (and (boundp 'pijul--last-repo) pijul--last-repo)
                default-directory))))

(defun pijul-commit--hunk-at-point ()
  "Return a plist (:file :line :added :removed) for the hunk containing point.
:added / :removed are the hunk's inserted / deleted content lines."
  (save-excursion
    (beginning-of-line)
    (let ((hstart (if (looking-at pijul-commit-hunk-header-re)
                      (point)
                    (save-excursion
                      (when (re-search-backward pijul-commit-hunk-header-re nil t)
                        (point))))))
      (when hstart
        (goto-char hstart)
        (looking-at pijul-commit-hunk-header-re)
        (let ((file (match-string-no-properties 1))
              (line (string-to-number (match-string-no-properties 2)))
              (hend (save-excursion
                      (goto-char (match-end 0))
                      (if (re-search-forward pijul-commit-hunk-header-re nil t)
                          (match-beginning 0)
                        (point-max))))
              added removed)
          (forward-line 1)
          (while (< (point) hend)
            (cond
             ((looking-at "\\+ ?\\(.*\\)") (push (match-string-no-properties 1) added))
             ((looking-at "- ?\\(.*\\)") (push (match-string-no-properties 1) removed)))
            (forward-line 1))
          (list :file file :line line
                :added (nreverse added) :removed (nreverse removed)))))))

(defun pijul-commit--pulse-in-context (needle)
  "Find NEEDLE in the `pijul-context' buffer, recenter its windows and pulse it."
  (let ((cbuf (get-buffer pijul-context-buffer-name)))
    (when (and needle (> (length needle) 0) cbuf (buffer-live-p cbuf))
      (with-current-buffer cbuf
        (save-excursion
          (goto-char (point-min))
          (when (search-forward needle nil t)
            (let ((b (line-beginning-position))
                  (e (line-end-position)))
              (dolist (w (get-buffer-window-list cbuf nil t))
                (set-window-point w b)
                (with-selected-window w (recenter)))
              (pulse-momentary-highlight-region b e))))))))

(defun pijul-commit--sync ()
  "Follow point: pulse the current hunk's span in the companion buffer."
  (when (derived-mode-p 'pijul-commit-mode)
    (let ((hunk (pijul-commit--hunk-at-point)))
      (when (and hunk (not (equal hunk pijul-commit--last-hunk)))
        (setq pijul-commit--last-hunk hunk)
        ;; Prefer an added line (it appears verbatim as an `add' run in the
        ;; companion); fall back to a removed line (a `del' ghost).
        (pijul-commit--pulse-in-context
         (car (append (plist-get hunk :added) (plist-get hunk :removed))))))))

(defun pijul-commit--show-context ()
  "Render the `pijul-context' companion for this buffer's repository."
  (let ((default-directory (file-name-as-directory (pijul-commit--repo))))
    (ignore-errors (pijul-context-diff))))

;;;###autoload
(define-derived-mode pijul-commit-mode text-mode "Pijul-Commit"
  "Major mode for Pijul's change-text (record) buffer.
Point-following pulses the current hunk's span in the `pijul-context'
companion buffer."
  (setq-local font-lock-defaults '(pijul-commit-font-lock-keywords t))
  (setq-local comment-start "#")
  (add-hook 'post-command-hook #'pijul-commit--sync nil t)
  (pijul-commit--show-context))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pijul-commit\\'" . pijul-commit-mode))

;;;###autoload
(defun pijul-record-preview ()
  "Preview what `pijul record' would record, with a highlighted companion.
Shows the change text in `pijul-commit-mode' (so point-following works) and
opens the `pijul-context' record preview beside it.  Read-only: it does not
record anything."
  (interactive)
  (let* ((repo (or (pijul-repository-root)
                   (user-error "Not inside a Pijul repository")))
         (default-directory repo)
         (buf (get-buffer-create "*pijul-record-preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process pijul-context-program nil t nil "diff")
        (goto-char (point-min)))
      (pijul-commit-mode)
      (setq-local pijul-commit-repository repo)
      (setq buffer-read-only t))
    (pop-to-buffer buf)))

(provide 'pijul-commit)
;;; pijul-commit.el ends here
