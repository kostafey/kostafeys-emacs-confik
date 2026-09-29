;;; pijul-piclaude.el --- piclaude workspace/land commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The Pijul team
;; SPDX-License-Identifier: GPL-2.0-or-later

;; Thin wrappers over the `piclaude' launcher (contrib/agents/piclaude), the
;; parallel-agent fork workflow's write path to main. The heavy logic — record
;; in the fork, the land lock, pulling main into the fork, the conflict gate,
;; the push — all lives in the shell script; here we just drive it and surface
;; the two outcomes that matter: landed, or stopped on a conflict to resolve in
;; the fork. Contract-first, like the VSCode client.

;;; Code:

(require 'subr-x)

;; Provided by pijul.el; declared here so this file byte-compiles alone.
(declare-function pijul-repository-root "pijul" (&optional dir))

(defgroup pijul-piclaude nil
  "Parallel-agent Pijul workspaces via the `piclaude' launcher."
  :group 'pijul
  :prefix "pijul-piclaude-")

(defcustom pijul-piclaude-program "piclaude"
  "Name of, or path to, the `piclaude' launcher script.
It must support `land' and use a `pijul' with the `pull --no-notify' /
`push --no-notify' flags."
  :type 'string)

(defun pijul-piclaude--revert-visited (root)
  "Revert unmodified file buffers visiting files under ROOT.
A `piclaude land' pull can move files in the fork; refresh what we show,
without clobbering buffers you have unsaved edits in."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name
                   (not (buffer-modified-p))
                   (string-prefix-p root (expand-file-name buffer-file-name))
                   (file-exists-p buffer-file-name))
          (ignore-errors (revert-buffer :ignore-auto :noconfirm)))))))

;;;###autoload
(defun pijul-land (&optional amend)
  "Land the current Pijul fork onto its main repo via `piclaude land'.
Records in the fork, then under a lock pulls main in and pushes back, so
main stays buildable.  With a prefix argument (or non-nil AMEND), revise
the last change (`--amend') instead of recording a new one.

On a conflict `piclaude land' stops and leaves markers in the fork:
resolve them, then run this command again (it is re-runnable — the re-run
records your resolution and pushes)."
  (interactive "P")
  (let* ((repo (or (pijul-repository-root)
                   (user-error "Not inside a Pijul repository")))
         (default-directory (file-name-as-directory repo))
         (msg (string-trim
               (read-string (if amend
                                "Land — amend message (empty = keep existing): "
                              "Land — change message: ")))))
    (when (and (not amend) (string-empty-p msg))
      (user-error "A change message is required"))
    (let* ((args (append '("land")
                         (when amend '("--amend"))
                         (unless (string-empty-p msg) (list "-m" msg))))
           (buf (get-buffer-create "*pijul-land*"))
           status output)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; DESTINATION t captures both stdout and stderr — land prints its
          ;; progress and the conflict guidance on stderr.
          (setq status (apply #'call-process pijul-piclaude-program nil t nil args))
          (setq output (buffer-string))
          (goto-char (point-max)))
        (special-mode))
      ;; The pull may have moved files whether we landed or hit a conflict.
      (pijul-piclaude--revert-visited repo)
      (cond
       ((eq status 0)
        (message "Pijul: landed on main."))
       ((let ((case-fold-search t))     ; matches both "CONFLIT" and "conflicts"
          (string-match-p "conflict\\|conflit" output))
        (display-buffer buf)
        (message "Pijul land: conflict pulling main into the fork — resolve the markers, then run `pijul-land' again."))
       (t
        (display-buffer buf)
        (message "piclaude land failed (see *pijul-land*)."))))))

(provide 'pijul-piclaude)
;;; pijul-piclaude.el ends here
