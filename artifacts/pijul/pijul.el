;;; pijul.el --- Pijul VCS integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The Pijul team
;; SPDX-License-Identifier: GPL-2.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://nest.pijul.com/pijul/pijul

;; The umbrella entry point. Loading this pulls in the feature modules
;; (`pijul-credit', `pijul-context', `pijul-commit') and provides:
;;
;;   * `pijul-repository-root'  — find the `.pijul' repo above a directory;
;;   * `pijul-mode'             — a minor mode with keybindings, auto-enabled
;;                                in any file inside a Pijul repository;
;;   * `global-pijul-mode'      — turn that on everywhere.
;;
;; Quick start (see editors/emacs/README.md):
;;   (add-to-list 'load-path "/path/to/pijul/editors/emacs")
;;   (require 'pijul)
;;   (global-pijul-mode 1)

;;; Code:

(require 'subr-x)

(defgroup pijul nil
  "Integration with the Pijul version control system."
  :group 'tools
  :prefix "pijul-")

(defvar pijul--last-repo nil
  "Root of the most recently visited Pijul repository.
Used as a fallback by buffers (such as the `.pijul-commit' record
buffer) whose own `default-directory' is not inside the repository.")

;;;###autoload
(defun pijul-repository-root (&optional dir)
  "Return the Pijul repository root at or above DIR (default `default-directory').
That is the nearest ancestor directory containing a `.pijul' entry, or nil."
  (when-let* ((root (locate-dominating-file (or dir default-directory)
                                            ".pijul")))
    (expand-file-name root)))

(defvar pijul-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c p b") #'pijul-credit)         ; blame / annotate
    (define-key m (kbd "C-c p d") #'pijul-context-diff)   ; record preview
    (define-key m (kbd "C-c p c") #'pijul-context-change) ; view a change
    (define-key m (kbd "C-c p r") #'pijul-record-preview) ; record buffer + companion
    (define-key m (kbd "C-c p l") #'pijul-land)           ; land the fork onto main
    ;; (define-key m (kbd "C-c p k") #'pijul-carve)          ; carve: pick hunks & record
    m)
  "Keymap for `pijul-mode'.")

;;;###autoload
(define-minor-mode pijul-mode
  "Minor mode for buffers inside a Pijul repository.
Provides the `C-c p' keybindings and remembers the repository so the
record buffer can find it later."
  :lighter " Pijul"
  :keymap pijul-mode-map
  (when pijul-mode
    (setq pijul--last-repo (or (pijul-repository-root) pijul--last-repo))))

(defun pijul-mode--maybe-enable ()
  "Enable `pijul-mode' if this file-visiting buffer is in a Pijul repository."
  (when (and buffer-file-name (pijul-repository-root))
    (pijul-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-pijul-mode
  pijul-mode pijul-mode--maybe-enable
  :group 'pijul)

(require 'pijul-credit)
(require 'pijul-context)
(require 'pijul-commit)
;; (require 'pijul-carve)
(require 'pijul-piclaude)

(provide 'pijul)
;;; pijul.el ends here
