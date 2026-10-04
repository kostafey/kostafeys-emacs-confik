;;; -*- lexical-binding: t -*-
(use-package rg
  :straight (rg :type git :host github
                :repo "dajva/rg.el" :branch "master")
  :bind (("C-c r" . k/rg)
         ("<f7>" . k/rg)
         ("C-<f7>" . k/rg-file)))

;;----------------------------------------------------------------------
;; ripgrep  - rg
(defun k/rg ()
  (interactive)
  (if (project-current)
      (command-execute 'rg-project)
    (command-execute 'rg)))

(setq rg-command-line-flags '("--no-messages")) ; Suppress all error messages.

(defvar dired-buffers)

(defvar k/rg-file-history nil
  "History of file name patterns entered in `k/rg-file'.")

(defun k/rg-file (dir pattern)
  "Find files under DIR whose names match PATTERN; list them in Dired.
PATTERN is a case-insensitive glob; without wildcards it matches any
file name containing it.  The whole directory tree is searched,
including hidden files and files ignored by .gitignore."
  (interactive
   (list (read-directory-name "Search in: " nil nil t)
         (read-string "File name pattern: " nil 'k/rg-file-history)))
  (require 'dired)
  (let* ((root (file-name-as-directory (expand-file-name dir)))
         (glob (if (string-match-p "[*?[]" pattern)
                   pattern
                 (concat "*" pattern "*")))
         (files (with-temp-buffer
                  (setq default-directory root)
                  (apply #'call-process
                         (if (boundp 'rg-executable) rg-executable "rg")
                         nil t nil
                         `("--files" "--no-ignore" "--hidden" "--no-messages"
                           "--path-separator" "/"
                           "--glob-case-insensitive" "--glob" ,glob))
                  (sort (split-string (buffer-string) "\n" t) #'string<))))
    (if files
        (let ((name (format "*rg-file %s: %s*"
                            (abbreviate-file-name root) pattern)))
          (when (get-buffer name)
            (kill-buffer name))
          ;; Hide existing Dired buffers so a plain Dired buffer of ROOT
          ;; is not reused for the results, and keep the results buffer
          ;; out of `dired-buffers' so plain `dired' won't reuse it.
          ;; Don't omit found files (`dired-omit-mode' also fails on a
          ;; file list buffer when it has something to omit).
          (pop-to-buffer-same-window
           (with-current-buffer (let ((dired-buffers nil)
                                      (dired-mode-hook
                                       (remq 'dired-omit-mode dired-mode-hook)))
                                  (dired-noselect (cons root files)))
             (rename-buffer name)
             (current-buffer))))
      (message "No files matching \"%s\" in %s" pattern dir))))

(provide 'rg-conf)
