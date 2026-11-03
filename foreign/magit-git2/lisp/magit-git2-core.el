;;; magit-git2-core.el --- Load the magit-git2 dynamic module  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Kostafey <kostafey@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Loads magit-git2-core.so (.dll on Windows), built by `zig build' in
;; the directory above this one.  Requiring this feature never fails:
;; when the module cannot be loaded, `magit-git2-core-error' says why
;; and `magit-git2-core-available-p' is nil.

;;; Code:

(declare-function magit-git2--release "magit-git2-core")

(defgroup magit-git2 nil
  "Answer Magit's read-only git queries in-process."
  :group 'magit-extensions)

(defcustom magit-git2-core-file
  (expand-file-name
   (concat "../zig-out/lib/magit-git2-core" module-file-suffix)
   (file-name-directory (or load-file-name buffer-file-name)))
  "The magit-git2-core dynamic module."
  :type 'file)

(defvar magit-git2-core-error nil
  "Why the module did not load, or nil.")

(defvar magit-git2-core--loaded-file nil
  "The file the module was last loaded from.")

(defun magit-git2-core-available-p ()
  "Return non-nil when the module is loaded."
  (featurep 'magit-git2-core-module))

(defun magit-git2-core-load (&optional file)
  "Load the module from FILE (default `magit-git2-core-file').
Return non-nil on success; on failure set `magit-git2-core-error'."
  (let ((file (or file magit-git2-core-file)))
    (setq magit-git2-core-error
          (cond ((not (and (fboundp 'module-load) module-file-suffix))
                 "this Emacs has no dynamic module support")
                ((not (file-exists-p file))
                 (format "%s does not exist; run `zig build'" file))
                ((condition-case err
                     (progn (module-load file)
                            (setq magit-git2-core--loaded-file file)
                            nil)
                   (error (error-message-string err))))))
    (null magit-git2-core-error)))

(defun magit-git2-core-reload ()
  "Load a fresh copy of the module, replacing the loaded functions.
Emacs never unloads a module, and loading the same file again does
nothing, so this loads a temporary copy of `magit-git2-core-file'."
  (let ((copy (make-temp-file "magit-git2-core" nil module-file-suffix)))
    (copy-file magit-git2-core-file copy t)
    (when (featurep 'magit-git2-core-module)
      (magit-git2--release))
    (prog1 (magit-git2-core-load copy)
      ;; The loaded copy stays mapped; on Windows it cannot be deleted.
      (ignore-errors (delete-file copy)))))

(unless (magit-git2-core-available-p)
  (magit-git2-core-load))

(provide 'magit-git2-core)

;;; magit-git2-core.el ends here
