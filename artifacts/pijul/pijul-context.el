;;; pijul-context.el --- Full-context palimpsest view of a Pijul change -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The Pijul team
;; SPDX-License-Identifier: GPL-2.0-or-later

;; Renders the "palimpsest" view of a change: the whole file with the change
;; inlined in context, in three colours — added, removed (ghost), and
;; added-then-removed. It consumes the CLI's structured output:
;;
;;   pijul change <hash> --context     a committed change  (files[].vertices[])
;;   pijul diff --context              the uncommitted record-time preview
;;                                     (files[].segments[])
;;
;; Both shapes reduce to an ordered list of {kind, text} runs, so one renderer
;; serves both. This is the "another buffer with the change highlighted"
;; companion: run `pijul-context-diff' while you have a `.pijul-commit' record
;; buffer open and you see exactly what you are about to record, in context.
;;
;; Status: experimental. Requires Emacs 28+ and `pijul' on PATH.

;;; Code:

(require 'seq)

(defgroup pijul-context nil
  "Full-context palimpsest view of a Pijul change."
  :group 'tools
  :prefix "pijul-context-")

(defcustom pijul-context-program "pijul"
  "Name of, or path to, the pijul executable."
  :type 'string)

(defface pijul-context-added
  '((t :inherit diff-added))
  "Face for text the change added (still alive).")

(defface pijul-context-removed
  '((t :inherit diff-removed :strike-through t))
  "Face for text the change removed, shown as an inline ghost.")

(defface pijul-context-obsolete
  '((t :inherit shadow :strike-through t))
  "Face for text the change added that a later change removed.")

(defface pijul-context-file
  '((t :inherit diff-file-header))
  "Face for the per-file header line.")

(defvar pijul-context-buffer-name "*pijul-context*"
  "Name of the buffer showing the palimpsest view.")

(defun pijul-context--run (args)
  "Run pijul with ARGS (a list) in `default-directory'; return parsed JSON."
  (with-temp-buffer
    (let ((status (apply #'call-process pijul-context-program nil t nil args)))
      (unless (eq status 0)
        (error "pijul %s failed: %s"
               (string-join args " ") (string-trim (buffer-string))))
      (goto-char (point-min))
      (json-parse-buffer :array-type 'list :object-type 'alist :null-object nil))))

(defun pijul-context--files (data)
  "Normalise DATA from either entry point to a list of (PATH . RUNS).
Each run is a cons (KIND . TEXT); KIND is a string, TEXT may be nil."
  (mapcar
   (lambda (file)
     (cons (alist-get 'path file)
           ;; `change --context' calls them vertices, `diff --context'
           ;; segments; both are ordered {kind, text} runs.
           (mapcar (lambda (r) (cons (alist-get 'kind r) (alist-get 'text r)))
                   (or (alist-get 'vertices file) (alist-get 'segments file)))))
   (alist-get 'files data)))

(defun pijul-context--face (kind)
  "Face for a run of the given KIND (a string), or nil for context."
  (pcase kind
    ("add" 'pijul-context-added)
    ("del" 'pijul-context-removed)
    ("obs" 'pijul-context-obsolete)
    (_ nil)))

(defun pijul-context--render (files title)
  "Render FILES (from `pijul-context--files') into the view buffer.
TITLE is shown at the top."
  (let ((buf (get-buffer-create pijul-context-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (concat title "\n\n") 'face 'bold))
        (dolist (file files)
          (insert (propertize (concat "── " (car file) "\n") 'face 'pijul-context-file))
          (dolist (run (cdr file))
            (let ((kind (car run))
                  (text (cdr run)))
              (cond
               ((equal kind "skip") (insert (propertize "    ⋮\n" 'face 'shadow)))
               ((null text) nil) ; lazily-omitted vertex: nothing to show
               (t (insert (propertize text 'face (pijul-context--face kind)))))))
          (insert "\n")))
      (goto-char (point-min))
      (view-mode 1))
    (display-buffer buf)
    buf))

;;;###autoload
(defun pijul-context-diff ()
  "Show the record-time palimpsest preview of the working copy."
  (interactive)
  (pijul-context--render
   (pijul-context--files (pijul-context--run '("diff" "--context")))
   "Pijul record preview (working copy)"))

;;;###autoload
(defun pijul-context-change (hash)
  "Show the full-context palimpsest of committed change HASH."
  (interactive "sChange hash (empty = latest): ")
  (let ((args (if (string-empty-p hash)
                  '("change" "--context")
                (list "change" "--context" hash))))
    (pijul-context--render
     (pijul-context--files (pijul-context--run args))
     (format "Pijul change %s" (if (string-empty-p hash) "(latest)" hash)))))

(provide 'pijul-context)
;;; pijul-context.el ends here
