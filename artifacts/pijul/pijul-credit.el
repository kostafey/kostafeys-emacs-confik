;;; pijul-credit.el --- Per-line Pijul change attribution overlays -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The Pijul team
;; SPDX-License-Identifier: GPL-2.0

;; This is a proof-of-concept vertical slice of a future Pijul editor mode.
;; It shells out to
;;
;;     pijul credit --output-format json FILE
;;
;; and paints each line of the current buffer with a colour derived from the
;; change that introduced it -- like `magit-blame' or VC annotate, but using
;; Pijul's *native* per-line attribution instead of re-deriving it.
;;
;; The whole point of the exercise: all the interesting work lives in the Rust
;; CLI's JSON contract, so this client stays tiny and the same JSON drives a
;; VSCode decoration provider with equally little code.
;;
;; Status: experimental.  Requires Emacs 28+ (for native `json-parse-buffer')
;; and the `pijul' binary on PATH.
;;
;; Usage:
;;   M-x pijul-credit         annotate the current file-visiting buffer
;;   M-x pijul-credit-clear   remove the annotations
;;   hover a line to see the full change hash(es) in the echo area / tooltip.

;;; Code:

(require 'color)
(require 'seq)

(defgroup pijul-credit nil
  "Per-line Pijul change attribution."
  :group 'tools
  :prefix "pijul-credit-")

(defcustom pijul-credit-program "pijul"
  "Name of, or path to, the pijul executable."
  :type 'string)

(defcustom pijul-credit-saturation 0.5
  "Saturation (0..1) of the per-change background tints."
  :type 'number)

(defcustom pijul-credit-lightness 0.85
  "Lightness (0..1) of the per-change background tints.
Kept high so buffer text stays readable over the tint."
  :type 'number)

(defvar-local pijul-credit--overlays nil
  "Overlays created by `pijul-credit' in the current buffer.")

(defun pijul-credit--color (hash)
  "Return a stable pale background colour (a hex string) for change HASH.
The hue is a deterministic function of HASH's contents, so a given change
always gets the same colour across files, buffers and sessions."
  (let* ((sum (seq-reduce #'+ (append hash nil) 0))
         (hue (/ (float (mod sum 360)) 360.0))
         (rgb (color-hsl-to-rgb hue
                                pijul-credit-saturation
                                pijul-credit-lightness)))
    (apply #'color-rgb-to-hex (append rgb '(2)))))

(defun pijul-credit--run (file)
  "Run `pijul credit --output-format json' on FILE, returning parsed entries.
The result is a list of alists, one per entry in the JSON stream."
  (with-temp-buffer
    (let ((status (call-process pijul-credit-program nil t nil
                                "credit" "--output-format" "json" file)))
      (unless (eq status 0)
        (error "pijul credit failed: %s" (string-trim (buffer-string))))
      (goto-char (point-min))
      (json-parse-buffer :array-type 'list :object-type 'alist))))

;;;###autoload
(defun pijul-credit-clear ()
  "Remove all Pijul credit overlays from the current buffer."
  (interactive)
  (mapc #'delete-overlay pijul-credit--overlays)
  (setq pijul-credit--overlays nil))

;;;###autoload
(defun pijul-credit ()
  "Annotate the current buffer with per-line Pijul change attribution."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (pijul-credit-clear)
  (let ((entries (pijul-credit--run buffer-file-name)))
    (save-excursion
      (dolist (entry entries)
        ;; We only care about "line" entries here; "conflict" markers are
        ;; carried in the same stream for a richer UI later on.
        (when (equal (alist-get 'type entry) "line")
          (let* ((start   (alist-get 'startLine entry))
                 (count   (alist-get 'lineCount entry))
                 (changes (alist-get 'changes entry))
                 ;; Colour by the first (sorted) contributing change; the
                 ;; tooltip lists them all.
                 (hash    (car changes)))
            (when (and start hash)
              (goto-char (point-min))
              (forward-line (1- start))
              (let ((beg (line-beginning-position)))
                (forward-line count)
                (let ((ov (make-overlay beg (point))))
                  (overlay-put ov 'face `(:background ,(pijul-credit--color hash)))
                  (overlay-put ov 'help-echo (string-join changes "\n"))
                  (overlay-put ov 'pijul-credit t)
                  (push ov pijul-credit--overlays))))))))
    (message "Pijul credit: %d span(s)" (length pijul-credit--overlays))))

(provide 'pijul-credit)
;;; pijul-credit.el ends here
