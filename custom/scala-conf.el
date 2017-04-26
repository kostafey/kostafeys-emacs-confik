(require 'ensime)

;; Usage:

;; 1. Run from console:

;; echo "
;; addSbtPlugin(\"org.ensime\" % \"sbt-ensime\" % \"1.12.9\")
;; addSbtPlugin(\"io.get-coursier\" % \"sbt-coursier\" % \"1.0.0-RC1\")
;; " >> ~/.sbt/0.13/plugins/plugins.sbt

;; 2. sbt ensimeConfig
;; 3. M-x ensime
;; 4. C-c C-v z
;; 5. M-x ensime-inf-eval-region

(defun k/ensime-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun k/ensime-skip-sexp (val)
  (ignore-errors
    (while (or
            (equal (string (preceding-char)) val)
            (equal (format "%s" (preceding-sexp)) val))
      (backward-sexp))))

(defun k/ensime-skip-line (val)
  (ignore-errors
    (while (or
            (equal (string (preceding-char)) val)
            (equal (format "%s" (preceding-sexp)) val))
      (backward-sexp)
      (beginning-of-line))))

(defun k/ensime-eval-last-scala-expr ()
  (interactive)
  (let ((prev-str (string (preceding-char))))
    (save-excursion
      (let ((start (point)))
        (cua-set-mark)
        (backward-sexp 1)
        (cond ((equal "}" prev-str)
               (ignore-errors (backward-sexp 1))
               (beginning-of-line))
              ((or
                (equal ")" prev-str)
                (equal "]" prev-str))
               (progn
                 (ignore-errors (backward-sexp 1))
                 (k/ensime-skip-sexp ".")
                 (k/ensime-skip-sexp "new")
                 (k/ensime-skip-line "=")))
              (t
               (progn
                 (k/ensime-skip-sexp ".")
                 (k/ensime-skip-sexp "import"))))
        (k/ensime-flash-region start (point))
        (ensime-inf-eval-region start (point))
        (setq deactivate-mark t)))))

(defun k/ensime-eval-buffer ()
  (interactive)
  (save-excursion
    (k/ensime-flash-region (point-max) (point-min))
    (cua-set-mark)
    (ensime-inf-eval-region (point-max) (point-min))
    (setq deactivate-mark t)))

(defun k/ensime-quit ()
  (interactive)
  (let* ((mask "*ENSIME")
         (buffers (buffer-list))
         (l (length mask)))
    (while buffers
      (with-current-buffer (car buffers)
        (when
            (and (>= (length (buffer-name)) l)
                 (equal (substring (buffer-name) 0 l) mask))
          (kill-buffer)))
      (setq buffers (cdr buffers))))
  (message "ENSIME buffers closed."))

(setq scala-indent:step 4)

(provide 'scala-conf)
