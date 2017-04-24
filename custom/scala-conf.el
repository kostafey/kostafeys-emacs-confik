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

(defun flash-region (start end &optional timeout)
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

(defun eval-last-scala-expr ()
  (interactive)
  (let ((prev-str (string (preceding-char))))
    (save-excursion
      (let ((start (point)))
        (cua-set-mark)
        (backward-sexp 1)
        (cond ((equal "}" prev-str)
               (beginning-of-line))
              ((equal ")" prev-str)
               (progn
                 (ignore-errors (backward-sexp 1))
                 (k/ensime-skip-sexp ".")
                 (k/ensime-skip-sexp "new")
                 (k/ensime-skip-line "=")))
              (t
               (progn
                 (k/ensime-skip-sexp ".")
                 (k/ensime-skip-sexp "import"))))
        (flash-region start (point))
        (ensime-inf-eval-region start (point))
        (setq deactivate-mark t)))))

(setq scala-indent:step 4)

(provide 'scala-conf)
