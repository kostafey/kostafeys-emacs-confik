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

(defun eval-last-scala-expr ()
  (interactive)
  (let ((prev-str (string (preceding-char))))
    (save-excursion
      (let ((start (point)))
        (cond ((equal "}" prev-str)
               (progn
                 (cua-set-mark)
                 (backward-sexp)
                 (beginning-of-line)))
              ((equal ")" prev-str)
               (progn
                 (cua-set-mark)
                 (ignore-errors (backward-sexp 2))
                 (while (equal (string (preceding-char)) ".")
                   (backward-sexp))
                 (if (equal (format "%s" (preceding-sexp)) "new")
                     (backward-sexp))
                 (when (equal (format "%s" (preceding-sexp)) "=")
                   (backward-sexp)
                   (beginning-of-line))))
              (t
               (progn
                 (cua-set-mark)
                 (backward-sexp 1)
                 (flash-region start (point)))))
        (flash-region start (point))
        (ensime-inf-eval-region start (point))
        (setq deactivate-mark t)))))

(setq scala-indent:step 4)

(provide 'scala-conf)
