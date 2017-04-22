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
    (cond ((equal "}" prev-str)
           (let ((start (point)))
             (save-excursion
               (cua-set-mark)
               (backward-sexp)
               (beginning-of-line)
               (flash-region start (point))
               (ensime-inf-eval-region start (point))
               (setq deactivate-mark t))))
          ((equal ")" prev-str)
           (let ((start (point)))
             (save-excursion
               (backward-sexp)
               (ensime-inf-eval-region start (point)))))
          (t (ensime-inf-eval-definition)))))

(setq scala-indent:step 4)

(provide 'scala-conf)
