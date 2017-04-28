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

(defun k/ensime-check-package (pname)
  (when (equal (format "%s" (preceding-sexp)) "package")
    (k/ensime-flash-region (line-beginning-position)
                           (line-end-position))
    (ensime-inf-send-string (concat "import " pname "._"))
    t))

(defun k/ensime-inf-eval-region (start end)
  "Send current region to Scala interpreter."
  (interactive "r")
  (ensime-inf-assert-running)
  (let* ((reg (trim-string
               (buffer-substring-no-properties start end)))
         ;; remove package ... line
         (reg (if (= (string-match "package" reg) 0)
                  (substring reg (1+ (string-match "\n" reg)))
                reg)))
    (with-current-buffer ensime-inf-buffer-name
      (goto-char (point-max))
      (comint-send-string nil ":paste\n")
      (comint-send-string nil reg)
      (comint-send-string nil "\n")
      (comint-send-eof))))

(cl-defun k/ensime-eval-last-scala-expr ()
  (interactive)
  (let ((prev-str (string (preceding-char))))
    (save-excursion
      (let ((start (point)))
        (backward-sexp 1)
        (cond ((equal "}" prev-str)
               (ignore-errors (backward-sexp 1))
               (beginning-of-line))
              ((or
                (equal ")" prev-str)
                (equal "]" prev-str))
               (progn
                 (if (not (= (current-column) 0))
                     (ignore-errors (backward-sexp 1)))
                 (k/ensime-skip-sexp ".")
                 (k/ensime-skip-sexp "new")
                 (k/ensime-skip-line "=")
                 (k/ensime-skip-line "case")
                 (k/ensime-skip-line "class")))
              (t
               (progn
                 (k/ensime-skip-sexp ".")
                 (k/ensime-skip-sexp "import")
                 (when (k/ensime-check-package
                        (buffer-substring start (point)))
                   (return-from k/ensime-eval-last-scala-expr)))))
        (k/ensime-flash-region start (point))
        (k/ensime-inf-eval-region start (point))))))

(defun k/quit-by-mask (mask mgs)
  (let* ((buffers (buffer-list))
         (l (length mask)))
    (while buffers
      (with-current-buffer (car buffers)
        (when
            (and (>= (length (buffer-name)) l)
                 (equal (substring (buffer-name) 0 l) mask))
          (kill-buffer)))
      (setq buffers (cdr buffers))))
  (if mgs (message mgs)))

(defun k/ensime-quit ()
  (interactive)
  (k/quit-by-mask "*ENSIME" "ENSIME buffers closed."))

(defun k/repl-quit ()
  (interactive)
  (k/quit-by-mask "*Scala REPL" nil))

(defun k/ensime-compile ()
  (interactive)
  (let ((buf (current-buffer)))
    (save-excursion
      (sbt-command "compile")
      (save-excursion
        (let ((beg)
              (end)))
        (beginning-of-buffer)
        (forward-sexp)
        (setq beg (point))
        (end-of-line)
        (setq end (point))
        (let ((pkg (concat
                    "import "
                    (trim-string
                     (buffer-substring beg end))
                    "._")))
          (k/repl-quit)
          (ensime-inf-switch)
          (sit-for 0.5)
          (ensime-inf-send-string pkg)
          (message pkg))))))

(defun k/ensime-config ()
  (interactive)
  (sbt-command "ensimeConfig"))

(defun k/ensime-eval-buffer ()
  (interactive)
  (save-excursion
    (k/ensime-flash-region (point-max) (point-min))
    (k/ensime-inf-eval-region (point-max) (point-min))))

(setq scala-indent:step 4)

(provide 'scala-conf)
