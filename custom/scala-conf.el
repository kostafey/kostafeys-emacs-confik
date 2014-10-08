(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(defun eval-last-scala-expr ()
  (interactive)
  (let ((start (point)))
    (save-excursion
      (backward-sexp)
      (beginning-of-line)
      (ensime-inf-eval-region start (point)))))


(provide 'scala-conf)
