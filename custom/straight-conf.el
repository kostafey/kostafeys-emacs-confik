(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun straight-highlight-initialize ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("straight-use-package\\b" . font-lock-keyword-face)
     ("straight-use-package '\\(.*\\)[ )]" (1 font-lock-function-name-face)))))

(eval-after-load "straight-conf"
  (lambda ()
    (straight-highlight-initialize)))

(provide 'straight-conf)
