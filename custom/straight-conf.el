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

;;-------------------------------------------------------------------
;; GNU ELPA core packages
;;
;; These ship both inside Emacs and on GNU ELPA, so two copies exist side by
;; side.  A current `eglot' needs the ELPA ones -- it asks for flymake 1.4.2
;; while Emacs 30.2 bundles 1.3.7 -- and enforces that with
;; `require-with-check': if the feature was already loaded from the built-in
;; file, it errors out with "Feature `flymake' is now provided by a different
;; file", which surfaces as a `File mode specification error' in every LSP
;; buffer.  Whoever loads the built-in copy first wins, and that used to be
;; whichever unrelated package happened to `require' it during init.
;;
;; Claiming them here settles the race: straight puts their build directories
;; on `load-path' before any other module is loaded, so every later `require'
;; resolves to the ELPA copy.
;;
;; `eldoc' and `seq' are left out on purpose -- they are preloaded into Emacs,
;; `eglot' merely reloads them, and swapping them this early is asking for
;; trouble.
(dolist (pkg '(flymake xref project jsonrpc external-completion))
  (straight-use-package pkg))
;;-------------------------------------------------------------------

(defun straight-highlight-initialize ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("straight-use-package\\b" . font-lock-keyword-face)
     ("straight-use-package '\\(.*\\)[ )]" (1 font-lock-function-name-face)))))

(eval-after-load "straight-conf"
  (lambda ()
    (straight-highlight-initialize)))

(provide 'straight-conf)
