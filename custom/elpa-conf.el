
;; `package-conf' leaves `package-archives' empty -- everything is installed
;; from git -- so `use-elpa' cannot reach an archive as it stands, and the two
;; modules that call it (`lua-conf', `irc-conf') are both commented out of
;; `init.el'.  Reviving either one means either giving those packages a `:vc'
;; recipe or setting `package-archives' first.

(defun use-elpa (name)
  (when (not (require name nil 'noerror))
    (when (or (not (boundp 'package-archive-contents))
              (not package-archive-contents))
      (package-refresh-contents))
    (package-install name)))

(defun elpa-highlight-initialize ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("use-elpa\\b" . font-lock-keyword-face)
     ("use-elpa '\\(.*\\)[ )]" (1 font-lock-function-name-face)))))

(eval-after-load "elpa-conf"
  (lambda ()
    (elpa-highlight-initialize)))

(provide 'elpa-conf)
