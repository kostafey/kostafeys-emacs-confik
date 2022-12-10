
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("ELPA" . "https://tromey.com/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("m-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(defun use-elpa (name)
  (when (not (require name nil 'noerror))
    (when (or (not (boundp 'package-archive-contents))
              (not package-archive-contents))
      (package-refresh-contents))
    (package-install name)))

(defun elpa-highlight-initialize ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("use-elpa\\b" . font-lock-keyword-face))))

(eval-after-load "elpa-conf"
  (lambda ()
    (elpa-highlight-initialize)))

(provide 'elpa-conf)
