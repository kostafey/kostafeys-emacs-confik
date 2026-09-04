(require 'functions)

(use-package rg
  :vc (:url "https://github.com/dajva/rg.el.git"
       :branch "master")
  :defer t)

;;----------------------------------------------------------------------
;; ack
(add-to-list
 'load-path
 (expand-file-name "~/.emacs.d/artifacts/ack/"))

;; Install for Fedora:
;; sudo dnf install perl-filetest
(defvar ack-src-file "ack-standalone.pl"
  "ack-filename.")

(defvar ack-src-file-path (find-file-in-load-path ack-src-file))
(when ack-src-file-path
  (require 'ack)
  (setq ack-command (concat "perl \"" ack-src-file-path
                            "\" --nocolor ")))

;;----------------------------------------------------------------------
;; ripgrep  - rg
(defun k/rg ()
  (interactive)
  (if (project-current)
      (command-execute 'rg-project)
    (command-execute 'rg)))

(setq rg-command-line-flags '("--no-messages")) ; Suppress all error messages.

(provide 'ack-conf)
