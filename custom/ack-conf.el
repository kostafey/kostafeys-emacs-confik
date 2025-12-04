(require 'functions)

(straight-use-package
 '(rg
   :type git :host github
   :repo "dajva/rg.el" :branch "master"))

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
  (if (projectile-project-root)
      (command-execute 'rg-project)
    (command-execute 'rg)))

(setq rg-command-line-flags '("--no-messages")) ; Suppress all error messages.

(provide 'ack-conf)
