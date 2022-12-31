;;----------------------------------------------------------------------
;; ack
(require 'elpa-conf)
(use-elpa 'ag)
(use-elpa 'rg)
(require 'functions)

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
;; The Silver Searcher - ag
(when (require 'ag nil 'noerror)
  (setq ag-reuse-window 'nil)
  (setq ag-reuse-buffers 't)
  (setq ag-highlight-search t)

  (defun k/ag (string directory)
    (interactive
     (list (ag/read-from-minibuffer "Search string")
           (read-directory-name "Directory: "
                                (ag/project-root default-directory))))
    (ag/search string directory)))

;;----------------------------------------------------------------------
;; ripgrep  - rg

(setq rg-command-line-flags '("--no-messages")) ; Suppress all error messages.

(provide 'ack-conf)
