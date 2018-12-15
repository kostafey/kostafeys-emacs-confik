;; ----------------------------------------------------------------------
;; ack
(add-to-list 
 'load-path
 (expand-file-name "ack/" third-party-lisp-path))

(defvar ack-src-file "ack-standalone.pl"
  "ack-filename.")

(defvar ack-src-file-path (find-file-in-load-path ack-src-file))
(when ack-src-file-path
  (require 'ack)
  (setq ack-command (concat "perl \"" ack-src-file-path 
                            "\" --nocolor --nogroup ")))

;; ----------------------------------------------------------------------
;; The Silver Searcher - ag
(when (require 'ag nil 'noerror)
  (setq ag-reuse-window 'nil)
  (setq ag-reuse-buffers 't)
  (setq ag-highlight-search t))

(provide 'ack-conf)
