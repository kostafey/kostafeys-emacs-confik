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

;; (defadvice after-ack (after ack)
;;   (switch-to-buffer "*ack*")
;;   )

;; (ad-activate 'after-ack)

;; (ad-disable-advice 'compilation-start 'after 'compilation-handle-exit)

(provide 'ack-conf)
