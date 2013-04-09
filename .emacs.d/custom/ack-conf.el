(add-to-list 
 'load-path
 (expand-file-name "ack/" third-party-lisp-path))

(defvar ack-src-file "ack-standalone.pl"
  "ack-filename.")

(defvar ack-src-file-path (find-file-in-load-path ack-src-file))
(when ack-src-file-path
  (require 'ack)

  (setq ack-command (concat "perl \"" ack-src-file-path "\"")))

;; (defadvice after-ack (after ack)
;;   (switch-to-buffer "*ack*")
;;   )

;; (ad-activate 'after-ack)

;; (ad-disable-advice 'compilation-start 'after 'compilation-handle-exit)

(recentf-mode 1)
(setq recentf-max-saved-items 300)

(defun ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

(global-set-key (kbd "C-c f") 'ido-choose-from-recentf)
(global-set-key (kbd "C-c C-f") 'ack)

(provide 'ack-conf)
