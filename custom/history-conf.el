;;-------------------------------------------------------------------
;; History
;; To have a menu of recently opened files
(recentf-mode 1)
(setq recentf-max-saved-items 300)

(defun choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (completing-read "Open file: " recentf-list nil t)))

;;-------------------------------------------------------------------
;; session
;; Save session before exit
(desktop-save-mode t)
;; Save file buffer encoding
(add-to-list 'desktop-locals-to-save 'buffer-file-coding-system)

;;-------------------------------------------------------------------
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(provide 'history-conf)

