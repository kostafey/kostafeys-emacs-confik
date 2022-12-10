;;-------------------------------------------------------------------
;; History
;; To have a menu of recently opened files
(recentf-mode 1)
(setq recentf-max-saved-items 300)

(defun ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;; Mode for automatic saving of minibuffer history.
(savehist-mode 1)

;;-------------------------------------------------------------------
;; session

;; (desktop-save-mode t) save session before exit (default)
;; Save file buffer encoding
(add-to-list 'desktop-locals-to-save 'buffer-file-coding-system)

;;-------------------------------------------------------------------
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;;-------------------------------------------------------------------
;;; save minibuffer history between sessions
(savehist-mode t)

(provide 'history-conf)

