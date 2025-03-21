;;-------------------------------------------------------------------
;; dired
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")
              (seq bol "." eol))))

(add-hook 'dired-mode-hook 'dired-omit-mode)

(defun dired-home ()
  "Go to dots .. line."
  (interactive)
  (k/buffer-beginning)
  (k/char-forward)
  (k/char-forward)
  (k/line-next))

(defun dired-end ()
  "Go to the last of selectable directory items."
  (interactive)
  (k/buffer-end)
  (k/line-previous)
  (k/line-end))

(defun dired-open ()
  "Open or obtain focus of the dired buffer."
  (interactive)
  (let ((current-dir default-directory))
    (if (eframe-pop-buffer 'dired-mode)
        (progn
          (eframe-kill-buffer)
          (dired current-dir))
      (dired current-dir))))

(defun copy-to-clipboard-dired-current-directory ()
  (interactive)
  "Copy current directory path to the clipboard."
  (let ((result (kill-new (dired-current-directory))))
    (message result)
    result))

(provide 'dired-conf)

;;; dired-conf.el ends here
