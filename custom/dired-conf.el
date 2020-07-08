;;-----------------------------------------------------------------------------
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

;; dired+
(when (require 'dired+ nil 'noerror)
  (toggle-diredp-find-file-reuse-dir t)

  (defun mydired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))

  (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (mydired-sort)))

(defun copy-to-clipboard-dired-current-directory ()
  (interactive)
  "Copy current directory path to the clipboard."
  (let ((result (kill-new (dired-current-directory))))
    (message result)
    result))

(provide 'dired-conf)

;;; dired-conf.el ends here
