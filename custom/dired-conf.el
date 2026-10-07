;;; -*- lexical-binding: t -*-
;;-------------------------------------------------------------------

;; Keep a single dired buffer: going into a directory (RET) or up (^)
;; replaces the current one instead of adding another.
(setq dired-kill-when-opening-new-dired-buffer t)

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

(global-set-key (kbd "<f5>") 'dired-open)

(defun copy-to-clipboard-dired-current-directory ()
  "Copy current directory path to the clipboard."
  (interactive)
  (let ((result (kill-new (dired-current-directory))))
    (message result)
    result))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-<down>") 'dired-find-file)
  (define-key dired-mode-map (kbd "C-<up>") 'dired-up-directory)
  (define-key dired-mode-map (kbd "M-p")
              'copy-to-clipboard-dired-current-directory)
  (define-key dired-mode-map (kbd "C-<home>") 'dired-home)
  (define-key dired-mode-map (kbd "C-<end>") 'dired-end))

(provide 'dired-conf)

;;; dired-conf.el ends here
