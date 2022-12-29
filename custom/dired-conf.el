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

;;-------------------------------------------------------------------
;; dired-single
;; Reuse the current dired buffer to visit another directory rather
;; than creating a new buffer for the new directory.
(use-package dired-single
  :ensure t
  :config
  (progn
    (defun my-dired-init ()
      "Bunch of stuff to run for dired, either immediately or when it's
       loaded."
      ;; <add other stuff here>
      (define-key dired-mode-map [remap dired-find-file]
        'dired-single-buffer)
      (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
        'dired-single-buffer-mouse)
      (define-key dired-mode-map [remap dired-up-directory]
        'dired-single-up-directory))

    ;; if dired's already loaded, then the keymap will be bound
    (if (boundp 'dired-mode-map)
        ;; we're good to go; just add our bindings
        (my-dired-init)
      ;; it's not loaded yet, so add our bindings to the load-hook
      (add-hook 'dired-load-hook 'my-dired-init))))

(provide 'dired-conf)

;;; dired-conf.el ends here
