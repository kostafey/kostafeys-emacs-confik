;;; switch-to-emacsclient.el --- Get Emacs frame focus for Windows.

(when (eq system-type 'windows-nt)
  (defvar my-switch-counter 0)

  (defun my-find-touch-file ()
    (find-file "~/.emacs.d/touch")
    (rename-buffer "*touch*"))

  (defun my-iconify-or-deiconify-frame ()
    (interactive)
    (my-find-touch-file)
    (setq my-switch-counter 0)
    (iconify-or-deiconify-frame))
  (global-set-key (kbd "M-z") 'my-iconify-or-deiconify-frame)

  (defun my-window-configuration-change ()
    (when (string= (buffer-file-name)
                   (expand-file-name "~/.emacs.d/touch"))
      (if (> my-switch-counter 0)
          (progn
            (my-find-touch-file)
            (setq my-switch-counter 0)
            (my-previous-buffer))
        (progn
          (my-previous-buffer)
          (incf my-switch-counter)))))
  (add-hook 'window-configuration-change-hook
            'my-window-configuration-change))

(provide 'switch-to-emacsclient)
