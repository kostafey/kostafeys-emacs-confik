
(when (require 'rust-mode nil 'noerror)
  ;; --------
  ;; flycheck
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

  ;; -------
  ;; compile
  (defun lcl:rust-compile-hook ()
    (require 'compile)
    (set (make-local-variable 'compile-command)
         (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
             "cargo run"
           (format "rustc %s && %s" (buffer-file-name)
                   (file-name-sans-extension (buffer-file-name))))))

  (setq-default compilation-read-command nil)
  (add-hook 'rust-mode-hook 'lcl:rust-compile-hook)

  (define-key rust-mode-map (kbd "C-c C-c") 'compile))

(provide 'rust-conf)
