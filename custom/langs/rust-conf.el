(when (require 'rust-mode nil 'noerror)
  ;; --------
  ;; flycheck
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

  ;; -------
  ;; compile
  (require 'compile)

  (defun k/rust-compile-and-run ()
    (set (make-local-variable 'compile-command)
         (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
             "cargo run"
           (format "rustc %s && %s" (buffer-file-name)
                   (file-name-sans-extension (buffer-file-name))))))

  (defun k/rust-compile ()
    (set (make-local-variable 'compile-command)
         (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
             "cargo build"
           (format "rustc %s" (buffer-file-name)))))

  (defun k/rust-compile-release ()
    (interactive)
    (set (make-local-variable 'compile-command)
         (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
             "cargo build --release"
           (format "rustc %s" (buffer-file-name))))
    (compile compile-command)
    (k/rust-compile))

  (setq-default compilation-read-command nil)
  (add-hook 'rust-mode-hook 'k/rust-compile)
  (define-key rust-mode-map (kbd "C-c C-c") 'compile)
  (define-key rust-mode-map (kbd "C-x c") 'k/rust-compile-release)

  ;; --------------------------------
  ;; autocompletion & code navigation
  ;; `install:' cargo install racer
  ;;            rustup component add rust-src
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)

  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 0)

  ;; ----------
  ;; formatting
  ;; `install:' rustup component add rustfmt-preview
  (define-key rust-mode-map (kbd "C-c <tab>") #'rust-format-buffer)
  )

(provide 'rust-conf)
