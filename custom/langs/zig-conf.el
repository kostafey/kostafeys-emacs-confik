;;; zig-conf.el --- Zig configuration  -*- lexical-binding: t -*-

;; zig itself is taken from PATH, which `exec-path-from-shell' copies
;; from the login shell (shell-conf).

;;----------------------------------------------------------------------
;; zig-ts-mode
;;
;; zig-ts-mode pins its own grammar recipe and offers to install it the
;; first time a .zig or .zon file is opened; M-x zig-ts-install-grammars
;; installs it on demand, C-u M-x zig-ts-install-grammars reinstalls.
(use-package zig-ts-mode
  :straight `(zig-ts-mode
              :type git :host nil
              :repo ,(pcase system-type
                       ('windows-nt
                        "https://github.com/emacsmirror/zig-ts-mode.git")
                       ('gnu/linux
                        "git@github.com:emacsmirror/zig-ts-mode.git"))
              :branch "master")
  :bind (:map zig-ts-mode-map
         ("C-c C-c" . zig-ts-build))
  :config
  ;; Markdown in doc comments needs the split markdown grammar of
  ;; tree-sitter-grammars: zig-ts-mode queries the `inline' nodes of its
  ;; block half, which the `markdown' grammar of ikatyang has not got.
  ;; So the feature is on only where that query compiles.
  (setq zig-ts-markdown-enable
        (and (ignore-errors
               (treesit-query-compile 'markdown '((inline) @inline) t))
             t)))

;; ZLS formats with `zig fmt'.
(with-eval-after-load 'zig-ts-mode
  (define-key zig-ts-mode-map (kbd "C-c <tab>") #'eglot-format-buffer))

;;----------------------------------------------------------------------
;; ZLS
;;
;; eglot runs the zls it finds on `exec-path', its default entry in
;; `eglot-server-programs', and never installs it: `k/zls-install'
;; downloads the release made for the Zig on PATH.  A ZLS release
;; supports only its own Zig version, so rerun it after upgrading Zig.

(defvar k/zls-install-dir
  (file-name-as-directory (expand-file-name "~/.local/bin"))
  "Directory `k/zls-install' unpacks zls into.")

;; On PATH on Linux, seldom on Windows.
(add-to-list 'exec-path k/zls-install-dir t)

(defun k/zls-asset ()
  "Return the name of the zls release archive for this machine."
  (let ((arch (car (split-string system-configuration "-"))))
    (pcase system-type
      ('windows-nt (format "zls-%s-windows.zip" arch))
      ('darwin (format "zls-%s-macos.tar.xz" arch))
      (_ (format "zls-%s-linux.tar.xz" arch)))))

(defun k/zls-install ()
  "Download the zls release for `zig version' into `k/zls-install-dir'.
curl runs in the background, then the archive is unpacked.  A
development build of Zig has no release to match.  On Windows shut
eglot down before updating: a running zls.exe cannot be overwritten."
  (interactive)
  (let* ((version (car (process-lines "zig" "version")))
         (asset (k/zls-asset))
         (url (concat "https://github.com/zigtools/zls/releases/download/"
                      version "/" asset))
         (windows (eq system-type 'windows-nt))
         (archive (make-temp-file "zls-" nil
                                  (if windows ".zip" ".tar.xz")))
         (exe (if windows "zls.exe" "zls"))
         (dir k/zls-install-dir)
         ;; The bsdtar shipped with Windows: a GNU tar of Git or MSYS2
         ;; found first on PATH does not read zip.
         (tar (if windows
                  (expand-file-name "System32/tar.exe" (getenv "SystemRoot"))
                "tar"))
         (log (get-buffer-create "*zls-install*")))
    (when (string-search "-dev" version)
      (delete-file archive)
      (user-error "No zls release for Zig %s, a development build" version))
    (make-directory dir t)
    (message "Downloading %s..." url)
    (make-process
     :name "zls-install"
     :buffer log
     :command (list "curl" "-fsSL" "-o" archive url)
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (if (and (zerop (process-exit-status process))
                  (zerop (call-process tar nil log nil
                                       "-xf" archive "-C" dir exe)))
             (message "Installed zls %s"
                      (car (process-lines (expand-file-name exe dir)
                                          "--version")))
           (message "zls install failed, see %s" (buffer-name log)))
         (delete-file archive))))))

;; Semantic tokens only for what takes semantic analysis: comments,
;; keywords, strings and the like stay with tree-sitter, whose Markdown
;; in doc comments the tokens of a whole comment would paint over.
(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration
                (plist-put (default-value 'eglot-workspace-configuration)
                           :zls '(:semantic_tokens "partial"))))

(add-hook 'zig-ts-mode-hook #'eglot-ensure)

(provide 'zig-conf)

;;; zig-conf.el ends here
