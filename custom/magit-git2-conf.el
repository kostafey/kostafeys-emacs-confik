;;; magit-git2-conf.el --- Magit's git queries in-process. -*- lexical-binding: t -*-

;; foreign/magit-git2: a Zig dynamic module over libgit2 that answers
;; Magit's cheap, read-only git queries without starting git -- a git
;; process costs ~190 ms on Windows.  Build it with `zig build' in
;; foreign/magit-git2 (`-Dtarget=x86_64-windows-gnu' for the .dll); see
;; its README.md.  Unbuilt, it stays off and Magit runs git as always.
;;
;; On Windows the emulation answers (`on'), and the status sections
;; version-control.el drops there for their git round-trips come back.
;; Elsewhere it runs beside git (`shadow') and records every difference
;; in *magit-git2-mismatches* (M-x magit-git2-show-mismatches).

(add-to-list 'load-path (concat site-lisp-path "foreign/magit-git2/lisp/"))

;; Its commands load Magit, which brings magit-git2 in and sets it up.
(dolist (command '(magit-git2-mode magit-git2-show-mismatches
                   magit-git2-stats magit-git2-reload-module))
  (autoload command "magit" nil t))

(with-eval-after-load 'magit
  (require 'magit-git2)
  (cond
   ((not (magit-git2-core-available-p))
    (message "magit-git2: %s" magit-git2-core-error))
   ((eq system-type 'windows-nt)
    (magit-git2-set-system-config-from-git)
    (setq magit-git2-mode 'on)
    ;; After version-control.el's removal, registered earlier.
    (with-eval-after-load 'magit-status
      (magit-git2-restore-sections)))
   (t (setq magit-git2-mode 'shadow))))

(provide 'magit-git2-conf)
