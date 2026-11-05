;;; magit-git2-conf.el --- Magit's git queries in-process. -*- lexical-binding: t -*-

;; magit-git2 answers Magit's cheap, read-only git queries without
;; starting git -- a git process costs ~190 ms on Windows.  It comes with
;; Magit and downloads its module, unless `zig build' made one in
;; straight/repos/magit-git2; see the README.md there.
;;
;; On Windows the emulation answers (`on').  Elsewhere it runs beside git
;; (`shadow'), the early warning for updates of git and Magit, and records
;; every difference in *magit-git2-mismatches*
;; (M-x magit-git2-show-mismatches).

(straight-use-package
 `(magit-git2 :type git :host nil
              :repo ,(pcase system-type
                       ('windows-nt
                        "https://github.com/kostafey/magit-git2.git")
                       ('gnu/linux
                        "git@github.com:kostafey/magit-git2.git"))
              :branch "master"))

(unless (eq system-type 'windows-nt)
  (setq magit-git2-mode 'shadow))

(provide 'magit-git2-conf)
