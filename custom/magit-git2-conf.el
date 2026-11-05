;;; magit-git2-conf.el --- Magit's git queries in-process. -*- lexical-binding: t -*-

;; magit-git2 answers Magit's cheap, read-only git queries without
;; starting git -- a git process costs ~190 ms on Windows.  It comes with
;; Magit and downloads its module to magit-git2/ here, unless that holds a
;; magit-git2-core.so; see straight/repos/magit-git2/README.md.
;;
;; The emulation answers (`on').  Uncomment the line below now and then to
;; run it beside git (`verify'), the check after updates of git and Magit:
;; differences show as warnings and in *magit-git2-mismatches*
;; (M-x magit-git2-show-mismatches).

(straight-use-package
 `(magit-git2 :type git :host nil
              :repo ,(pcase system-type
                       ('windows-nt
                        "https://github.com/kostafey/magit-git2.git")
                       ('gnu/linux
                        "git@github.com:kostafey/magit-git2.git"))
              :branch "master"))

;; (setq magit-git2-mode 'verify)

(provide 'magit-git2-conf)
