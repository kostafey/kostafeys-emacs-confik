;;; tags-conf.el --- Navigation by tags, no LSP  -*- lexical-binding: t -*-

;; Definitions and references through GNU Global, for code where no
;; language server runs -- Java without eglot first of all.  `gtags-mode'
;; plugs Global into the stock tools: xref (`M-.', `M-?', `M-,') and so
;; `hop-at-point' too, in every file under a GTAGS database.  It adds no
;; keys of its own.
;;
;; Needs `global' and `gtags' in PATH:
;;   Void Linux: xbps-install -S global
;;   Windows:    scoop install global
;;               (or the MSYS2 package mingw-w64-ucrt-x86_64-global)
;;
;; The database is created once per project with `k/gtags-create'.  Files
;; saved in Emacs are reindexed on save; `gtags-mode-update' catches up
;; with changes made outside Emacs (git pull, branch switch).

(require 'project)

;;-------------------------------------------------------------------
;; gtags-mode
;;
(use-package gtags-mode
  :straight `(gtags-mode
              :type git :host nil
              :repo ,(pcase system-type
                       ('windows-nt
                        "https://github.com/Ergus/gtags-mode.git")
                       ('gnu/linux
                        "git@github.com:Ergus/gtags-mode.git"))
              :branch "master")
  :custom
  ;; Left out:
  ;; `project'    - the GTAGS root would replace the VC project, and its
  ;;                file list holds parsed sources only, no pom.xml;
  ;; `completion' - it would complete in every buffer under the root,
  ;;                prose and commit messages included, see below;
  ;; `imenu'      - the major mode's own index stays.
  (gtags-mode-features '(xref hooks))
  :config
  (defun k/gtags-completion-setup ()
    "Complete symbols from the Global database after the mode's own capfs."
    (add-hook 'completion-at-point-functions
              #'gtags-mode-completion-function 90 t))
  (add-hook 'prog-mode-hook #'k/gtags-completion-setup)
  (gtags-mode 1))

(defun k/gtags-create ()
  "Create the GNU Global database at the root of the current project."
  (interactive)
  (gtags-mode-create (expand-file-name (project-root (project-current t)))))

(provide 'tags-conf)
