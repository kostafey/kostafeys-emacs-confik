;;; -*- lexical-binding: t -*-
(require 'cl-lib)

(straight-use-package
 '(magit :type git :host github
         :repo "magit/magit" :branch "main"))

(straight-use-package
 `(multi-magit :type git :host nil
               :repo "https://github.com/luismbo/multi-magit.git"
               :branch "master"
               ;; Remotes named the GitHub way, not straight's: `origin' is
               ;; the fork, `upstream' the original.
               :remote "upstream"
               :fork (:repo ,(pcase system-type
                               ('windows-nt
                                "https://github.com/kostafey/multi-magit.git")
                               ('gnu/linux
                                "git@github.com:kostafey/multi-magit.git"))
                      :branch "develop"
                      :remote "origin")))
(straight-use-package
 '(darcsum :type git :host github
           :repo "emacsmirror/darcsum" :branch "master"))
(straight-use-package
 '(diffview :type git :host github
            :repo "mgalgs/diffview-mode" :branch "master"))

;; Disable auto-reverting of file-visiting buffers after Magit commands (a
;; notable cost with many buffers open).  A plain `setq' suffices: Magit defers
;; the mode's activation until after init specifically so this value is honored.
(setq magit-auto-revert-mode nil)

(custom-set-variables
 '(magit-save-some-buffers (quote dontask)))

(defun k/magit-status-mode ()
  (cl-loop for m in (list magit-diff-mode-map
                          magit-file-section-map
                          magit-hunk-section-map
                          magit-unstaged-section-map
                          magit-staged-section-map)
           do (define-key m (kbd "C-c") 'cua-copy-region)))

(add-hook 'magit-status-mode-hook #'k/magit-status-mode)

;;-----------------------------------------------------------------------------
;; Magit performance on Windows
;;
;; Without magit-git2's module, uncomment this to drop the status sections
;; that each cost extra git round-trips: the ahead/behind counts against
;; upstream and pushremote, plus the tags header (`git describe').
;;
;; (when (eq system-type 'windows-nt)
;;   (with-eval-after-load 'magit-status
;;     (dolist (section '(magit-insert-unpushed-to-pushremote
;;                        magit-insert-unpushed-to-upstream-or-recent
;;                        magit-insert-unpulled-from-pushremote
;;                        magit-insert-unpulled-from-upstream))
;;       (remove-hook 'magit-status-sections-hook section))
;;     (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)))

;; magit-git2 answers Magit's cheap, read-only git queries without starting
;; git -- a git process costs ~190 ms on Windows.  It comes with Magit and
;; downloads its module to magit-git2/ here, unless that holds a
;; magit-git2-core.so; see straight/repos/magit-git2/README.md.
;;
;; The emulation answers (`on').  Uncomment the `setq' below now and then to
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

;; `vc-backend' probes every backend in `vc-handled-backends' in order,
;; spawning a subprocess per backend — painfully slow on Windows.  We only
;; drive Git (darcs goes through darcsum, not vc), so probe nothing else.
(setq vc-handled-backends '(Git))

(defvar multi-magit-selected-repositories)
(declare-function magit-toplevel "magit-git" (&optional directory))

(defun k/subdirectories (entries directory)
  "Return the subdirectories of DIRECTORY among its ENTRIES.
ENTRIES are names as `file-name-all-completions' returns them, which
marks a directory with a trailing slash; `.git' is left out."
  (seq-keep (lambda (name)
              (and (string-suffix-p "/" name)
                   (not (member name '("./" "../" ".git/")))
                   (concat directory name)))
            entries))

(defun k/git-repositories-under (directory)
  "Return the git repositories below DIRECTORY, as sorted directory names.
Subdirectories count down to two levels, as a/ and b/c/, each by a
`.git' of its own -- a directory, or the file of a linked worktree or a
submodule.  No git process runs: one listing of DIRECTORY and of each
of its subdirectories, and a file check per second level directory.
Nil for a remote DIRECTORY, where each of these is a network round trip.

The listings and checks run without file name handlers: matching every
name against them took nine tenths of the time, and an archive entry
such as x.tar would be opened by `tramp-archive' to look for x.tar/.git."
  (when (and (not (file-remote-p directory))
             (file-accessible-directory-p directory))
    (let ((file-name-handler-alist nil)
          (directory (file-name-as-directory (expand-file-name directory)))
          repositories)
      (dolist (child (k/subdirectories (file-name-all-completions "" directory)
                                       directory))
        (let ((entries (file-name-all-completions "" child)))
          (when (or (member ".git/" entries) (member ".git" entries))
            (push child repositories))
          (dolist (grandchild (k/subdirectories entries child))
            (when (file-exists-p (concat grandchild ".git"))
              (push grandchild repositories)))))
      (sort repositories #'string<))))

(defun get-vc-status ()
  "Open the VCS status buffer for the current buffer's repository.
Use darcsum for a darcs working tree and `pijul-record-preview' for a
Pijul one (when `pijul-conf' is loaded).  A directory with git
repositories up to two levels below it, see `k/git-repositories-under',
gets `multi-magit-status' over them, and over the repository it belongs
to itself, if any.  Magit covers everything else.

Backend detection via `vc-backend' is deliberately avoided: the only
other case also opens Magit, so the probe was pure overhead (a
subprocess per handled backend, slow on Windows)."
  (interactive)
  (let* ((darcs-root (and (fboundp 'darcsum-repository-root)
                          (ignore-errors (darcsum-repository-root))))
         (pijul-root (and (fboundp 'pijul-repository-root)
                          (pijul-repository-root)))
         (nested (unless (or darcs-root pijul-root)
                   (k/git-repositories-under default-directory))))
    (cond (darcs-root (darcsum-whatsnew darcs-root))
          (pijul-root (pijul-record-preview))
          (nested
           (require 'multi-magit)
           (setq multi-magit-selected-repositories
                 (append (ensure-list (magit-toplevel)) nested))
           (multi-magit-status))
          (t (magit-status)))))

;; `git-gutter:in-git-repository-p' shells out to `git rev-parse
;; --is-inside-work-tree', and `global-git-gutter-mode' asks it twice for
;; every file opened: `after-change-major-mode-hook' runs once for the
;; `fundamental-mode' that `find-file' starts from, and once for the real
;; mode.  Answer from the file system instead -- which is what git-gutter
;; itself does for svn, hg and bzr, in `git-gutter:in-repository-common-p'.
(defun k/git-gutter-in-git-repository-p ()
  "Non-nil when `default-directory' sits inside a git working tree.
A `.git' file (a linked worktree, a submodule) counts; being inside the
`.git' directory itself does not.  Unlike `git rev-parse' this misses a
tree named by `GIT_DIR'/`GIT_WORK_TREE' alone -- a fair trade at some
190 ms per subprocess, twice per file, on MS Windows."
  (and (locate-dominating-file default-directory ".git")
       (not (string-match-p (regexp-quote "/.git/") default-directory))
       t))

(advice-add 'git-gutter:in-git-repository-p
            :override #'k/git-gutter-in-git-repository-p)

(use-package git-gutter
  :straight '(git-gutter
              :type git :host github
              :repo "emacsorphanage/git-gutter" :branch "master")
  ;; `:bind' alone would defer loading until the first key press, leaving
  ;; `global-git-gutter-mode' off at startup.
  :demand t
  :bind (("C-M-g <down>" . git-gutter:next-hunk)
         ("C-M-g <up>" . git-gutter:previous-hunk)
         ("C-M-g p" . git-gutter:popup-hunk)
         ("C-M-g r" . git-gutter:revert-hunk))
  :config
  (global-git-gutter-mode t))

(use-package git-gutter-fringe
  :straight '(git-gutter-fringe
              :type git :host github
              :repo "emacsorphanage/git-gutter-fringe" :branch "master")
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [#b11100000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b11100000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [#b11111111] nil nil '(center repeated)))

(defun my-enable-smerge-maybe ()
  "Turn on `smerge-mode' when the buffer holds conflict markers.
No `vc-backend' probe first: it spends a `git ls-files' -- and a second
`git ls-tree' for a file that is not in the index, which here is most of
them -- to answer a question the regexp below settles in microseconds.
On MS Windows, where spawning a process costs some 190 ms, that probe
was a third of the time it took to open a file."
  (when buffer-file-name
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode +1)))))

(add-hook 'find-file-hook #'my-enable-smerge-maybe)
(add-hook 'after-revert-hook #'my-enable-smerge-maybe)

;; The smerge commands go under C-c s instead of C-c ^; this has to be set
;; before smerge-mode is loaded.
(setq smerge-command-prefix (kbd "C-c s"))

(defun k/display-buffer-in-next-window (buffer _alist)
  "Display BUFFER in the window next to the selected one.
Split the frame when it holds a single window, so there always is a
neighboring window to reuse.  Meant to be used as a `display-buffer'
action function; returns the window BUFFER is shown in."
  (when (one-window-p 'nomini)
    (split-window-right))
  (let ((window (next-window (selected-window) 'nomini)))
    (set-window-buffer window buffer)
    window))

(defun k/magit-diff-visit-worktree-file-other-window ()
  "From a diff visit the worktree version of the file at point.
Like `magit-diff-visit-worktree-file' — always the \"real\" file of the
working tree, with point on the line corresponding to the position inside
the diff — but the file is shown in the neighboring window (the frame is
split when it holds a single window) and that window gets selected, so
the Magit buffer stays visible.  Cf. `hop-at-point-other-window'."
  (interactive)
  (let ((display-buffer-overriding-action
         (list #'k/display-buffer-in-next-window)))
    (magit-diff-visit-worktree-file-other-window)))

(defun k/diff (new)
  "Compare current buffer file with other one."
  (interactive (list (read-file-name "Compare with file: ")))
  (let ((old (buffer-file-name)))
    (diff old new)))

;;-----------------------------------------------------------------------------
;; diffview: scroll both sides at once
;;
(defun k/diffview-side-by-side (action)
  "Call ACTION in the selected window and then in the next one."
  (funcall action)
  (other-window 1)
  (funcall action)
  (other-window 1))

(defun k/diffview-page-down ()
  "Page down both sides of the diff."
  (interactive)
  (k/diffview-side-by-side #'pager-page-down))

(defun k/diffview-page-up ()
  "Page up both sides of the diff."
  (interactive)
  (k/diffview-side-by-side #'pager-page-up))

(defun k/diffview-scroll-down-line ()
  "Scroll both sides of the diff down by a line."
  (interactive)
  (k/diffview-side-by-side #'scroll-down-line))

(defun k/diffview-scroll-up-line ()
  "Scroll both sides of the diff up by a line."
  (interactive)
  (k/diffview-side-by-side #'scroll-up-line))

;;-----------------------------------------------------------------------------
;; Keys
;;
(global-set-key (kbd "M-w") 'get-vc-status)

(with-eval-after-load 'magit-mode
  (define-key magit-mode-map (kbd "C-w") 'kill-buffer)
  (define-key magit-mode-map (kbd "S-M-w") 'magit-copy-buffer-revision)
  (define-key magit-mode-map (kbd "M-w") 'diffview-current)
  (define-key magit-mode-map (kbd "C-s-<down>") 'magit-section-forward)
  (define-key magit-mode-map (kbd "C-s-<up>") 'magit-section-backward))

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "C-x d")
              'k/magit-diff-visit-worktree-file-other-window))

(with-eval-after-load 'diffview
  (define-key diffview-mode-map (kbd "<next>") 'k/diffview-page-down)
  (define-key diffview-mode-map (kbd "<prior>") 'k/diffview-page-up)
  (define-key diffview-mode-map (kbd "C-<up>") 'k/diffview-scroll-down-line)
  (define-key diffview-mode-map (kbd "C-<down>") 'k/diffview-scroll-up-line)
  (define-key diffview-mode-map (kbd "<mouse-4>") 'k/diffview-scroll-down-line)
  (define-key diffview-mode-map (kbd "<mouse-5>") 'k/diffview-scroll-up-line))

(provide 'version-control)
