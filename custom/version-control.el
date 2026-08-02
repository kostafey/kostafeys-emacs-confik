(require 'cl-lib)

(straight-use-package
 '(magit :type git :host github
         :repo "magit/magit" :branch "main"))
(straight-use-package
 '(multi-magit :type git :host github
               :repo "luismbo/multi-magit" :branch "master"))
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
(when (eq system-type 'windows-nt)
  ;; Drop the status sections that each cost extra git round-trips: the
  ;; ahead/behind counts against upstream and pushremote, plus the tags header
  ;; (`git describe').  Re-add any you miss.
  (with-eval-after-load 'magit-status
    (dolist (section '(magit-insert-unpushed-to-pushremote
                       magit-insert-unpushed-to-upstream-or-recent
                       magit-insert-unpulled-from-pushremote
                       magit-insert-unpulled-from-upstream))
      (remove-hook 'magit-status-sections-hook section))
    (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)))

;; `vc-backend' probes every backend in `vc-handled-backends' in order,
;; spawning a subprocess per backend — painfully slow on Windows.  We only
;; drive Git (darcs goes through darcsum, not vc), so probe nothing else.
;; Also speeds up every `find-file' and `my-enable-smerge-maybe' below.
(setq vc-handled-backends '(Git))

(defun get-vc-status ()
  "Open the VCS status buffer for the current buffer's repository.
Use darcsum for a darcs working tree and Magit for everything else.

Backend detection via `vc-backend' is deliberately avoided: the only
non-darcs case also opens Magit, so the probe was pure overhead (a
subprocess per handled backend, slow on Windows)."
  (interactive)
  (let ((darcs-root (and (fboundp 'darcsum-repository-root)
                         (ignore-errors (darcsum-repository-root)))))
    (if darcs-root
        (darcsum-whatsnew darcs-root)
      (magit-status))))

(use-package git-gutter
  :straight '(git-gutter
              :type git :host github
              :repo "emacsorphanage/git-gutter" :branch "master")
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
  (when (and buffer-file-name (vc-backend buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode +1)))))

(add-hook 'find-file-hook #'my-enable-smerge-maybe)
(add-hook 'after-revert-hook #'my-enable-smerge-maybe)

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

(provide 'version-control)
