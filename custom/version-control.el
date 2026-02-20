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

(defun get-vc-status ()
  "Call status function according to the actual vc system of the active file."
  (interactive)
  (let ((vc-type (downcase
                  (format "%s"
                          (vc-backend
                           (buffer-file-name (current-buffer)))))))
    (cond ((equal vc-type "git")
           (magit-status))
          ((and (boundp 'darcsum-repository-root)
                (not (equal (darcsum-repository-root) nil)))
           (darcsum-whatsnew
            (darcsum-repository-root)))
          (t (magit-status)))))

(defvar k/use-gitall t)

(defun k/multy-magit-status ()
  "Set directories list to check their git status."
  (interactive)
  (let* ((current-project (magit-toplevel))
         (root (concat-path current-project ".."))
         (dirs (-map
                (lambda (d) (concat-path root d))
                (-filter
                 (lambda (d) (and (not (s-starts-with? "." d))
                             (file-directory-p
                              (concat-path root d))
                             (magit-git-repo-p (concat-path root d))))
                 (if k/use-gitall
                     (s-split
                      "\n"
                      (->
                       (s-split "Total affected:"
                                (let ((default-directory root))
                                  (shell-command-to-string "gitall status")))
                       cadr
                       s-trim))
                   (directory-files root))))))
    (setq multi-magit-selected-repositories dirs))
  (multi-magit-status))

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

(defun k/diff (new)
  "Compare current buffer file with other one."
  (interactive (list (read-file-name "Compare with file: ")))
  (let ((old (buffer-file-name)))
    (diff old new)))

(provide 'version-control)
