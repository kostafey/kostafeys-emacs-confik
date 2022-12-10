(require 'elpa-conf)
(use-elpa 'magit)
(use-elpa 'multi-magit)
(use-elpa 'darcsum)
(use-elpa 'git-gutter)
(use-elpa 'git-gutter-fringe)
(use-elpa 'diffview)

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(custom-set-variables
 '(magit-save-some-buffers (quote dontask)))

(defun get-vc-status ()
  "Call status function according to the actual vc system of the active file."
  (interactive)
  (let ((vc-type (downcase
                  (format "%s"
                          (vc-backend
                           (buffer-file-name (current-buffer)))))))
    (cond ((equal vc-type "git") (magit-status))
          ((not (equal
                 (darcsum-repository-root) nil)) (darcsum-whatsnew
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

(when (require 'git-gutter-fringe nil 'noerror)
  (global-git-gutter-mode t))

(defun my-enable-smerge-maybe ()
  (when (and buffer-file-name (vc-backend buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode +1)))))

(add-hook 'find-file-hook #'my-enable-smerge-maybe)
(add-hook 'after-revert-hook #'my-enable-smerge-maybe)

(provide 'version-control)
