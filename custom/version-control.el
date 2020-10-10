(require 'magit)
(require 'multi-magit)
(require 'ahg)
(require 'darcsum)

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(custom-set-variables
 '(magit-save-some-buffers (quote dontask)))

(defun copy-to-clipboard-git-branch ()
  (interactive)
  "Copy current branch name to the clipboard."
  (let* ((branch (car (vc-git-branches)))
         (result (kill-new branch)))
    (message result)
    result))

(defun get-vc-status ()
  "Call status function according to the actual vc system of the active file."
  (interactive)
  (let ((vc-type (downcase
                  (format "%s"
                          (vc-backend
                           (buffer-file-name (current-buffer)))))))
    (cond ((equal vc-type "hg")  (ahg-status))
          ((equal vc-type "git") (magit-status))
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

(if (require 'git-gutter-fringe nil 'noerror)
    (progn
      (global-git-gutter-mode t)
      (fringe-helper-define 'git-gutter-fr:modified nil
        ".XX..XX."
        ".XXXXXX."
        "..XXXX.."
        "XXXXXXXX"
        "XXXXXXXX"
        "..XXXX.."
        ".XXXXXX."
        ".XX..XX."))
  (when (require 'git-gutter nil 'noerror)
    (global-git-gutter-mode +1)
    (git-gutter:linum-setup)
    (setq git-gutter:modified-sign "*")))

(defun my-enable-smerge-maybe ()
  (when (and buffer-file-name (vc-backend buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode +1)))))

(add-hook 'find-file-hook #'my-enable-smerge-maybe)
(add-hook 'after-revert-hook #'my-enable-smerge-maybe)

(provide 'version-control)
