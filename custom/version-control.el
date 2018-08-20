(require 'magit)

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(custom-set-variables
 '(magit-save-some-buffers (quote dontask)))

(require 'ahg)
(require 'darcsum)

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

(provide 'version-control)
