(require 'magit)

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(custom-set-variables
 '(magit-save-some-buffers (quote dontask)))

(require 'ahg)

(defun get-vc-status ()
  "Call status function according to the actual vc system of the active file."
  (interactive)
  (let ((vc-type (downcase
                  (format "%s"
                          (vc-backend
                           (buffer-file-name (current-buffer)))))))
    (cond ((equal vc-type "hg")  (ahg-status))
          ((equal vc-type "git") (magit-status))
          (t (magit-status)))))

(when (require 'git-gutter nil 'noerror)
  (global-git-gutter-mode +1)
  (git-gutter:linum-setup)
  (setq git-gutter:modified-sign "*"))

(provide 'version-control)
