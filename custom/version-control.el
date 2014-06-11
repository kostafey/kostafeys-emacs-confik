(require 'magit)

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
          ((equal vc-type "git") (magit-status
                                  (if current-prefix-arg
                                      (magit-read-top-dir
                                       (> (prefix-numeric-value
                                           current-prefix-arg)
                                          4))
                                    (or (magit-get-top-dir)
                                        (magit-read-top-dir nil)))))
          (t (magit-status nil)))))

(provide 'version-control)
