;;; jiraf.el --- Some Jira & Stash ad-hoc utils.

(defun jiraf-get-line ()
  (buffer-substring (progn (beginning-of-line)
                           (point))
                    (progn (end-of-line)
                           (point))))

(defun jiraf-get-repo-name ()
  "Copy project name from repository path to `kill-ring'.
https://stash.company.com/projects/path/repos/project-type.project-name/browse
-> project-type.project-name"
  (interactive)
  (save-excursion
    (let* ((current-line (jiraf-get-line))
           (repo-name (with-temp-buffer
                        (insert current-line)
                        (beginning-of-line)
                        (or
                         (condition-case nil
                             (progn
                               (search-forward "repos/")
                               (cua-set-mark)
                               (search-forward "/")
                               (left-char 1)
                               (buffer-substring (point) (mark)))
                           (error nil))
                         (jiraf-get-line)))))
      (message (format
                "Copied repo name: %s"
                (propertize (car kill-ring)
                            'face 'font-lock-constant-face)))
      (kill-new repo-name)
      repo-name)))

(defun jiraf-split-repo (repo-name)
  (let ((split-repo (split-string repo-name "\\.")))
    (format "%s %s"
     (string-join (butlast split-repo) ".")
     (car (last split-repo)))))

(defun jiraf-find-script (file-path split-repo)
  "Return the artifact installation script."
  (with-temp-buffer
    (insert-file-contents file-path)
    (condition-case nil
        (progn
          (search-forward split-repo)
          (save-excursion
            (buffer-substring (progn (beginning-of-line)
                                     (point))
                              (progn (end-of-line)
                                     (point)))))
      (error nil))))

(defun jiraf-get-script ()
  "Copy installation script from scripts list file to `kill-ring'."
  (interactive)
  (let* ((repo-name (jiraf-get-repo-name))
         (script
          (jiraf-find-script
           (file-truename "~/.emacs.d/scripts")
           (jiraf-split-repo repo-name))))
    (message (if script
                 (progn
                   (kill-new script)
                   (format
                    "Script: %s"
                    (propertize script
                                'face 'font-lock-constant-face)))
               (format
                "Can't find script for: %s"
                (propertize repo-name
                            'face 'font-lock-constant-face))))))

(provide 'jiraf)
