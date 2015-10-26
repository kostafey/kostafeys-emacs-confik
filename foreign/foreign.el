;; External programs wrapper

(defvar foreign-bin-path (expand-file-name "foreign" "~/.emacs.d"))

(defun foreign-format-json-file (file-name)
  (shell-command
   (concat foreign-bin-path "jsonpp " file-name)))

(defun foreign-replace-file (file-name old-string new-string)
  (shell-command
   (concat foreign-bin-path "replace " file-name
           " " old-string " " new-string)))

(defun json-file-format ()
  "Pretty print json file."
  (interactive)
  (js-mode)
  (foreign-format-json-file (buffer-file-name))
  (revert-buffer t t))

(defun replace-file (old-string new-string)
  "Replace string in file."
  (interactive
   (list
    (read-from-minibuffer "Replace string: "
                          "" nil nil 'replace-file-old-history)
    (read-from-minibuffer (concat "Replace with:")
                          "" nil nil 'replace-file-new-history)))
  (foreign-replace-file (buffer-file-name) old-string new-string)
  (revert-buffer t t))

(provide 'foreign)
