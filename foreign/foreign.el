;; External programs wrapper

(defvar foreign-bin-path (expand-file-name "foreign" "~/.emacs.d"))

(defun foreign-format-json-file (file-name)
  (shell-command
   (concat foreign-bin-path "jsonpp " file-name)))

(defun json-file-format ()
  "Pretty print json file."
  (interactive)
  (js-mode)
  (foreign-format-json-file (buffer-file-name))
  (revert-buffer))

(provide 'foreign)
