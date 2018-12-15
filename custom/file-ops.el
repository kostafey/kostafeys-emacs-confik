;;; file-ops.el --- Some handy file & buffer operations.

;; Function that kills the current buffer and removes
;; the file it is connected to.
(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rename-file-of-buffer ()
  "Renames both current buffer and file it's visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" (buffer-name))
      (let ((new-name (read-from-minibuffer
                       "New name: "
                       (file-name-nondirectory (buffer-file-name)))))
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

(defun copy-to-clipboard-buffer-file-path ()
  (interactive)
  "Copy current file path to the clipboard."
  (let ((result (kill-new
                 (if (eq system-type 'windows-nt)
                     (let ((uri (replace-regexp-in-string
                                 "/" "\\\\" (buffer-file-name))))
                       (concat (upcase (substring uri 0 1))
                               (substring uri 1)))
                   (buffer-file-name)))))
    (message result)
    result))

(defun copy-to-clipboard-buffer-file-name ()
  (interactive)
  "Copy current file name to the clipboard."
  (let ((result (kill-new (file-name-nondirectory (buffer-file-name)))))
    (message result)
    result))

(provide 'file-ops)

;;; file-ops.el ends here
