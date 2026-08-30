;;; file-ops.el --- Some handy file & buffer operations.  -*- lexical-binding: t -*-

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
  "Copy current file path to the clipboard."
  (interactive)
  (let* ((value (if (eq system-type 'windows-nt)
                    (let ((uri (replace-regexp-in-string
                                "/" "\\\\" (buffer-file-name))))
                      (concat (upcase (substring uri 0 1))
                              (substring uri 1)))
                  (buffer-file-name)))
         (result (kill-new value)))
    (message "%s" value)
    result))

(defun copy-to-clipboard-buffer-file-name ()
  "Copy current file name to the clipboard."
  (interactive)
  (let* ((value (file-name-nondirectory (buffer-file-name)))
         (result (kill-new value)))
    (message "%s" value)
    result))

(defun file-ops--line-reference ()
  "Return the current line number, or \"START-END\" over an active region.

A region ending at the beginning of a line does not reach into that
line -- selecting three whole lines leaves point on the fourth -- so
that last line is dropped and the range reads 5-7 rather than 5-8.
A region inside a single line reads as that one line, not a range."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (end (if (and (> end beg)
                           (= end (save-excursion
                                    (goto-char end)
                                    (line-beginning-position))))
                      (1- end)
                    end))
             (first (line-number-at-pos beg))
             (last (line-number-at-pos end)))
        (if (= first last)
            (number-to-string first)
          (format "%d-%d" first last)))
    (number-to-string (line-number-at-pos))))

(defun copy-file-name-and-line ()
  "Copy the current buffer's file name and line number to the clipboard.

With an active region, copy the range of lines it spans instead, as
file.java:5-10."
  (interactive)
  (if (buffer-file-name)
      (let ((formatted-string (format "%s:%s"
                                      (file-name-nondirectory (buffer-file-name))
                                      (file-ops--line-reference))))
        (kill-new formatted-string)
        (message "%s" formatted-string))
    (message "This buffer is not visiting a file.")))

(defun copy-file-path-and-line ()
  "Copy the current buffer's file path and line number to the clipboard.

With an active region, copy the range of lines it spans instead, as
/path/to/file.java:5-10."
  (interactive)
  (if (buffer-file-name)
      (let ((formatted-string (format "%s:%s"
                                      (buffer-file-name)
                                      (file-ops--line-reference))))
        (kill-new formatted-string)
        (message "%s" formatted-string))
    (message "This buffer is not visiting a file.")))

(provide 'file-ops)

;;; file-ops.el ends here
