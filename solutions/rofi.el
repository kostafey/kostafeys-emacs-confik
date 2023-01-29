;;; rofi.el --- switch to other buffers and files via rofi. -*- lexical-binding: t -*-

(use-elpa 'popup-switcher)

(cl-defun rofi (&key
                prompt
                items-list
                item-name-getter
                switcher)
  (with-temp-buffer
    (thread-first
      (mapcar item-name-getter items-list)
      (string-join "\n")
      string-trim
      insert)
    (shell-command-on-region
     (point-min)
     (point-max)
     (pcase system-type
       ('gnu/linux (format
                    (concat "rofi -dmenu -sorting-method fzf -sort "
                            "-matching fuzzy -m -1 -i -p '%s'")
                    prompt))
       ('darwin "choose"))
     nil t "*rofi*" nil)
    (let ((result (string-trim (buffer-string))))
      (when (> (length result) 0)
        (funcall switcher result)))))

;;;###autoload
(defun rofi-switch-buffer (arg)
  (interactive "P")
  (rofi
   :prompt "Buffer"
   :items-list (psw-get-buffer-list arg)
   :item-name-getter (lambda (buffer)
                       (with-current-buffer buffer
                         (if (and psw-mark-modified-buffers
                                  (buffer-modified-p)
                                  (not (psw-is-temp-buffer)))
                             (concat (buffer-name) " *")
                           (buffer-name))))
   :switcher 'switch-to-buffer))

;;;###autoload
(defun rofi-switch-recentf ()
  (interactive)
  (rofi
   :prompt "Recent"
   :items-list recentf-list
   :item-name-getter 'identity
   :switcher 'find-file))

;;;###autoload
(defun rofi-switch-projectile-files ()
  (interactive)
  (rofi
   :prompt "Project files"
   :items-list (let ((current-projectile-mode projectile-mode)
                     (files (projectile-current-project-files)))
                 (setq projectile-mode current-projectile-mode)
                 files)
   :item-name-getter 'identity
   :switcher (lambda (file)
               (find-file
                (expand-file-name file
                                  (projectile-project-root))))))

(provide 'rofi)
