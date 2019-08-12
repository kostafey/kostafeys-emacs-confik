;; Set of tips to increase responsibility speed for windows.

;; Disable bidirectional text support
(setq-default bidi-display-reordering nil)

(when (eq system-type 'windows-nt)
  ;; garbage collector
  (setq gc-cons-threshold (* 511 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 10 t #'garbage-collect)

  ;; autocompletion
  (setq ac-delay 0.5)

  (if (>= emacs-major-version 25)
      (remove-hook 'find-file-hooks 'vc-refresh-state)
    (remove-hook 'find-file-hooks 'vc-find-file-hook))

  ;;------------------------------------------------------------
  ;; magit

  ;; Tell Magit to only automatically refresh the current Magit buffer, but not
  ;; the status buffer. The status buffer is only refreshed automatically if it
  ;; itself is the current buffer.
  (setq magit-refresh-status-buffer nil)

  (defun magit-status-narrow ()
    ;; Hide sections by default
    (let ((hide (lambda (_section) 'hide)))
      (add-hook 'magit-section-set-visibility-hook hide)
      (magit-status)
      (remove-hook 'magit-section-set-visibility-hook hide)))

  (defvar magit-show-staged t)

  (defun magit-toggle-show-staged ()
    (interactive)
    (setq magit-show-staged (not magit-show-staged))
    (if (not magit-show-staged)
        (progn
          (remove-hook 'magit-status-sections-hook #'magit-insert-staged-changes)
          (message "Magit: hide staged section"))
      (progn
        (add-hook 'magit-status-sections-hook #'magit-insert-staged-changes)
        (message "Magit: show staged section"))))

  (defvar-local magit-git--git-dir-cache nil)
  (defvar-local magit-git--toplevel-cache nil)
  (defvar-local magit-git--cdup-cache nil)

  (defun memoize-rev-parse (fun &rest args)
    (pcase (car args)
      ("--git-dir"
       (unless magit-git--git-dir-cache
         (setq magit-git--git-dir-cache (apply fun args)))
       magit-git--git-dir-cache)
      ("--show-toplevel"
       (unless magit-git--toplevel-cache
         (setq magit-git--toplevel-cache (apply fun args)))
       magit-git--toplevel-cache)
      ("--show-cdup"
       (let ((cdup (assoc default-directory magit-git--cdup-cache)))
         (unless cdup
           (setq cdup (cons default-directory (apply fun args)))
           (push cdup magit-git--cdup-cache))
         (cdr cdup)))
      (_ (apply fun args))))

  (advice-add 'magit-rev-parse-safe :around #'memoize-rev-parse)

  (defvar-local magit-git--config-cache (make-hash-table :test 'equal))

  (defun memoize-git-config (fun &rest keys)
    (let ((val (gethash keys magit-git--config-cache :nil)))
      (when (eq val :nil)
        (setq val (puthash keys (apply fun keys) magit-git--config-cache)))
      val))

  (advice-add 'magit-get :around #'memoize-git-config)
  (advice-add 'magit-get-boolean :around #'memoize-git-config))

(provide 'perfomance-conf)

;;; perfomance-conf.el ends here
