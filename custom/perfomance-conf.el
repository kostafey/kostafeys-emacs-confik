;;; -*- lexical-binding: t -*-
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

  ;; The answers depend on `default-directory', which is not fixed for a
  ;; buffer: `multi-magit-status' moves it to the repository at point.  So
  ;; both caches are keyed by it -- a cache per buffer alone handed one
  ;; repository's toplevel to the files of another.
  (defvar-local magit-git--rev-parse-cache nil
    "Alist of ((DIRECTORY . ARGS) . OUTPUT) of memoized `git rev-parse'.")

  (defun memoize-rev-parse (fun &rest args)
    (if (not (member (car args) '("--git-dir" "--show-toplevel" "--show-cdup")))
        (apply fun args)
      (let* ((key (cons default-directory args))
             (hit (assoc key magit-git--rev-parse-cache)))
        (if hit
            (cdr hit)
          (let ((val (apply fun args)))
            ;; As before, outside a repository ask again next time: it
            ;; may have been created since.  The cdup was always kept.
            (when (or val (equal (car args) "--show-cdup"))
              (push (cons key val) magit-git--rev-parse-cache))
            val)))))

  (advice-add 'magit-rev-parse-safe :around #'memoize-rev-parse)

  ;; Shared by all buffers, as the `defvar-local' it replaces was in
  ;; effect: nothing ever set that one, so every buffer used its default.
  (defvar magit-git--config-cache (make-hash-table :test 'equal)
    "Memoized `magit-get' answers, keyed by (DIRECTORY . KEYS).")

  (defun memoize-git-config (fun &rest keys)
    (let* ((key (cons default-directory keys))
           (val (gethash key magit-git--config-cache :nil)))
      (when (eq val :nil)
        (setq val (puthash key (apply fun keys) magit-git--config-cache)))
      val))

  (advice-add 'magit-get :around #'memoize-git-config)
  (advice-add 'magit-get-boolean :around #'memoize-git-config))

;;------------------------------------------------------------
;; Lisp backtrace of a frozen Emacs, for ~/freeze-collect.sh

;; The script writes a file name into the request file and sends SIGUSR2.
;; `debug-on-event' turns the signal into a quit that calls `debug' with the
;; spinning code still on the stack, and this advice writes that stack out.
;; The *Backtrace* buffer alone is not enough: nobody can see it on a dead
;; display, and the debugger stays out altogether when it has already been
;; entered for the same input event.  Costs one `file-exists-p' per `debug'.
(defconst k/freeze-backtrace-request "~/.emacs-freeze-backtrace-request"
  "Exists while `freeze-collect.sh' waits for a Lisp backtrace.
Holds the name of the file to write it to.")

(defun k/freeze-dump-backtrace (&rest _)
  "Write the Lisp backtrace where `k/freeze-backtrace-request' asks."
  (let ((request (expand-file-name k/freeze-backtrace-request)))
    (when (file-exists-p request)
      (ignore-errors
        (let ((target (with-temp-buffer
                        (insert-file-contents request)
                        (buffer-string)))
              (trace (with-output-to-string (backtrace))))
          (delete-file request)
          (with-temp-file target (insert trace)))))))

(advice-add 'debug :before #'k/freeze-dump-backtrace)

(provide 'perfomance-conf)

;;; perfomance-conf.el ends here
