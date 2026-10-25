;;; codex-conf.el --- Run Codex CLI in Ghostel. -*- lexical-binding: t; -*-

(require 'project)
(require 'seq)

(defvar k/codex-cli "codex"
  "Codex CLI executable to run in Ghostel.")

(defvar-local k/codex-project-root nil
  "Project root associated with this Codex buffer.")

(defun k/codex--project-root (&optional buffer)
  "Return the project root for BUFFER, or its `default-directory'."
  (with-current-buffer (or buffer (current-buffer))
    (if-let ((project (project-current nil)))
        (project-root project)
      default-directory)))

(defun k/codex--buffer-name (root)
  "Return the Codex buffer name for project ROOT."
  (format "*codex[%s]*"
          (file-name-nondirectory (directory-file-name root))))

(defun k/codex--buffer-root ()
  "Return the project root of the current Codex buffer, or nil.
Falls back to the root stored in `ghostel-identity', which survives
the switch to `ghostel-mode'."
  (or k/codex-project-root
      (and (boundp 'ghostel-identity)
           (listp ghostel-identity)
           (alist-get 'project-root ghostel-identity))))

(defun k/codex--session-buffer (root)
  "Return the live Codex buffer associated with project ROOT, if any."
  (seq-some
   (lambda (buffer)
     (with-current-buffer buffer
       (let ((buffer-root (k/codex--buffer-root)))
         (and (stringp buffer-root)
              (or (equal buffer-root root)
                  (condition-case nil
                      (file-equal-p buffer-root root)
                    (file-error nil)))
              (bound-and-true-p ghostel--process)
              (process-live-p ghostel--process)
              buffer))))
   (buffer-list)))

(defun k/codex-insert-newline ()
  "Insert a newline in the current Codex prompt without submitting it."
  (interactive)
  (ghostel-paste-string "\n"))

(define-minor-mode k/codex-input-mode
  "Keep Codex input bindings separate from other Ghostel sessions."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "S-<return>") #'k/codex-insert-newline)
            (define-key map (kbd "S-RET") #'k/codex-insert-newline)
            map))

(defun k/codex ()
  "Open or switch to a Codex CLI session in the current project using Ghostel."
  (interactive)
  (require 'ghostel)
  (let* ((root (file-name-as-directory (expand-file-name (k/codex--project-root))))
         (name (k/codex--buffer-name root))
         (buffer (get-buffer name)))
    (if (and buffer
             (with-current-buffer buffer
               (and (equal (k/codex--buffer-root) root)
                    (bound-and-true-p ghostel--process)
                    (process-live-p ghostel--process))))
        (progn
          (with-current-buffer buffer
            (k/codex-input-mode 1))
          (pop-to-buffer buffer))
      (unless (executable-find k/codex-cli)
        (user-error "Codex CLI not found: %s (check PATH or k/codex-cli)"
                    k/codex-cli))
      (let* ((buffer (or buffer (get-buffer-create name)))
             (program (if (eq system-type 'windows-nt) "cmd.exe" k/codex-cli))
             (args (if (eq system-type 'windows-nt)
                       (list "/d" "/c" k/codex-cli)
                     nil))
             (identity `((kind . exec)
                         (project-root . ,root)
                         (command . (,program . ,args)))))
        (with-current-buffer buffer
          (setq-local default-directory root))
        (ghostel-exec buffer program args identity)
        ;; Set after `ghostel-exec': switching to `ghostel-mode' runs
        ;; `kill-all-local-variables'.
        (with-current-buffer buffer
          (setq-local k/codex-project-root root)
          (k/codex-input-mode 1))
        (pop-to-buffer buffer)))))

(defun k/codex-send-region (beg end)
  "Paste the active region into the Codex session for this file's project."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((root (file-name-as-directory
                (expand-file-name (k/codex--project-root (current-buffer)))))
         (buffer (k/codex--session-buffer root))
         (text (buffer-substring-no-properties beg end)))
    (unless buffer
      (user-error "No running Codex session for this project; run `k/codex' first"))
    (when (string-blank-p text)
      (user-error "Region is blank"))
    (deactivate-mark)
    (with-current-buffer buffer
      (ghostel-paste-string text)
      (sit-for 0.1)
      (ghostel-send-key "return"))))

(global-set-key (kbd "C-M-a J") #'k/codex)
(global-set-key (kbd "C-M-a C") #'k/codex-send-region)

(provide 'codex-conf)

;;; codex-conf.el ends here
